package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._

import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import com.lightning.wallet.ln.Tools.{runAnd, none}
import fr.acinq.bitcoin.{Satoshi, Transaction}
import fr.acinq.bitcoin.Crypto.PrivateKey
import scala.collection.mutable


abstract class Channel extends StateMachine[ChannelData] { me =>
  override def process(some: Any) = try super.process(some) catch events.onError
  val listeners = mutable.Set.empty[ChannelListener]

  private[this] val events = new ChannelListener {
    override def onError = { case error => for (lst <- listeners if lst.onError isDefinedAt error) lst onError error }
    override def onBecome = { case trans => for (lst <- listeners if lst.onBecome isDefinedAt trans) lst onBecome trans }
  }

  def SEND(msg: LightningMessage): Unit
  def SAVE(content: HasCommitments): HasCommitments
  def STAY(d1: ChannelData): Channel = BECOME(d1, state)
  def BECOME(data1: ChannelData, state1: String): Channel = {
    // Transition should be defined before the vars are updated
    // Returns a self-reference for next channel operations
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
    me
  }

  def doProcess(change: Any): Unit = (data, change, state) match {
    case (InitData(announce), cmd @ CMDOpenChannel(localParams, tempChannelId,
      initialFeeratePerKw, pushMsat, _, fundingAmountSat), WAIT_FOR_INIT) =>

      BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(localParams.chainHash, tempChannelId, fundingAmountSat, pushMsat,
        localParams.dustLimitSatoshis, localParams.maxHtlcValueInFlightMsat, localParams.channelReserveSat, localParams.htlcMinimumMsat,
        initialFeeratePerKw, localParams.toSelfDelay, localParams.maxAcceptedHtlcs, localParams.fundingPrivKey.publicKey,
        localParams.revocationSecret.toPoint, localParams.paymentKey.toPoint, localParams.delayedPaymentKey.toPoint,
        Generators.perCommitPoint(localParams.shaSeed, index = 0), channelFlags = 0x00)


    case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
      if accept.temporaryChannelId == cmd.temporaryChannelId =>

      // If remote requires us to keep too much in local reserve we should close
      // otherwise we should wait for user to create a funding transaction in wallet UI
      val exceedsReserve = LNParams.exceedsReserve(accept.channelReserveSatoshis, cmd.fundingAmountSat)
      if (exceedsReserve) BECOME(wait, CLOSING) else BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


    // They have accepted our proposal, now let them sign a first commit tx
    case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
      val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat, accept.channelReserveSatoshis,
        accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs, accept.fundingPubkey, accept.revocationBasepoint,
        accept.paymentBasepoint, accept.delayedPaymentBasepoint, cmd.remoteInit.globalFeatures, cmd.remoteInit.localFeatures)

      val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
        Funding.makeFirstFunderCommitTxs(cmd, remoteParams, fundTx.hash,
          outIndex, accept.firstPerCommitmentPoint)

      val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
      val firstRemoteCommit = RemoteCommit(index = 0, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
      BECOME(WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, outIndex), remoteParams, fundTx,
        localSpec, localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND FundingCreated(cmd.temporaryChannelId,
        fundTx.hash, outIndex, localSigOfRemoteTx)


    // They have signed our first commit tx, we can broadcast a funding tx
    case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED)
      if remote.channelId == wait.channelId =>

      val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
        wait.remoteParams.fundingPubKey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

      if (Scripts.checkSpendable(signedLocalCommitTx).isFailure) BECOME(wait, CLOSING) else {
        val localCommit = LocalCommit(index = 0, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
        val localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
        val remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty)
        val dummy = PrivateKey(data = Tools.random getBytes 32, compressed = true).toPoint

        val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit,
          remoteCommit = wait.remoteCommit, localChanges, remoteChanges, localNextHtlcId = 0,
          remoteNextHtlcId = 0, remoteNextCommitInfo = Right(dummy), wait.localCommitTx.input,
          remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), wait.channelId)

        BECOME(me SAVE WaitFundingDoneData(wait.announce, our = None,
          their = None, wait.fundingTx, commitments), WAIT_FUNDING_DONE)
      }


    // FUNDING TX IS BROADCASTED AT THIS POINT


    // We have not yet sent a FundingLocked but just got one from them so we save it and keep waiting
    case (wait @ WaitFundingDoneData(_, None, _, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
      if their.channelId == wait.commitments.channelId =>
      me STAY wait.copy(their = Some apply their)


    // We have already sent them a FundingLocked and now we got one from them so we can enter normal state now
    case (wait @ WaitFundingDoneData(_, Some(our), _, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
      if their.channelId == wait.commitments.channelId =>

      val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
      BECOME(me SAVE NormalData(wait.announce, c1, localShutdown = None, remoteShutdown = None), NORMAL)


    // We got our lock but their is not yet present so we save ours and just keep waiting for their
    case (wait @ WaitFundingDoneData(_, _, None, _, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
      if wait.fundingTx.txid == tx.txid =>

      val our = makeFundingLocked(wait.commitments)
      val d1 = me SAVE wait.copy(our = Some apply our)
      me STAY d1 SEND our


    // We got our lock when their is already present so we can safely enter normal state now
    case (wait @ WaitFundingDoneData(_, _, Some(their), _, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
      if wait.fundingTx.txid == tx.txid =>

      val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
      BECOME(me SAVE NormalData(wait.announce, c1, None, None), NORMAL) SEND makeFundingLocked(wait.commitments)


    // NORMAL MODE


    // We only can add new HTLCs when mutual shutdown process is not active
    case (norm @ NormalData(_, commitments, None, None, _), cmd: CMDAddHtlc, NORMAL) =>
      val c1 ~ updateAddHtlc = Commitments.sendAdd(commitments, cmd)
      me STAY norm.copy(commitments = c1) SEND updateAddHtlc
      doProcess(CMDProceed)


    case (norm: NormalData, add: UpdateAddHtlc, NORMAL)
      if add.channelId == norm.commitments.channelId =>

      val incomingExpiry: Int = LNParams.broadcaster.currentHeight + 6
      val c1 = Commitments.receiveAdd(norm.commitments, add, incomingExpiry)
      me STAY norm.copy(commitments = c1)


    // We're fulfilling an HTLC we got earlier
    case (norm: NormalData, cmd: CMDFulfillHtlc, NORMAL) =>
      val c1 ~ updateFulfillHtlc = Commitments.sendFulfill(norm.commitments, cmd)
      me STAY norm.copy(commitments = c1) SEND updateFulfillHtlc
      doProcess(CMDProceed)


    case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
      if fulfill.channelId == norm.commitments.channelId =>

      // Got a fulfill for an outgoing HTLC we sent them earlier
      val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
      me STAY norm.copy(commitments = c1)


    // Failing an HTLC we got earlier
    case (norm: NormalData, cmd: CMDFailHtlc, NORMAL) =>
      val c1 ~ updateFailHtlc = Commitments.sendFail(norm.commitments, cmd)
      me STAY norm.copy(commitments = c1) SEND updateFailHtlc
      doProcess(CMDProceed)


    case (norm: NormalData, cmd: CMDFailMalformedHtlc, NORMAL) =>
      val c1 ~ updateFailMalformedHtlс = Commitments.sendFailMalformed(norm.commitments, cmd)
      me STAY norm.copy(commitments = c1) SEND updateFailMalformedHtlс
      doProcess(CMDProceed)


    case (norm: NormalData, fail: FailHtlc, NORMAL)
      if fail.channelId == norm.commitments.channelId =>

      // Got a failure for an outgoing HTLC we sent earlier
      val c1 = Commitments.receiveFail(norm.commitments, fail)
      me STAY norm.copy(commitments = c1)


    // GUARD: only send in the correct state
    case (norm: NormalData, CMDProceed, NORMAL)
      if Commitments canSendCommitSig norm.commitments =>

      // Propose new remote commit via commit tx sig
      val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
      val c1 ~ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
      val d1 = me SAVE norm.copy(commitments = c1)
      me STAY d1 SEND commitSig


    // Serves as a trigger to enter negotiations when shutdown is on
    case (norm @ NormalData(_, commitments, Some(local), Some(remote), _), CMDProceed, NORMAL) =>
      if (Commitments hasNoPendingHtlcs commitments) STARTNegotiations(norm.announce, commitments, local, remote)


    case (norm: NormalData, sig: CommitSig, NORMAL)
      if sig.channelId == norm.commitments.channelId =>

      // We received a commit sig from them, now we can update our local commit
      val c1 ~ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
      val d1 = me SAVE norm.copy(commitments = c1)
      me STAY d1 SEND revokeAndAck
      doProcess(CMDProceed)


    case (norm: NormalData, rev: RevokeAndAck, NORMAL)
      if rev.channelId == norm.commitments.channelId =>

      // We received a revocation because we sent a commit sig
      val c1 = Commitments.receiveRevocation(norm.commitments, rev)
      me STAY norm.copy(commitments = c1)
      doProcess(CMDProceed)


    case (norm: NormalData, CMDFeerate(rate), NORMAL)
      // GUARD: only send fee updates if the fee gap between nodes is large enough
      if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>

      // Periodic fee updates to ensure commit txs could be confirmed
      val c1 ~ updateFeeMessage = Commitments.sendFee(norm.commitments, rate)
      me STAY norm.copy(commitments = c1) SEND updateFeeMessage
      doProcess(CMDProceed)


    // NORMAL: SHUTDOWN


    // Sending CMDShutdown when mutual shutdown is already in progress means we want uncooperative close
    case (norm: NormalData, CMDShutdown, NORMAL) if norm.localShutdown.isDefined => STARTLocalCurrentClose(norm)
    case (norm: NormalData, CMDShutdown, NORMAL) if norm.remoteShutdown.isDefined => STARTLocalCurrentClose(norm)
    case (norm @ NormalData(_, _, None, None, _), CMDShutdown, NORMAL) => STARTShutdown(norm)


    // They try to shutdown with uncommited changes
    case (norm: NormalData, remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId &&
        Commitments.remoteHasChanges(norm.commitments) =>

      // Can't start mutual shutdown
      STARTLocalCurrentClose(norm)


    // We have not yet sent or received a shutdown so send one and re-call again
    case (norm @ NormalData(_, commitments, None, None, _), remote: Shutdown, NORMAL)
      if remote.channelId == commitments.channelId =>

      STARTShutdown(norm)
      doProcess(remote)


    // We have already sent a shutdown (initially or in response to their shutdown above)
    case (norm @ NormalData(announce, commitments, Some(local), None, _), remote: Shutdown, NORMAL)
      if remote.channelId == commitments.channelId =>

      // We can start negotiations if there are no in-flight HTLCs, otherwise wait until they are cleared
      if (Commitments hasNoPendingHtlcs commitments) STARTNegotiations(announce, commitments, local, remote)
      else me STAY norm.copy(remoteShutdown = Some apply remote)


    // HTLC TIMEOUT WATCH


    case (norm: NormalData, CMDHeight(chainHeight), NORMAL | SYNC)
      if Commitments.hasTimedoutOutgoingHtlcs(norm.commitments, chainHeight) =>
      STARTLocalCurrentClose(norm)


    // SYNC MODE


    // We may get this message any time so just save it here
    case (wait: WaitFundingDoneData, CMDConfirmed(tx), SYNC)
      if wait.fundingTx.txid == tx.txid =>

      val our = makeFundingLocked(wait.commitments)
      me STAY wait.copy(our = Some apply our)


    // This is a special case where we already have both locks when exiting a sync phase so we go directly to normal state
    case (wait @ WaitFundingDoneData(_, Some(our), Some(their), _, commitments, _), ChannelReestablish(channelId, 1, 0), SYNC)
      if channelId == commitments.channelId =>

      val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
      BECOME(me SAVE NormalData(wait.announce, c1, None, None), NORMAL) SEND our


    // We're exiting a sync state but don't have enough locks so we keep waiting
    case (wait: WaitFundingDoneData, ChannelReestablish(channelId, 1, 0), SYNC)
      if channelId == wait.commitments.channelId =>

      wait.our foreach SEND
      BECOME(wait, WAIT_FUNDING_DONE)


    case (neg: NegotiationsData, cr: ChannelReestablish, SYNC)
      if cr.channelId == neg.commitments.channelId =>

      // Should re-send last closingSigned according to spec
      BECOME(neg, NEGOTIATIONS) SEND neg.localClosingSigned


    case (norm: NormalData, cr: ChannelReestablish, SYNC)
      if cr.channelId == norm.commitments.channelId &&
        norm.commitments.remoteChanges.acked.isEmpty &&
        norm.commitments.remoteChanges.signed.isEmpty &&
        norm.commitments.localCommit.index == 0 =>

      // They may not have received our FundingLocked so re-send
      BECOME(norm, NORMAL) SEND makeFundingLocked(norm.commitments)


    case (norm: NormalData, cr: ChannelReestablish, SYNC)
      if cr.channelId == norm.commitments.channelId =>

      // First we clean up unacknowledged updates
      val localProposedIdDelta = norm.commitments.localChanges.proposed count { case u: UpdateAddHtlc => true }
      val remoteProposedIdDelta = norm.commitments.remoteChanges.proposed count { case u: UpdateAddHtlc => true }
      val c1 = norm.commitments.modifyAll(_.localChanges.proposed, _.remoteChanges.proposed).setTo(Vector.empty)
        .modify(_.remoteNextHtlcId).using(currentRemoteCount => currentRemoteCount - remoteProposedIdDelta)
        .modify(_.localNextHtlcId).using(currentLocalCount => currentLocalCount - localProposedIdDelta)

      // Let's see the state of remote sigs
      if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
        // Our last revocation got lost, let's resend it, followed by commit sig later
        val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
        val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
        me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
      } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw new LightningException

      // Let's see the state of local sigs
      val c2 = c1.remoteNextCommitInfo match {
        // We had sent a new sig and were waiting for their revocation, they didn't receive the new sig because of the disconnection
        // for now we simply discard the changes we had just signed, we also "un-sign" their changes that we already acked
        case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>

          val localSignedDelta = c1.localChanges.signed count { case u: UpdateAddHtlc => true }
          c1.copy(localNextHtlcId = c1.localNextHtlcId - localSignedDelta, localChanges = c1.localChanges.copy(signed = Vector.empty),
            remoteChanges = c1.remoteChanges.copy(acked = c1.remoteChanges.signed ++ c1.remoteChanges.acked, signed = Vector.empty),
            remoteNextCommitInfo = Right apply wait.nextRemoteCommit.remotePerCommitmentPoint)

        // We had sent a new sig and were waiting for their revocation, they had received the new sig
        // but their revocation was lost during the disconnection, they'll resend us the revocation
        case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => c1
        // There wasn't any sig in-flight when the disconnection occured so we do nothing here
        case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => c1
        case _ => throw new LightningException
      }

      BECOME(norm.copy(commitments = c2), NORMAL)
      norm.localShutdown foreach SEND
      doProcess(CMDProceed)


    // We just close a channel in any kind of irregular state
    case (some: HasCommitments, cr: ChannelReestablish, SYNC)
      if cr.channelId == some.commitments.channelId =>
      STARTLocalCurrentClose(some)


    // Don't accept these while in sync
    case (_, cmd: MemoCommand, SYNC) =>
      throw SyncException(cmd)


    // SYNC: CONNECT/DISCONNECT


    case (some: HasCommitments, CMDOnline, SYNC) =>
      me SEND ChannelReestablish(channelId = some.commitments.channelId,
        nextLocalCommitmentNumber = some.commitments.localCommit.index + 1,
        nextRemoteRevocationNumber = some.commitments.remoteCommit.index)


    case (wait: WaitFundingDoneData, CMDOffline, WAIT_FUNDING_DONE) => BECOME(wait, SYNC)
    case (negs: NegotiationsData, CMDOffline, NEGOTIATIONS) => BECOME(negs, SYNC)
    case (norm: NormalData, CMDOffline, NORMAL) => BECOME(norm, SYNC)


    // NEGOTIATIONS MODE


    case (neg: NegotiationsData, ClosingSigned(channelId, feeSatoshis, remoteSig), NEGOTIATIONS)
      if channelId == neg.commitments.channelId =>

      val Seq(closeFeeSat, remoteFeeSat) = Seq(neg.localClosingSigned.feeSatoshis, feeSatoshis) map Satoshi
      val Seq(localScript, remoteScript) = Seq(neg.localShutdown.scriptPubKey, neg.remoteShutdown.scriptPubKey)
      val closeTxOpt = Closing.checkClosingSignature(neg.commitments, localScript, remoteScript, remoteFeeSat, remoteSig)

      lazy val nextCloseFee = Closing.nextClosingFee(closeFeeSat, remoteFeeSat)
      lazy val (_, nextMessage: ClosingSigned) = Closing.makeClosing(neg.commitments,
        localScript, remoteScript, nextCloseFee)

      closeTxOpt match {
        case Some(closeTx) if closeFeeSat == remoteFeeSat => STARTMutualClose(neg, closeTx)
        case Some(closeTx) if nextCloseFee == remoteFeeSat => STARTMutualClose(neg, closeTx)
        case Some(_) => me STAY neg.copy(localClosingSigned = nextMessage) SEND nextMessage
        case _ => throw new LightningException
      }


    // HANDLE FUNDING SPENT


    case (neg: NegotiationsData, CMDSpent(spendTx), NEGOTIATIONS)
      if spendTx.txIn.exists(_.outPoint == neg.commitments.commitInput.outPoint) =>

      val (closeTx, _) = Closing.makeClosing(neg.commitments, neg.localShutdown.scriptPubKey,
        neg.remoteShutdown.scriptPubKey, closingFee = Satoshi apply neg.localClosingSigned.feeSatoshis)

      // Happens when we agreed on a closeSig, but we don't know it yet
      // we receive a tx notification before their ClosingSigned arrives
      if (closeTx.tx.txid == spendTx.txid) STARTMutualClose(neg, closeTx.tx)
      else DEFINEClosingAction(neg, spendTx)


    case (norm: HasCommitments, CMDSpent(spendTx), _)
      // GUARD: something which spends our funding is broadcasted, must react
      if spendTx.txIn.exists(_.outPoint == norm.commitments.commitInput.outPoint) =>
      DEFINEClosingAction(norm, spendTx)


    // HANDLE INITIALIZATION


    case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
    case (null, closing: ClosingData, null) => BECOME(closing, CLOSING)
    case (null, wait: WaitFundingDoneData, null) => BECOME(wait, SYNC)
    case (null, negs: NegotiationsData, null) => BECOME(negs, SYNC)
    case (null, norm: NormalData, null) => BECOME(norm, SYNC)


    // HANDLE SHUTDOWN


    // In all states except NORMAL and CLOSING this will immediately lead to an uncooperative close
    case (some, CMDShutdown, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) => BECOME(some, CLOSING)
    case (some: HasCommitments, CMDShutdown, WAIT_FUNDING_DONE | NEGOTIATIONS | SYNC) => STARTLocalCurrentClose(some)


    case _ =>
      // Let know if received an unhandled message
      Tools log s"Channel: unhandled $state : $change"
  }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, index = 1)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = point)
  }

  private def STARTShutdown(norm: NormalData) = {
    val localShutdown = Shutdown(norm.commitments.channelId, norm.commitments.localParams.defaultFinalScriptPubKey)
    BECOME(me SAVE norm.copy(localShutdown = Some apply localShutdown), NORMAL) SEND localShutdown
  }

  private def STARTNegotiations(announce: NodeAnnouncement, cs: Commitments, local: Shutdown, remote: Shutdown) = {
    val firstSigned = Closing.makeFirstClosing(cs, local.scriptPubKey, remote.scriptPubKey, cs.localCommit.spec.feeratePerKw)
    BECOME(me SAVE NegotiationsData(announce, cs, firstSigned, local, remote), NEGOTIATIONS) SEND firstSigned
  }

  private def STARTMutualClose(neg: NegotiationsData, closeTx: Transaction) =
    BECOME(me SAVE ClosingData(neg.announce, neg.commitments, closeTx :: Nil), CLOSING)

  private def STARTLocalCurrentClose(some: HasCommitments) =
    // Something went wrong and we decided to spend our current commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, some.commitments.localCommit.commitTx.tx, LNParams.bag) -> some match {
      case (claim: LocalCommitPublished, closing: ClosingData) => BECOME(data1 = me SAVE closing.copy(localCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(data1 = me SAVE ClosingData(some.announce, some.commitments, localCommit = claim :: Nil), CLOSING)
    }

  private def DEFINEClosingAction(some: HasCommitments, tx: Transaction) =
    // We are not sure what kind of closing transaction this so finding out
    some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit) match {
      case Left(remoteCommit) if remoteCommit.txid == tx.txid => STARTRemoteNextClose(some, tx, remoteCommit)
      case _ if some.commitments.remoteCommit.txid == tx.txid => STARTRemoteCurrentClose(some, tx)
      case _ => STARTOtherClose(some, tx)
    }

  private def STARTRemoteCurrentClose(some: HasCommitments, commitTx: Transaction) =
    // Something went wrong on their side and they decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, commitTx, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me SAVE closing.copy(remoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me SAVE ClosingData(some.announce, some.commitments, remoteCommit = claim :: Nil), CLOSING)
    }

  private def STARTRemoteNextClose(some: HasCommitments, commitTx: Transaction, nextRemoteCommit: RemoteCommit) =
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, commitTx, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me SAVE closing.copy(nextRemoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me SAVE ClosingData(some.announce, some.commitments, nextRemoteCommit = claim :: Nil), CLOSING)
    }

  private def STARTOtherClose(some: HasCommitments, commitTx: Transaction) =
    // This is a contract breach, they have spent a revoked transaction so we can take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, commitTx) -> some match {
      case (Some(claim), closing: ClosingData) => BECOME(me SAVE closing.modify(_.revokedCommits).using(claim +: _), CLOSING)
      case (Some(claim), _) => BECOME(me SAVE ClosingData(some.announce, some.commitments, revokedCommits = claim :: Nil), CLOSING)
      case _ => STARTLocalCurrentClose(some)
    }
}

object Channel {
  // Ephemeral (chan not saved)
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FOR_FUNDING = "WaitForFunding"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"

  // These states are saved and need sync
  val WAIT_FUNDING_DONE = "WaitFundingDone"
  val NEGOTIATIONS = "Negotiations"
  val NORMAL = "Normal"
  val SYNC = "Sync"

  // Makes chan inactive
  val CLOSING = "Closing"
}

trait ChannelListener {
  type Transition = (Channel, Any, String, String)
  def onError: PartialFunction[Throwable, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none
}