package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._

import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import com.lightning.wallet.ln.Tools.{none, runAnd}
import fr.acinq.bitcoin.Crypto.PrivateKey
import scala.collection.mutable


abstract class Channel extends StateMachine[ChannelData] { me =>
  // When in sync state we may receive commands from user, save them
  private[this] var memoCmdBuffer = Vector.empty[MemoCommand].toIterator
  val listeners = mutable.Set.empty[ChannelListener]

  private[this] val events = new ChannelListener {
    override def onError = { case error => for (lst <- listeners if lst.onError isDefinedAt error) lst onError error }
    override def onBecome = { case trans => for (lst <- listeners if lst.onBecome isDefinedAt trans) lst onBecome trans }
  }

  def send(message: LightningMessage): Unit
  def notifyListeners = events onBecome Tuple4(me, data, null, state)
  def stayAndSend(data: ChannelData, message: LightningMessage) =
    runAnd(me stayWith data)(me send message)

  override def process(change: Any) =
    try super.process(change)
    catch events.onError

  override def become(data1: ChannelData, state1: String) = {
    // Transition should be defined before the vars are updated
    val t4 = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome t4
  }

  def doProcess(change: Any): Unit = (data, change, state) match {
    case (InitData(announce), cmd @ CMDOpenChannel(localParams, tempChannelId,
      initialFeeratePerKw, pushMsat, _, fundingAmountSat), WAIT_FOR_INIT) =>

      become(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT)
      me send OpenChannel(localParams.chainHash, tempChannelId, fundingAmountSat, pushMsat,
        localParams.dustLimitSatoshis, localParams.maxHtlcValueInFlightMsat, localParams.channelReserveSat,
        localParams.htlcMinimumMsat, initialFeeratePerKw, localParams.toSelfDelay, localParams.maxAcceptedHtlcs,
        localParams.fundingPrivKey.publicKey, localParams.revocationSecret.toPoint, localParams.paymentKey.toPoint,
        localParams.delayedPaymentKey.toPoint, Generators.perCommitPoint(localParams.shaSeed, index = 0), 0x00)


    case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
      if accept.temporaryChannelId == cmd.temporaryChannelId =>

      // If remote requires us to keep too much in local reserve we should close
      // otherwise we should wait for user to create a funding transaction in wallet UI for example
      val exceedsReserve = LNParams.exceedsReserve(accept.channelReserveSatoshis, cmd.fundingAmountSat)
      if (exceedsReserve) become(wait, CLOSING) else become(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


    // They have accepted our proposal, now let them sign a first commit tx
    case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
      val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat, accept.channelReserveSatoshis,
        accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs, accept.fundingPubkey, accept.revocationBasepoint,
        accept.paymentBasepoint, accept.delayedPaymentBasepoint, cmd.remoteInit.globalFeatures, cmd.remoteInit.localFeatures)

      val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
        Funding.makeFirstFunderCommitTxs(cmd, remoteParams, fundTx.hash,
          outIndex, accept.firstPerCommitmentPoint)

      val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
      val fundingCreated = FundingCreated(cmd.temporaryChannelId, fundTx.hash, outIndex, localSigOfRemoteTx)
      val firstRemoteCommit = RemoteCommit(index = 0, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)
      val d1 = WaitFundingSignedData(announce, cmd.localParams, Tools.toLongId(fundTx.hash, outIndex), remoteParams,
        fundTx, localSpec, localCommitTx, firstRemoteCommit)

      become(d1, WAIT_FUNDING_SIGNED)
      me send fundingCreated


    // They have signed our first commit tx, we can broadcast a funding tx
    case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED)
      if remote.channelId == wait.channelId =>

      val dummy = PrivateKey(data = Tools.random getBytes 32, compressed = true).toPoint
      val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
        wait.remoteParams.fundingPubKey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

      if (Scripts.checkSpendable(signedLocalCommitTx).isFailure) become(wait, CLOSING) else {
        val localCommit = LocalCommit(index = 0, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
        val localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
        val remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty)

        val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit,
          remoteCommit = wait.remoteCommit, localChanges, remoteChanges, localNextHtlcId = 0,
          remoteNextHtlcId = 0, remoteNextCommitInfo = Right(dummy), wait.localCommitTx.input,
          remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), wait.channelId)

        // At this point funding tx should be broadcasted
        become(WaitFundingConfirmedData(wait.announce, None,
          None, wait.fundingTx, commitments), WAIT_FUNDING_DONE)
      }


    // Channel closing in WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED
    case (some, CMDShutdown, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) => become(some, CLOSING)
    case (wait: WaitAcceptData, err: Error, WAIT_FOR_ACCEPT) if err.channelId == wait.cmd.temporaryChannelId => become(wait, CLOSING)
    case (wait: WaitFundingData, err: Error, WAIT_FOR_FUNDING) if err.channelId == wait.cmd.temporaryChannelId => become(wait, CLOSING)
    case (wait: WaitFundingSignedData, err: Error, WAIT_FUNDING_SIGNED) if err.channelId == wait.channelId => become(wait, CLOSING)


    // FUNDING TX IS BROADCASTED AT THIS POINT


    // We have not yet sent a FundingLocked but just got one from them so we keep it
    case (wait: WaitFundingConfirmedData, their: FundingLocked, WAIT_FUNDING_DONE)
      if their.channelId == wait.commitments.channelId && wait.our.isEmpty =>
      me stayWith wait.copy(their = Some apply their)


    // We have already sent them a FundingLocked and now we got one from them
    case (wait: WaitFundingConfirmedData, their: FundingLocked, WAIT_FUNDING_DONE)
      if their.channelId == wait.commitments.channelId && wait.our.isDefined =>
      becomeNormal(wait, their)


    // We got our lock when their is already present so we can safely enter normal state now
    case (wait @ WaitFundingConfirmedData(_, _, Some(their), _, commitments, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
      if wait.fundingTx.txid == tx.txid =>

      becomeNormal(wait, their)
      me send makeFundingLocked(commitments)


    // We got our lock but their is not yet present so we save ours and just keep waiting for their
    case (wait @ WaitFundingConfirmedData(_, _, None, _, commitments, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
      if wait.fundingTx.txid == tx.txid =>

      val our = makeFundingLocked(commitments)
      stayAndSend(wait.copy(our = Some apply our), our)


    // NORMAL MODE


    case (norm @ NormalData(_, commitments, None, None, _), cmd: CMDAddHtlc, NORMAL) =>
      val c1 ~ updateAddHtlcMessage = Commitments.sendAdd(commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateAddHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, add: UpdateAddHtlc, NORMAL)
      if add.channelId == norm.commitments.channelId =>

      val incomingExpiry: Int = LNParams.broadcaster.currentHeight + 5
      val c1 = Commitments.receiveAdd(norm.commitments, add, incomingExpiry)
      me stayWith norm.copy(commitments = c1)


    // We're fulfilling an HTLC we got earlier
    case (norm: NormalData, cmd: CMDFulfillHtlc, NORMAL) =>
      val c1 ~ updateFulfillHtlcMessage = Commitments.sendFulfill(norm.commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFulfillHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
      if fulfill.channelId == norm.commitments.channelId =>

      // Got a fulfill for an outgoing HTLC we sent earlier
      val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
      me stayWith norm.copy(commitments = c1)


    // Failing an HTLC we got earlier
    case (norm: NormalData, cmd: CMDFailHtlc, NORMAL) =>
      val c1 ~ updateFailHtlcMessage = Commitments.sendFail(norm.commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFailHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, cmd: CMDFailMalformedHtlc, NORMAL) =>
      val c1 ~ updateFailMalformedHtlcMessage = Commitments.sendFailMalformed(norm.commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFailMalformedHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, fail: FailHtlc, NORMAL)
      if fail.channelId == norm.commitments.channelId =>

      // Got a failure for an outgoing HTLC we sent earlier
      val c1 = Commitments.receiveFail(norm.commitments, fail)
      me stayWith norm.copy(commitments = c1)


    // GUARD: we only send in the correct state
    case (norm: NormalData, CMDCommitSig, NORMAL)
      if Commitments canSendCommitSig norm.commitments =>

      // Propose new remote commit via commit tx signature
      val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
      val c1 ~ commitSigMessage = Commitments.sendCommit(norm.commitments, nextRemotePoint)
      stayAndSend(norm.copy(commitments = c1), commitSigMessage)


    // Serves as a trigger to enter negotiations when shutdown is on
    case (norm @ NormalData(_, commitments, Some(local), Some(remote), _), CMDCommitSig, NORMAL) =>
      if (Commitments hasNoPendingHtlcs commitments) startNegotiations(norm.announce, commitments, local, remote)


    case (norm: NormalData, sig: CommitSig, NORMAL)
      if sig.channelId == norm.commitments.channelId =>

      // We received a commit sig from them, now we can update our local commit
      val c1 ~ revokeMessage = Commitments.receiveCommit(norm.commitments, sig)
      stayAndSend(norm.copy(commitments = c1), revokeMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, rev: RevokeAndAck, NORMAL)
      if rev.channelId == norm.commitments.channelId =>

      // We received a revocation because we sent a commit sig
      val c1 = Commitments.receiveRevocation(norm.commitments, rev)
      me stayWith norm.copy(commitments = c1)
      doProcess(CMDCommitSig)


    case (norm: NormalData, CMDFeerate(rate), NORMAL)
      // GUARD: only send fee updates if the fee gap between nodes is large enough
      if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>

      // Periodic fee updates to ensure commit txs could be confirmed
      val c1 ~ updateFeeMessage = Commitments.sendFee(norm.commitments, rate)
      stayAndSend(norm.copy(commitments = c1), updateFeeMessage)
      doProcess(CMDCommitSig)


    // NORMAL: SHUTDOWN


    // We can start a mutual shutdown here, should be preferred to uncooperative
    case (norm @ NormalData(_, commitments, None, _, _), CMDShutdown, NORMAL) =>
      startShutdown(norm)


    case (norm: NormalData, remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId &&
        Commitments.remoteHasChanges(norm.commitments) =>

      // Can't start mutual shutdown
      startLocalCurrentClose(norm)


    // We have not yet sent or received a shutdown so send one and retry
    case (norm @ NormalData(_, commitments, None, None, _), remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId =>

      startShutdown(norm)
      doProcess(remote)


    // We have already sent a shutdown (initially or in response to their shutdown above)
    case (norm @ NormalData(announce, commitments, Some(local), None, _), remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId =>

      // We can start negotiations if there are no in-flight HTLCs, otherwise wait until they are cleared
      if (Commitments hasNoPendingHtlcs commitments) startNegotiations(announce, commitments, local, remote)
      else me stayWith norm.copy(remoteShutdown = Some apply remote)


    // Periodic watch for timed-out outgoing HTLCs
    case (norm: NormalData, CMDHeight(chainHeight), NORMAL | SYNC)
      if Commitments.hasTimedoutOutgoingHtlcs(norm.commitments, chainHeight) =>
      startLocalCurrentClose(norm)


    // SYNC MODE


    // We may get this message any time so just save it in this state
    case (wait: WaitFundingConfirmedData, CMDConfirmed(tx), SYNC)
      if wait.fundingTx.txid == tx.txid =>

      val our = makeFundingLocked(wait.commitments)
      me stayWith wait.copy(our = Some apply our)


    // This is a special case where we already have both locks when exiting a sync phase so we go directly to normal state
    case (wait @ WaitFundingConfirmedData(_, Some(our), Some(their), _, commitments, _), ChannelReestablish(channelId, 1, 0), SYNC)
      if channelId == commitments.channelId =>

      becomeNormal(wait, their)
      me send our


    // We're exiting a sync state but don't have enough locks so we keep waiting
    case (wait: WaitFundingConfirmedData, ChannelReestablish(channelId, 1, 0), SYNC)
      if channelId == wait.commitments.channelId =>

      become(wait, WAIT_FUNDING_DONE)
      wait.our foreach send


    // Should re-send last closingSigned according to spec
    case (neg: NegotiationsData, cr: ChannelReestablish, SYNC)
      if cr.channelId == neg.commitments.channelId =>

      become(neg, NEGOTIATIONS)
      me send neg.localClosingSigned
      memoCmdBuffer foreach doProcess


    case (norm: NormalData, cr: ChannelReestablish, SYNC)
      if norm.commitments.remoteChanges.acked.isEmpty &&
        norm.commitments.remoteChanges.signed.isEmpty &&
        norm.commitments.localCommit.index == 0 =>

      // They may not received our FundingLocked
      // also we could have started a local shutdown

      become(norm, NORMAL)
      me send makeFundingLocked(norm.commitments)
      for(our <- norm.localShutdown) me send our
      memoCmdBuffer foreach doProcess


    case (norm: NormalData, cr: ChannelReestablish, SYNC) =>
      for(our <- norm.localShutdown) me send our
      memoCmdBuffer foreach doProcess


    // We just close a channel if this is some kind of irregular state
    case (some: ChannelData with HasCommitments, cr: ChannelReestablish, SYNC)
      if cr.channelId == some.commitments.channelId =>
      startLocalCurrentClose(some)


    // Record command for future use
    case (_, cmd: MemoCommand, SYNC) =>
      val memoCmdBuffer1 = memoCmdBuffer.toVector :+ cmd
      memoCmdBuffer = memoCmdBuffer1.toIterator


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
        case Some(closeTx) if closeFeeSat == remoteFeeSat => startMutualClose(neg, closeTx)
        case Some(closeTx) if nextCloseFee == remoteFeeSat => startMutualClose(neg, closeTx)
        case Some(_) => stayAndSend(neg.copy(localClosingSigned = nextMessage), nextMessage)
        case _ => throw new LightningException
      }


    // Tuple(tx, does it spend a funding) instead of a Command
    case (neg: NegotiationsData, (spendTx: Transaction, true), NEGOTIATIONS) =>
      val (closeTx, _) = Closing.makeClosing(neg.commitments, neg.localShutdown.scriptPubKey,
        neg.remoteShutdown.scriptPubKey, closingFee = Satoshi apply neg.localClosingSigned.feeSatoshis)

      // Happens when we agreed on a closeSig, but we don't know it yet
      // we receive a tx notification before their ClosingSigned arrives
      if (closeTx.tx.txid == spendTx.txid) startMutualClose(neg, closeTx.tx)
      else defineClosingAction(neg, spendTx)


    // Something which spends our funding is broadcasted so we react in any state
    case (norm: ChannelData with HasCommitments, (spendTx: Transaction, true), _) =>
      defineClosingAction(norm, spendTx)


    // Check all incoming transactions if they spend our funding output
    case (some: ChannelData with HasCommitments, (spendTx: Transaction, false), _)
      if spendTx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>
      me doProcess Tuple2(spendTx, true)


    // Error in any state means we have to close a chan right now
    case (some: ChannelData with HasCommitments, err: Error, _)
      if err.channelId == some.commitments.channelId =>
      startLocalCurrentClose(some)


    // In all states except normal this will lead to uncooperative close
    case (some: ChannelData with HasCommitments, CMDShutdown, _) =>
      startLocalCurrentClose(some)


    case _ =>
      // Let know if received an unhandled message
      Tools log s"Channel: unhandled $change : $data"
  }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, index = 1)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = point)
  }

  private def becomeNormal(wait: WaitFundingConfirmedData, their: FundingLocked) =
    become(NormalData(wait.announce, wait.commitments.copy(remoteNextCommitInfo =
      Right apply their.nextPerCommitmentPoint), None, None), NORMAL)

  private def startShutdown(norm: NormalData) = {
    val finalScriptPubKey = norm.commitments.localParams.defaultFinalScriptPubKey
    val localShutdown = Shutdown(norm.commitments.channelId, finalScriptPubKey)
    become(norm.copy(localShutdown = Some apply localShutdown), NORMAL)
    me send localShutdown
  }

  private def startNegotiations(announce: NodeAnnouncement, cs: Commitments, local: Shutdown, remote: Shutdown) = {
    val firstSigned = Closing.makeFirstClosing(cs, local.scriptPubKey, remote.scriptPubKey, cs.localCommit.spec.feeratePerKw)
    become(NegotiationsData(announce, cs, firstSigned, local, remote), NEGOTIATIONS)
    me send firstSigned
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction) =
    become(ClosingData(neg.announce, neg.commitments, closeTx :: Nil), CLOSING)

  private def startLocalCurrentClose(some: ChannelData with HasCommitments) =
    // Something went wrong and we decided to spend our current commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, some.commitments.localCommit.commitTx.tx, LNParams.bag) -> some match {
      case (claim: LocalCommitPublished, closing: ClosingData) => become(data1 = closing.copy(localCommit = claim :: Nil), state1 = CLOSING)
      case (claim, _) => become(ClosingData(some.announce, some.commitments, localCommit = claim :: Nil), CLOSING)
    }

  private def defineClosingAction(some: ChannelData with HasCommitments, tx: Transaction) =
    // We are not sure what kind of closing transaction this is so we check against commitments
    some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit) match {
      case Left(remoteCommit) if remoteCommit.txid == tx.txid => startRemoteNextClose(some, tx, remoteCommit)
      case _ if some.commitments.remoteCommit.txid == tx.txid => startRemoteCurrentClose(some, tx)
      case _ => startOtherClose(some, tx)
    }

  private def startRemoteCurrentClose(some: ChannelData with HasCommitments, commitTx: Transaction) =
    // Something went wrong on their side and they decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, commitTx, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => become(data1 = closing.copy(remoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => become(ClosingData(some.announce, some.commitments, remoteCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteNextClose(some: ChannelData with HasCommitments, commitTx: Transaction, nextRemoteCommit: RemoteCommit) =
    // Something went wrong on their side and they decided to spend their NEXT commit transaction, we still need to take ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, commitTx, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => become(closing.copy(nextRemoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => become(ClosingData(some.announce, some.commitments, nextRemoteCommit = claim :: Nil), CLOSING)
    }

  private def startOtherClose(some: ChannelData with HasCommitments, commitTx: Transaction) =
    // This is a contract breach, they have spent a revoked transaction so we can take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, commitTx) -> some match {
      case (Some(claim), closing: ClosingData) => become(closing.modify(_.revokedCommits).using(claim +: _), CLOSING)
      case (Some(claim), _) => become(ClosingData(some.announce, some.commitments, revokedCommits = claim :: Nil), CLOSING)
      case _ => startLocalCurrentClose(some)
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