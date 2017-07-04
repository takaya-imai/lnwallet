package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._

import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.{Satoshi, Transaction}

import com.lightning.wallet.ln.Tools.runAnd
import fr.acinq.bitcoin.Crypto.PrivateKey


abstract class Channel extends StateMachine[ChannelData] { me =>
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
      me send wait.our.get


    // We got a local funding confirmation and now we need to decide what to do
    case (wait: WaitFundingConfirmedData, CMDSomethingConfirmed(tx), WAIT_FUNDING_DONE)
      if wait.fundingTx.txid == tx.txid =>

      val point = Generators.perCommitPoint(wait.commitments.localParams.shaSeed, index = 1)
      val our = FundingLocked(wait.commitments.channelId, nextPerCommitmentPoint = point)
      if (wait.their.isDefined) becomeNormal(wait, wait.their.get)
      else me stayWith wait.copy(our = Some apply our)
      me send our


    // Channel closing in WAIT_FUNDING_DONE
    // remember that from now on we have a funding transaction!
    case (wait: WaitFundingConfirmedData, CMDShutdown, WAIT_FUNDING_DONE) =>
      startLocalCurrentClose(wait)


    case (wait: WaitFundingConfirmedData, err: Error, WAIT_FUNDING_DONE)
      if err.channelId == wait.commitments.channelId =>
      startLocalCurrentClose(wait)


    // Either a first remote commit has been spent or this is an info leak
    case (wait: WaitFundingConfirmedData, CMDSomethingSpent(tx, true), WAIT_FUNDING_DONE) =>
      if (tx.txid == wait.commitments.remoteCommit.txid) startRemoteCurrentClose(wait, tx)
      else startLocalCurrentClose(wait)


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
    case (norm @ NormalData(_, commitments, _, _, _), cmd: CMDFulfillHtlc, NORMAL) =>
      val c1 ~ updateFulfillHtlcMessage = Commitments.sendFulfill(commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFulfillHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
      if fulfill.channelId == norm.commitments.channelId =>

      // Got a fulfill for an outgoing HTLC we sent earlier
      val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
      me stayWith norm.copy(commitments = c1)


    // Failing an HTLC we got earlier
    case (norm @ NormalData(_, commitments, _, _, _), cmd: CMDFailHtlc, NORMAL) =>
      val c1 ~ updateFailHtlcMessage = Commitments.sendFail(commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFailHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm @ NormalData(_, commitments, _, _, _), cmd: CMDFailMalformedHtlc, NORMAL) =>
      val c1 ~ updateFailMalformedHtlcMessage = Commitments.sendFailMalformed(commitments, cmd)
      stayAndSend(norm.copy(commitments = c1), updateFailMalformedHtlcMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, fail: FailHtlc, NORMAL)
      if fail.channelId == norm.commitments.channelId =>

      // Got a failure for an outgoing HTLC we sent earlier
      val c1 = Commitments.receiveFail(norm.commitments, fail)
      me stayWith norm.copy(commitments = c1)


    // GUARD: we only send in the correct state
    case (norm: NormalData, CMDCommitSig, NORMAL)
      if me canSendCommitSig norm.commitments =>

      // Propose new remote commit via commit tx signature
      val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
      val c1 ~ commitSigMessage = Commitments.sendCommit(norm.commitments, nextRemotePoint)
      stayAndSend(norm.copy(commitments = c1), commitSigMessage)


    case (norm: NormalData, sig: CommitSig, NORMAL)
      if sig.channelId == norm.commitments.channelId =>

      // We received a commit sig from them, now we can update our local commit
      val c1 ~ revokeAndAckMessage = Commitments.receiveCommit(norm.commitments, sig)
      stayAndSend(norm.copy(commitments = c1), revokeAndAckMessage)
      doProcess(CMDCommitSig)


    case (norm: NormalData, rev: RevokeAndAck, NORMAL)
      if rev.channelId == norm.commitments.channelId =>

      // We received a revocation because we sent a commit sig
      val c1 = Commitments.receiveRevocation(norm.commitments, rev)
      me stayWith norm.copy(commitments = c1)
      doProcess(CMDCommitSig)


    case (norm: NormalData, CMDFeerate(rate), NORMAL)
      // GUARD: we only send fee updates if the fee gap between nodes is large enough
      if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>

      // Periodic fee updates to ensure commit txs could be confirmed
      val c1 ~ updateFeeMessage = Commitments.sendFee(norm.commitments, rate)
      stayAndSend(norm.copy(commitments = c1), updateFeeMessage)
      doProcess(CMDCommitSig)


    // Periodic watch for timed-out outgoing HTLCs
    case (norm: NormalData, CMDDepth(blockHeight), NORMAL)
      if Commitments.hasTimedoutOutgoingHtlcs(norm.commitments, blockHeight) =>
      startLocalCurrentClose(norm)


    // When initiating or receiving a shutdown message
    // we can't proceed until all local changes are cleared
    // and we can't enter negotiations util pending HTLCs are present


    case (norm: NormalData, CMDShutdown, NORMAL)
      if !Commitments.localHasChanges(norm.commitments) =>
      initiateShutdown(norm)


    // We have not yet send or received a shutdown so send it and retry
    case (norm @ NormalData(_, _, None, None, _), remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId =>

      initiateShutdown(norm)
      doProcess(remote)


    // We have already sent a shutdown (initially or in response to their shutdown)
    case (norm @ NormalData(announce, commitments, Some(local), None, _), remote: Shutdown, NORMAL)
      if remote.channelId == norm.commitments.channelId =>

      val isOk = Commitments hasNoPendingHtlcs commitments
      if (isOk) startNegotiations(announce, commitments, local, remote)
      else me stayWith norm.copy(remoteShutdown = Some apply remote)


    // Something which spends our funding is broadcasted
    case (norm: NormalData, CMDSomethingSpent(tx, true), NORMAL) =>
      defineClosingAction(norm, tx)


    case (norm: NormalData, err: Error, NORMAL)
      if err.channelId == norm.commitments.channelId =>
      startLocalCurrentClose(norm)


    // NEGOTIATIONS MODE


    // At this point commitments.unackedMessages may contain:
    // - Shutdown, but it is acknowledged since we just received ClosingSigned
    // - ClosingSigned, but they are never acknowledged and spec says we only need to re-send the last one
    // this means that we can just set commitments.unackedMessages to the last sent ClosingSigned
    case (neg: NegotiationsData, ClosingSigned(channelId, feeSatoshis, remoteSig), NEGOTIATIONS)
      if channelId == neg.commitments.channelId =>

      val Seq(closeFeeSat, remoteFeeSat) = Seq(neg.localClosingSigned.feeSatoshis, feeSatoshis) map Satoshi
      val Seq(localScript, remoteScript) = Seq(neg.localShutdown.scriptPubKey, neg.remoteShutdown.scriptPubKey)
      val closeTxOpt = Closing.checkClosingSignature(neg.commitments, localScript, remoteScript, remoteFeeSat, remoteSig)

      lazy val nextCloseFee = Closing.nextClosingFee(closeFeeSat, remoteFeeSat)
      lazy val (_, nextLocalClosingSigned) = Closing.makeClosing(neg.commitments,
        localScript, remoteScript, nextCloseFee)

      closeTxOpt match {
        case None => throw new LightningException
        case Some(tx) if closeFeeSat == remoteFeeSat => startMutualClose(neg, tx, neg.localClosingSigned)
        case Some(tx) if nextCloseFee == remoteFeeSat => startMutualClose(neg, tx, nextLocalClosingSigned)
        case Some(tx) =>
          // Continue negotiations...
          val c1 = Commitments.addUnacked(neg.commitments, nextLocalClosingSigned)
          me stayWith neg.copy(commitments = c1, localClosingSigned = nextLocalClosingSigned)
      }


    // Unilateral channel closing in NEGOTIATIONS
    case (neg: NegotiationsData, CMDShutdown, NEGOTIATIONS) =>
      startLocalCurrentClose(neg)


    case (neg: NegotiationsData, err: Error, NEGOTIATIONS)
      if err.channelId == neg.commitments.channelId =>
      startLocalCurrentClose(neg)


    case (neg: NegotiationsData, CMDFundingSpent(tx), NEGOTIATIONS) =>
      val (closeTx, signed) = Closing.makeClosing(neg.commitments, neg.localShutdown.scriptPubKey,
        neg.remoteShutdown.scriptPubKey, closingFee = Satoshi apply neg.localClosingSigned.feeSatoshis)

      // Happens when we agreed on a closeSig, but we don't know it yet
      // we receive a tx notification before their ClosingSigned arrives
      if (closeTx.tx.txid == tx.txid) startMutualClose(neg, closeTx.tx, signed)
      else defineClosingAction(neg, tx)


    // Channel closing events in CLOSING phase
    case (closing: ClosingData, CMDFundingSpent(tx), CLOSING) =>
      defineClosingAction(closing, tx)


    case (some: ChannelData with HasCommitments, CMDSomethingSpent(tx), _)
      // GUARD: check all incoming transactions if they spend our funding output
      if tx.txIn.exists(_.outPoint == some.commitments.commitInput.outPoint) =>
      doProcess(CMDFundingSpent apply tx)


    case (_, _: CMDDepth, _) =>
      // IMPORTANT: active state listeners should be idempotent
      // IMPORTANT: listeners should not send CMDDepth from onBecome
      notifyListeners


    case _ =>
      // Let know if received an unhandled message
      Tools log s"Channel: unhandled $change: $data"
  }

  private def canSendCommitSig(cs: Commitments): Boolean =
    Commitments.localHasChanges(cs) && cs.remoteNextCommitInfo.isRight

  private def becomeNormal(wait: WaitFundingConfirmedData, their: FundingLocked) =
    become(NormalData(wait.announce, wait.commitments.copy(remoteNextCommitInfo =
      Right apply their.nextPerCommitmentPoint), None, None), NORMAL)

  private def initiateShutdown(norm: NormalData) = {
    val local = Shutdown(norm.commitments.channelId, norm.commitments.localParams.defaultFinalScriptPubKey)
    me stayWith norm.copy(commitments = Commitments.addUnacked(norm.commitments, local), localShutdown = Some apply local)
  }

  private def startNegotiations(announce: NodeAnnouncement, cs: Commitments, local: Shutdown, remote: Shutdown) = {
    val firstSigned = Closing.makeFirstClosing(cs, local.scriptPubKey, remote.scriptPubKey, cs.localCommit.spec.feeratePerKw)
    become(NegotiationsData(announce, Commitments.addUnacked(cs, firstSigned), firstSigned, local, remote), state1 = NEGOTIATIONS)
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction, signed: ClosingSigned) = {
    // Negotiations completed successfully and we can broadcast a mutual closing transaction while entering a CLOSING state
    val closing = ClosingData(neg.announce, Commitments.addUnacked(neg.commitments, signed), mutualClose = closeTx :: Nil)
    become(closing, state1 = CLOSING)
  }

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
      case _ => startRemoteOther(some, tx)
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

  private def startRemoteOther(some: ChannelData with HasCommitments, commitTx: Transaction) =
    // This is a contract breach, they have spent a revoked transaction so we can take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, commitTx) -> some match {
      case (Some(claim), closing: ClosingData) => become(closing.modify(_.revokedCommits).using(claim +: _), CLOSING)
      case (Some(claim), _) => become(ClosingData(some.announce, some.commitments, revokedCommits = claim :: Nil), CLOSING)
      case (None, _) => startLocalCurrentClose(some) // Info leak, try to spend current commit
    }

  def send(message: LightningMessage): Unit
  def notifyListeners = events onBecome Tuple3(data, null, state)
  def stayAndSend(data: ChannelData, message: LightningMessage): Unit =
    runAnd(me stayWith data)(me send message)
}

object Channel {
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FOR_FUNDING = "WaitForFunding"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"
  val WAIT_FUNDING_DONE = "WaitFundingDone"
  val NEGOTIATIONS = "Negotiations"
  val CLOSING = "Closing"
  val NORMAL = "Normal"
}