package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.Exceptions._

import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}


class Channel(bag: InvoiceBag)
extends StateMachine[ChannelData] { me =>

  def doProcess(change: Any) = (change, data, state) match {
    case (cmd: CMDOpenChannel, InitData(announce), WAIT_FOR_INIT) =>

      val lp = cmd.localParams
      val open = OpenChannel(lp.chainHash, cmd.temporaryChannelId, cmd.fundingSatoshis, cmd.pushMsat, lp.dustLimitSatoshis,
        lp.maxHtlcValueInFlightMsat, lp.channelReserveSatoshis, lp.htlcMinimumMsat, cmd.initialFeeratePerKw, lp.toSelfDelay,
        lp.maxAcceptedHtlcs, lp.fundingPrivKey.publicKey, lp.revocationSecret.toPoint, lp.paymentKey.toPoint,
        Generators.perCommitPoint(lp.shaSeed, 0), lp.delayedPaymentKey.toPoint)

      val data1 = WaitAcceptData(announce, cmd, open)
      become(data1, state1 = WAIT_FOR_ACCEPT)

    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT)
      // GUARD: remote requires local to keep too much in reserve which is unacceptable
      if LNParams.exceedsReserve(accept.channelReserveSatoshis, waitAccept.cmd.fundingSatoshis) =>
      become(waitAccept, FINISHED)

    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT) =>
      val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat,
        accept.channelReserveSatoshis, accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs,
        accept.fundingPubkey, accept.revocationBasepoint, accept.paymentBasepoint, accept.delayedPaymentBasepoint,
        waitAccept.cmd.remoteInit.globalFeatures, waitAccept.cmd.remoteInit.localFeatures)

      become(WaitFundingTxData(waitAccept.announce, waitAccept.cmd,
        accept.firstPerCommitmentPoint, remoteParams, waitAccept.lastSent),
        state1 = WAIT_FUNDING_CREATED)

    // We now have a funding tx with funding out
    case (Tuple2(funding: Transaction, outIndex: Int), wait: WaitFundingTxData, WAIT_FUNDING_CREATED) =>
      val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(wait.cmd.localParams, wait.remoteParams,
        wait.cmd.fundingSatoshis, wait.cmd.pushMsat, wait.cmd.initialFeeratePerKw, funding.hash, outIndex, wait.remoteFirstPerCommitmentPoint)

      val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, wait.cmd.localParams.fundingPrivKey)
      val fundingCreated = FundingCreated(wait.cmd.temporaryChannelId, funding.hash, outIndex, localSigOfRemoteTx)
      val firstRemoteCommit = RemoteCommit(0L, remoteSpec, remoteCommitTx.tx.txid, wait.remoteFirstPerCommitmentPoint)

      become(WaitFundingSignedData(wait.announce, Tools.toLongId(funding.hash, outIndex),
        wait.cmd.localParams, wait.remoteParams, funding, localSpec, localCommitTx,
        firstRemoteCommit, fundingCreated), state1 = WAIT_FUNDING_SIGNED)

    // They have signed our first commit tx, we can broadcast a funding tx
    case (remote: FundingSigned, wait: WaitFundingSignedData, WAIT_FUNDING_SIGNED) =>
      val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
        wait.remoteParams.fundingPubKey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

      if (Scripts.checkSpendable(signedLocalCommitTx).isFailure) become(wait, FINISHED) else {
        val localCommit = LocalCommit(0L, wait.localSpec, PublishableTxs(Nil, signedLocalCommitTx), null)
        val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit, wait.remoteCommit,
          localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
          remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty),
          localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(null),
          unackedMessages = Vector.empty, commitInput = wait.localCommitTx.input,
          ShaHashesWithIndex(Map.empty, None), wait.channelId)

        // At this point funding tx should be broadcasted
        // lastSent may contain FundingCreated or FundingLocked
        become(WaitFundingConfirmedData(wait.announce, commitments,
          None, wait.lastSent), state1 = WAIT_FUNDING_DONE)
      }

    // Channel closing in WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FUNDING_CREATED | WAIT_FUNDING_SIGNED
    case (CMDShutdown, some, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FUNDING_CREATED | WAIT_FUNDING_SIGNED) => become(some, FINISHED)
    case (_: Error, some, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FUNDING_CREATED | WAIT_FUNDING_SIGNED) => become(some, FINISHED)

    // FUNDING TX IS BROADCASTED AT THIS POINT

    // We have not yet sent a FundingLocked to them but just got a FundingLocked from them so we keep it
    case (their: FundingLocked, wait @ WaitFundingConfirmedData(_, _, _, _: FundingCreated), WAIT_FUNDING_DONE) =>
      me stayWith wait.copy(their = Some apply their)

    // We have got a FundingDepthOk blockchain event but have not yet got a confirmation from them
    case (Tuple2(CMDDepth, LNParams.minDepth), wait @ WaitFundingConfirmedData(_, commitments, None, _), WAIT_FUNDING_DONE) =>
      val nextPerCommitmentPoint = Generators.perCommitPoint(commitments.localParams.shaSeed, index = 1)
      val our = FundingLocked(commitments.channelId, nextPerCommitmentPoint)
      me stayWith wait.copy(lastSent = our)

    // We have already sent them a FundingLocked some time ago, now we got a FundingLocked from them
    case (their: FundingLocked, wait @ WaitFundingConfirmedData(_, commitments, _, _: FundingLocked), WAIT_FUNDING_DONE) =>
      becomeNormal(wait, their.nextPerCommitmentPoint)

    // They have already sent us a FundingLocked message so we got it saved and now we get a FundingDepthOk blockchain event
    case (Tuple2(CMDDepth, LNParams.minDepth), wait @ WaitFundingConfirmedData(_, commitments, Some(their), _), WAIT_FUNDING_DONE) =>
      becomeNormal(wait, their.nextPerCommitmentPoint)

    // Channel closing in WAIT_FUNDING_DONE (from now on we have a funding transaction)
    case (_: Error, wait: WaitFundingConfirmedData, WAIT_FUNDING_DONE) => startLocalCurrentClose(wait)
    case (CMDShutdown, wait: WaitFundingConfirmedData, WAIT_FUNDING_DONE) => startLocalCurrentClose(wait)
    case (Tuple2(CMDFundingSpent, tx: Transaction), wait: WaitFundingConfirmedData, WAIT_FUNDING_DONE) =>
      // We don't yet have any revoked commits so they either spent a first commit or it's an info leak
      if (tx.txid == wait.commitments.remoteCommit.txid) startRemoteCurrentClose(wait, tx)
      else startLocalCurrentClose(wait)

    // NORMAL MODE

    case (htlc: Htlc, norm: NormalData, NORMAL)
      // Throw an exception so we can do something about it
      // GUARD: can't add new outgoing HTLCs when closing is in progress
      if norm.localShutdown.isDefined | norm.remoteShutdown.isDefined =>
      throw DetailedException(CHANNEL_SHUTDOWN_IN_PROGRESS, htlc)

    case (htlc: Htlc, norm: NormalData, NORMAL) =>
      val chainHeight: Int = LNParams.broadcaster.currentHeight
      val c1 = Commitments.sendAdd(norm.commitments, htlc, chainHeight)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    case (add: UpdateAddHtlc, norm: NormalData, NORMAL) =>
      val chainHeight: Int = LNParams.broadcaster.currentHeight
      val c1 = Commitments.receiveAdd(norm.commitments, add, chainHeight)
      me stayWith norm.copy(commitments = c1)

    // We're fulfilling an HTLC we got earlier
    case (cmd: CMDFulfillHtlc, norm: NormalData, NORMAL) =>
      val c1 = Commitments.sendFulfill(norm.commitments, cmd)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    // Got a fulfill for an HTLC we sent earlier
    case (fulfill: UpdateFulfillHtlc, norm: NormalData, NORMAL) =>
      val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
      me stayWith norm.copy(commitments = c1)

    // Failing an HTLC we got earlier
    case (cmd: CMDFailHtlc, norm: NormalData, NORMAL) =>
      val c1 = Commitments.sendFail(norm.commitments, cmd)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    case (cmd: CMDFailMalformedHtlc, norm: NormalData, NORMAL) =>
      val c1 = Commitments.sendFailMalformed(norm.commitments, cmd)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    // Got a failure for an HTLC we sent earlier
    case (fail: FailHtlc, norm: NormalData, NORMAL) =>
      val c1 = Commitments.receiveFail(norm.commitments, fail)
      me stayWith norm.copy(commitments = c1)

    // Send new commit tx signature
    case (CMDCommitSig, norm: NormalData, NORMAL)
      // GUARD: we only send it if we have changes
      if Commitments localHasChanges norm.commitments =>

      // If afterCommit is not empty we process it once
      norm.commitments.remoteNextCommitInfo.right foreach { point =>
        val commitments1 = Commitments.sendCommit(norm.commitments, point)
        me stayWith norm.copy(afterCommit = None, commitments = commitments1)
        norm.afterCommit foreach process
      }

    // GUARD: we have received a commit sig from them when shutdown is fully confirmed by both parties
    case (sig: CommitSig, norm @ NormalData(announce, commitments, _, Some(local), Some(remote), _), NORMAL) =>

      val c1 = Commitments.receiveCommit(commitments, sig)
      val canStartNegotiations = Commitments hasNoPendingHtlcs c1
      if (canStartNegotiations) startNegotiations(announce, c1, local, remote)
      if (!canStartNegotiations) me stayWith norm.copy(commitments = c1)
      if (!canStartNegotiations) process(CMDCommitSig)

    // We received a commit sig from them
    case (sig: CommitSig, norm: NormalData, NORMAL) =>
      val c1 = Commitments.receiveCommit(norm.commitments, sig)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    // GUARD: we have received a revocation from them when shutdown is fully confirmed by both parties
    case (rev: RevokeAndAck, norm @ NormalData(announce, commitments, _, Some(local), Some(remote), _), NORMAL) =>

      val c1 = Commitments.receiveRevocation(commitments, rev)
      val canStartNegotiations = Commitments hasNoPendingHtlcs c1
      if (canStartNegotiations) startNegotiations(announce, c1, local, remote)
      if (!canStartNegotiations) me stayWith norm.copy(commitments = c1)
      if (!canStartNegotiations) process(CMDCommitSig)

    // We received a revocation because we sent a sig
    case (rev: RevokeAndAck, norm: NormalData, NORMAL) =>
      val c1 = Commitments.receiveRevocation(norm.commitments, rev)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    // Periodic fee updates
    case (Tuple2(CMDFeerate, rate: Long), norm: NormalData, NORMAL)
      // GUARD: we only send fee updates if the fee gap between nodes is large enough
      if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>
      val c1 = Commitments.sendFee(norm.commitments, rate)
      me stayWith norm.copy(commitments = c1)
      process(CMDCommitSig)

    // Periodic watch for timed-out outgoing HTLCs
    case (Tuple2(CMDDepth, count: Int), norm: NormalData, NORMAL)
      if Commitments.hasTimedoutOutgoingHtlcs(norm.commitments, count) =>
      startLocalCurrentClose(norm)

    // Can only send announcement signatures when no closing is in progress
    case (remote: AnnouncementSignatures, norm @ NormalData(_, commitments, false, None, None, _), NORMAL) =>
      val (localNodeSig, localBitcoinSig) = Announcements.signChannelAnnouncement(shortChannelId = remote.shortChannelId,
        LNParams.extendedPrivateKey.privateKey, PublicKey(norm.announce.nodeId), commitments.localParams.fundingPrivKey,
        commitments.remoteParams.fundingPubKey, commitments.localParams.globalFeatures)

      // Let the other node announce our channel availability
      val as = AnnouncementSignatures(commitments.channelId, remote.shortChannelId, localNodeSig, localBitcoinSig)
      me stayWith norm.copy(commitments = Commitments.addUnackedMessage(commitments, as), announced = true)

    // When initiating or receiving a shutdown message
    // we can't proceed until all local changes are cleared
    // and we can't enter negotiations util pending HTLCs are present

    case (CMDShutdown, norm: NormalData, NORMAL)
      // GUARD: postpone shutdown if we have changes
      if Commitments localHasChanges norm.commitments =>
      me stayWith norm.copy(afterCommit = Some apply CMDShutdown)
      process(CMDCommitSig)

    case (CMDShutdown, norm: NormalData, NORMAL) =>
      initiateShutdown(norm)

    case (_: Shutdown, norm: NormalData, NORMAL)
      // GUARD: can't accept shutdown with unacked changes
      if norm.commitments.remoteChanges.proposed.nonEmpty =>
      throw ChannelException(CHANNEL_CLOSE_PENDING_CHANGES)

    case (remote: Shutdown, norm: NormalData, NORMAL)
      // GUARD: postpone our reply if we have changes
      if Commitments localHasChanges norm.commitments =>
      me stayWith norm.copy(afterCommit = Some apply remote)
      process(CMDCommitSig)

    // We have not yet send or received a shutdown so send it and retry
    case (remote: Shutdown, norm @ NormalData(_, _, _, None, None, _), NORMAL) =>
      initiateShutdown(norm)
      process(remote)

    // We have already sent a shutdown (initially or in response to their shutdown)
    case (remote: Shutdown, norm @ NormalData(announce, commitments, _, Some(local), None, _), NORMAL) =>
      if (Commitments hasNoPendingHtlcs commitments) startNegotiations(announce, commitments, local, remote)
      else me stayWith norm.copy(remoteShutdown = Some apply remote)

    // Unilateral channel closing in NORMAL (bilateral is handled above)
    case (_: Error, norm: NormalData, NORMAL) => startLocalCurrentClose(norm)
    case (Tuple2(CMDFundingSpent, tx: Transaction), norm: NormalData, NORMAL) =>
      defineClosingAction(norm, tx)

    // NEGOTIATIONS MODE

    // At this point commitments.unackedMessages may contain:
    // - Shutdown, but it is acknowledged since we just received ClosingSigned
    // - ClosingSigned, but they are never acknowledged and spec says we only need to re-send the last one
    // this means that we can just set commitments.unackedMessages to the last sent ClosingSigned

    case (ClosingSigned(_, remoteClosingFee, remoteSig), neg: NegotiationsData, NEGOTIATIONS) =>
      val Seq(closeFeeSat, remoteFeeSat) = Seq(neg.localClosingSigned.feeSatoshis, remoteClosingFee) map Satoshi
      val Seq(localKey: BinaryData, remoteKey) = Seq(neg.localShutdown.scriptPubKey, neg.remoteShutdown.scriptPubKey)
      val (checks, closeTx) = Closing.checkClosingSignature(neg.commitments, localKey, remoteKey, remoteFeeSat, remoteSig)
      if (!checks) throw ChannelException(CHANNEL_CLOSE_SIG_FAIL)

      if (closeFeeSat != remoteFeeSat) {
        val nextCloseFee: Satoshi = Closing.nextClosingFee(closeFeeSat, remoteFeeSat)
        val (_, nextLocalClosingSigned) = Closing.makeClosingTx(neg.commitments, localKey, remoteKey, nextCloseFee)
        if (nextCloseFee != remoteFeeSat) updateNegotiations(neg, signed = nextLocalClosingSigned)
        else startMutualClose(neg, closeTx, nextLocalClosingSigned)
      } else startMutualClose(neg, closeTx, neg.localClosingSigned)

    // Channel closing in NEGOTIATIONS
    case (_: Error, neg: NegotiationsData, NEGOTIATIONS) => startLocalCurrentClose(neg)
    case (CMDShutdown, neg: NegotiationsData, NEGOTIATIONS) => startLocalCurrentClose(neg)
    case (Tuple2(CMDFundingSpent, tx: Transaction), neg: NegotiationsData, NEGOTIATIONS) =>
      val (closeTx, signed) = Closing.makeClosingTx(neg.commitments, neg.localShutdown.scriptPubKey,
        neg.remoteShutdown.scriptPubKey, closingFee = Satoshi apply neg.localClosingSigned.feeSatoshis)

      // Happens when we agreed on a closeSig, but we don't know it yet
      // we receive the a tx notification before their ClosingSigned arrives
      if (closeTx.tx.txid == tx.txid) startMutualClose(neg, closeTx.tx, signed)
      else defineClosingAction(neg, tx)

    // Channel closing evenis in CLOSING phase
    // These spends have already been taken care of
    case (Tuple2(CMDFundingSpent, tx: Transaction), closing: ClosingData, CLOSING)
      if closing.nextRemoteCommit.map(_.commitTx.txid).contains(tx.txid) ||
        closing.remoteCommit.map(_.commitTx.txid).contains(tx.txid) ||
        closing.localCommit.map(_.commitTx.txid).contains(tx.txid) ||
        closing.mutualClose.map(_.txid).contains(tx.txid) =>
        // Do nothing

    // Some other type of tx has been spent while we await for confirmations
    case (Tuple2(CMDFundingSpent, tx: Transaction), closing: ClosingData, CLOSING) => defineClosingAction(closing, tx)
    case (CMDClosingFinished, closing: ClosingData, CLOSING) => become(closing, state1 = FINISHED)

    case otherwise =>
      // Let know if received an unhandled message
      android.util.Log.d("Channel", s"Unhandled $otherwise")
  }

  private def becomeNormal(source: ChannelData with HasCommitments, theirNextPoint: Point) = {
    val commitments1 = source.commitments.modify(_.remoteNextCommitInfo) setTo Right(theirNextPoint)
    become(NormalData(source.announce, commitments1, announced = false, None, None), state1 = NORMAL)
  }

  private def initiateShutdown(norm: NormalData) = {
    val local = Shutdown(norm.commitments.channelId, norm.commitments.localParams.defaultFinalScriptPubKey)
    me stayWith norm.copy(commitments = Commitments.addUnackedMessage(norm.commitments, local), localShutdown = Some apply local)
  }

  private def startNegotiations(announce: NodeAnnouncement, cs: Commitments, local: Shutdown, remote: Shutdown) = {
    val firstSigned = Closing.makeFirstClosingTx(cs, local.scriptPubKey, remote.scriptPubKey, cs.localCommit.spec.feeratePerKw)
    become(NegotiationsData(announce, Commitments.addUnackedMessage(cs, firstSigned), firstSigned, local, remote), state1 = NEGOTIATIONS)
  }

  private def updateNegotiations(neg: NegotiationsData, signed: ClosingSigned) = {
    // Negotiations are in progress, we send our next estimated fee in an attempt of reaching a mutual agreement
    val neg1 = neg.copy(commitments = neg.commitments.copy(unackedMessages = Vector apply signed), localClosingSigned = signed)
    become(neg1, state1 = NEGOTIATIONS)
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction, signed: ClosingSigned) = {
    // Negotiations completed successfully and we can broadcast a mutual closing transaction while entering a CLOSING state
    val closing = ClosingData(neg.announce, neg.commitments.copy(unackedMessages = Vector apply signed), mutualClose = closeTx :: Nil)
    become(closing, state1 = CLOSING)
  }

  private def startLocalCurrentClose(some: ChannelData with HasCommitments) = {
    // Something went wrong and we decided to spend our current commit transaction
    val commitTx: Transaction = some.commitments.localCommit.publishableTxs.commitTx.tx
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, commitTx, bag) -> some match {
      case (claim, closing: ClosingData) => become(data1 = closing.copy(localCommit = claim :: Nil), state1 = CLOSING)
      case (claim, _) => become(data1 = ClosingData(some.announce, some.commitments, localCommit = claim :: Nil), CLOSING)
    }
  }

  private def startRemoteCurrentClose(some: ChannelData with HasCommitments, commitTx: Transaction) =
    // Something went wrong on their side and they decided to spend their CURRENT commit tx, we need to take ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, commitTx, bag) -> some match {
      case (claim, closing: ClosingData) => become(data1 = closing.copy(remoteCommit = claim :: Nil), state1 = CLOSING)
      case (claim, _) => become(data1 = ClosingData(some.announce, some.commitments, remoteCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteNextClose(some: ChannelData with HasCommitments, commitTx: Transaction, nextRemoteCommit: RemoteCommit) =
    // Something went wrong on their side and they decided to spend their NEXT commit transaction, we still need to take ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, commitTx, bag) -> some match {
      case (claim, closing: ClosingData) => become(data1 = closing.copy(nextRemoteCommit = claim :: Nil), state1 = CLOSING)
      case (claim, _) => become(data1 = ClosingData(some.announce, some.commitments, nextRemoteCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteOther(some: ChannelData with HasCommitments, commitTx: Transaction) =
    // This is a contract breach, they have spent a revoked transaction so we can take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, commitTx) -> some match {
      case (Some(claim), closing: ClosingData) => become(data1 = closing.modify(_.revokedCommits).using(claim +: _), state1 = CLOSING)
      case (Some(claim), _) => become(data1 = ClosingData(some.announce, some.commitments, revokedCommits = claim :: Nil), CLOSING)
      case (None, _) => startLocalCurrentClose(some) // Info leak, try to spend current commit
    }

  private def defineClosingAction(some: ChannelData with HasCommitments, tx: Transaction) =
    some.commitments.remoteNextCommitInfo.left.map(waiting => waiting.nextRemoteCommit) match {
      case Left(remoteCommit) if remoteCommit.txid == tx.txid => startRemoteNextClose(some, tx, remoteCommit)
      case _ if some.commitments.remoteCommit.txid == tx.txid => startRemoteCurrentClose(some, tx)
      case _ => startRemoteOther(some, tx)
    }
}

object Channel {
  // Channel states
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FUNDING_CREATED = "WaitFundingCreated"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"
  val WAIT_FUNDING_DONE = "WaitFundingDone"
  val NEGOTIATIONS = "Negotiations"
  val FINISHED = "Finished"
  val CLOSING = "Closing"
  val NORMAL = "Normal"

  // Normal text commands
  val CMDCommitSig = "CMDCommitSig"
  val CMDFeerate = "CMDFeerate"
  val CMDDepth = "CMDDepth"

  // Closing text commands
  val CMDShutdown = "CMDShutdown"
  val CMDFundingSpent = "CMDFundingSpent"
  val CMDClosingFinished = "CMDClosingFinished"
}