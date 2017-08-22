package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.Tools.{none, runAnd}
import com.lightning.wallet.ln.Helpers.{Closing, Funding}
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi, Transaction}
import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.PaymentRoute
import concurrent.ExecutionContext.Implicits.global
import fr.acinq.bitcoin.Crypto.PrivateKey
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Success


abstract class Channel extends StateMachine[ChannelData] { me =>
  override def process(change: Any) = try super.process(change) catch events.onError
  def id = Some(data) collect { case some: HasCommitments => some.commitments.channelId }
  def async(change: Any) = Future apply process(change)
  val listeners = mutable.Set.empty[ChannelListener]

  private[this] val events = new ChannelListener {
    override def onError = { case error => for (lst <- listeners if lst.onError isDefinedAt error) lst onError error }
    override def onBecome = { case trans => for (lst <- listeners if lst.onBecome isDefinedAt trans) lst onBecome trans }
    override def onProcess = { case some => for (lst <- listeners if lst.onProcess isDefinedAt some) lst onProcess some }
  }

  def SEND(msg: LightningMessage): Unit
  def STORE(content: HasCommitments): HasCommitments
  def UPDATE(d1: ChannelData): Channel = BECOME(d1, state)
  def BECOME(data1: ChannelData, state1: String) = runAnd(me) {
    // Transition should always be defined before vars are updated
    val trans = Tuple4(me, data1, state, state1)
    super.become(data1, state1)
    events onBecome trans
  }

  def outPaymentOpt(rs: Vector[PaymentRoute], request: PaymentRequest) =
    Some(rs, id) collect { case (route +: restOfReservedRoutes) ~ Some(chanId) =>
      val startConditions = (Vector.empty, request.amount.get.amount, LNParams.expiry)
      val (payloads, firstAmount, firstExpiry) = buildRoute(startConditions, route)

      val keyChain = data.announce.nodeId +: route.map(_.nextNodeId)
      val onion = buildOnion(keyChain, payloads, request.paymentHash)
      val routing = RoutingData(restOfReservedRoutes, onion, firstAmount, firstExpiry)
      OutgoingPayment(routing, NOIMAGE, request, MilliSatoshi(0), chanId, TEMP)
    }

  def doProcess(change: Any) = {
    Tuple3(data, change, state) match {
      case (InitData(announce), cmd @ CMDOpenChannel(localParams, tempId,
        initialFeeratePerKw, pushMsat, _, fundingSat), WAIT_FOR_INIT) =>

        BECOME(WaitAcceptData(announce, cmd), WAIT_FOR_ACCEPT) SEND OpenChannel(LNParams.chainHash,
          tempId, fundingSat, pushMsat, localParams.dustLimitSatoshis, localParams.maxHtlcValueInFlightMsat,
          localParams.channelReserveSat, LNParams.htlcMinimumMsat, initialFeeratePerKw, localParams.toSelfDelay,
          localParams.maxAcceptedHtlcs, localParams.fundingPrivKey.publicKey, localParams.revocationSecret.toPoint,
          localParams.paymentKey.toPoint, localParams.delayedPaymentKey.toPoint,
          Generators.perCommitPoint(localParams.shaSeed, index = 0),
          channelFlags = 1.toByte) // TODO: remove announce


      case (wait @ WaitAcceptData(announce, cmd), accept: AcceptChannel, WAIT_FOR_ACCEPT)
        if accept.temporaryChannelId == cmd.temporaryChannelId =>

        // If remote requires us to keep too much in local reserve we should close
        // otherwise we should wait for user to create a funding transaction in wallet UI
        val exceedsReserve = LNParams.exceedsReserve(accept.channelReserveSatoshis, cmd.fundingAmountSat)
        if (exceedsReserve) BECOME(wait, CLOSING) else BECOME(WaitFundingData(announce, cmd, accept), WAIT_FOR_FUNDING)


      // They have accepted our proposal, now let them sign a first commit tx
      case (WaitFundingData(announce, cmd, accept), (fundTx: Transaction, outIndex: Int), WAIT_FOR_FUNDING) =>
        val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) = Funding.makeFirstFunderCommitTxs(cmd, accept,
          fundTx.hash, outIndex, accept.firstPerCommitmentPoint)

        val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, cmd.localParams.fundingPrivKey)
        val fundingCreated = FundingCreated(cmd.temporaryChannelId, fundTx.hash, outIndex, localSigOfRemoteTx)
        val firstRemoteCommit = RemoteCommit(index = 0, remoteSpec, remoteCommitTx.tx.txid, accept.firstPerCommitmentPoint)

        BECOME(WaitFundingSignedData(announce, cmd.localParams,
          Tools.toLongId(fundTx.hash, outIndex), accept, fundTx, localSpec,
          localCommitTx, firstRemoteCommit), WAIT_FUNDING_SIGNED) SEND fundingCreated


      // They have signed our first commit tx, we can broadcast a funding tx
      case (wait: WaitFundingSignedData, remote: FundingSigned, WAIT_FUNDING_SIGNED)
        if remote.channelId == wait.channelId =>

        val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
          wait.remoteParams.fundingPubkey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

        if (Scripts.checkSpendable(signedLocalCommitTx).isEmpty) BECOME(wait, CLOSING) else {
          val localCommit = LocalCommit(index = 0, wait.localSpec, htlcTxsAndSigs = Nil, signedLocalCommitTx)
          val localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty)
          val remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty)
          val dummy = PrivateKey(data = Tools.random getBytes 32, compressed = true).toPoint

          val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit,
            remoteCommit = wait.remoteCommit, localChanges, remoteChanges, localNextHtlcId = 0,
            remoteNextHtlcId = 0, remoteNextCommitInfo = Right(dummy), wait.localCommitTx.input,
            remotePerCommitmentSecrets = ShaHashesWithIndex(Map.empty, None), wait.channelId)

          BECOME(me STORE WaitFundingDoneData(wait.announce, our = None,
            their = None, wait.fundingTx, commitments), WAIT_FUNDING_DONE)
        }


      // FUNDING TX IS BROADCASTED AT THIS POINT


      // We have not yet sent a FundingLocked but just got one from them so we save it and keep waiting
      case (wait @ WaitFundingDoneData(_, None, _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>
        me UPDATE wait.copy(their = Some apply their)


      // We have already sent them a FundingLocked and now we got one from them so we can enter normal state now
      case (wait @ WaitFundingDoneData(_, Some(our), _, _, _), their: FundingLocked, WAIT_FUNDING_DONE)
        if their.channelId == wait.commitments.channelId =>
        BECOMENormal(wait, their)


      // We got our lock but their is not yet present so we save ours and just keep waiting for their
      case (wait @ WaitFundingDoneData(_, _, None, _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        val d1 = me STORE wait.copy(our = Some apply our)
        me UPDATE d1 SEND our


      // We got our lock when their is already present so we can safely enter normal state now
      case (wait @ WaitFundingDoneData(_, _, Some(their), _, _), CMDConfirmed(tx), WAIT_FUNDING_DONE)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        BECOMENormal(wait, their) SEND our


      // NORMAL MODE


      // TODO: remove this
      case (norm @ NormalData(_, commitments, None, None), remote: AnnouncementSignatures, NORMAL) =>
        val (localNodeSig, localBitcoinSig) = Announcements.signChannelAnnouncement(remote.shortChannelId,
          LNParams.nodePrivateKey, norm.announce.nodeId, commitments.localParams.fundingPrivKey,
          commitments.remoteParams.fundingPubkey, LNParams.globalFeatures)

        me SEND AnnouncementSignatures(commitments.channelId,
          remote.shortChannelId, localNodeSig, localBitcoinSig)


      case (norm: NormalData, add: UpdateAddHtlc, NORMAL)
        if add.channelId == norm.commitments.channelId =>

        // Should check if we have enough blocks to fulfill this HTLC
        val c1 = Commitments.receiveAdd(norm.commitments, add, LNParams.expiry)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fulfill: UpdateFulfillHtlc, NORMAL)
        if fulfill.channelId == norm.commitments.channelId =>

        // Got a fulfill for an outgoing HTLC we sent them earlier
        val c1 = Commitments.receiveFulfill(norm.commitments, fulfill)
        me UPDATE norm.copy(commitments = c1)


      case (norm: NormalData, fail: FailHtlc, NORMAL)
        if fail.channelId == norm.commitments.channelId =>

        // Got a failure for an outgoing HTLC we sent earlier
        val c1 = Commitments.receiveFail(norm.commitments, fail)
        me UPDATE norm.copy(commitments = c1)


      // We only can add new HTLCs when mutual shutdown process is not active
      case (norm @ NormalData(_, commitments, None, None), cmd: CMDAddHtlc, NORMAL) =>
        val c1 ~ updateAddHtlc = Commitments.sendAdd(commitments, cmd)

        LNParams.bag getPaymentInfo updateAddHtlc.paymentHash match {
          case Success(out: OutgoingPayment) if out.isFulfilled => throw ExtendedException(cmd)
          case Success(out: OutgoingPayment) if out.isPending => throw ExtendedException(cmd)
          case Success(_: IncomingPayment) => throw ExtendedException(cmd)

          case _ =>
            // This may be a failed outgoing payment
            // which probably means we try to re-use a failed request
            // this is fine, but such a request should not be stored twice
            me UPDATE norm.copy(commitments = c1) SEND updateAddHtlc
            doProcess(CMDProceed)
        }

      // We're fulfilling an HTLC we got earlier
      case (norm: NormalData, cmd: CMDFulfillHtlc, NORMAL) =>
        val c1 ~ updateFulfillHtlc = Commitments.sendFulfill(norm.commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFulfillHtlc


      // Failing an HTLC we got earlier
      case (norm: NormalData, cmd: CMDFailHtlc, NORMAL) =>
        val c1 ~ updateFailHtlc = Commitments.sendFail(norm.commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailHtlc


      case (norm: NormalData, cmd: CMDFailMalformedHtlc, NORMAL) =>
        val c1 ~ updateFailMalformedHtlс = Commitments.sendFailMalformed(norm.commitments, cmd)
        me UPDATE norm.copy(commitments = c1) SEND updateFailMalformedHtlс


      // GUARD: We can send a commit sig
      case (norm: NormalData, CMDProceed, NORMAL)
        if Commitments.localHasChanges(norm.commitments) &&
          norm.commitments.remoteNextCommitInfo.isRight =>

        // Propose new remote commit via commit tx sig
        val nextRemotePoint = norm.commitments.remoteNextCommitInfo.right.get
        val c1 ~ commitSig = Commitments.sendCommit(norm.commitments, nextRemotePoint)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND commitSig


      // GUARD: serves as a trigger to enter negotiations when shutdown mode is active
      case (norm @ NormalData(_, commitments, Some(local), Some(remote)), CMDProceed, NORMAL)
        if Commitments.pendingHtlcs(commitments).isEmpty =>

        // Send a message and enter negotiations mode right away
        startNegotiations(norm.announce, commitments, local, remote)


      case (norm: NormalData, sig: CommitSig, NORMAL)
        if sig.channelId == norm.commitments.channelId =>

        // We received a commit sig from them, now we can update our local commit
        val c1 ~ revokeAndAck = Commitments.receiveCommit(norm.commitments, sig)
        val d1 = me STORE norm.copy(commitments = c1)
        me UPDATE d1 SEND revokeAndAck
        doProcess(CMDProceed)


      case (norm: NormalData, rev: RevokeAndAck, NORMAL)
        if rev.channelId == norm.commitments.channelId =>

        // We received a revocation because we sent a commit sig
        val c1 = Commitments.receiveRevocation(norm.commitments, rev)
        me UPDATE norm.copy(commitments = c1)
        doProcess(CMDHTLCProcess)

      // Fail or fulfill incoming HTLCs
      case (norm: NormalData, CMDHTLCProcess, NORMAL) =>
        for (Htlc(false, add) <- norm.commitments.remoteCommit.spec.htlcs)
          me doProcess resolveHtlc(LNParams.nodePrivateKey, add, LNParams.bag)

        // And sign right away
        doProcess(CMDProceed)


      case (norm: NormalData, CMDFeerate(rate), NORMAL)
        // GUARD: only send fee updates if the fee gap between nodes is large enough
        if LNParams.shouldUpdateFee(norm.commitments.localCommit.spec.feeratePerKw, rate) =>

        // Periodic fee updates to ensure commit txs could be confirmed
        val c1 ~ updateFeeMessage = Commitments.sendFee(norm.commitments, rate)
        me UPDATE norm.copy(commitments = c1) SEND updateFeeMessage
        doProcess(CMDProceed)


      // NORMAL: SHUTDOWN


      // Sending CMDShutdown when mutual shutdown is already in progress means we want uncooperative close
      case (norm: NormalData, CMDShutdown, NORMAL) if norm.localShutdown.isDefined => startLocalCurrentClose(norm)
      case (norm: NormalData, CMDShutdown, NORMAL) if norm.remoteShutdown.isDefined => startLocalCurrentClose(norm)
      case (norm @ NormalData(_, _, None, None), CMDShutdown, NORMAL) => me startShutdown norm


      // They try to shutdown with uncommited changes
      case (norm: NormalData, remote: Shutdown, NORMAL)
        if remote.channelId == norm.commitments.channelId &&
          Commitments.remoteHasChanges(norm.commitments) =>

        // Can't start mutual shutdown
        startLocalCurrentClose(norm)


      // We have not yet sent or received a shutdown so send one and re-call again
      case (norm @ NormalData(_, commitments, None, None), remote: Shutdown, NORMAL)
        if remote.channelId == commitments.channelId =>

        me startShutdown norm
        doProcess(remote)


      // We have already sent a shutdown (initially or in response to their shutdown above)
      case (norm @ NormalData(announce, commitments, Some(local), None), remote: Shutdown, NORMAL)
        if remote.channelId == commitments.channelId =>

        // We can start negotiations if there are no in-flight HTLCs, otherwise wait until they are cleared
        if (Commitments.pendingHtlcs(commitments).isEmpty) startNegotiations(announce, commitments, local, remote)
        else me UPDATE norm.copy(remoteShutdown = Some apply remote)


      case (norm: NormalData, CMDBestHeight(height), NORMAL | SYNC)
        // GUARD: we have to always watch for expired HTLCs and close a chan if they exist
        if Commitments.pendingHtlcs(norm.commitments).exists(_.add.expiry <= height) =>
        startLocalCurrentClose(norm)


      // SYNC MODE


      // We may get this message any time so just save it here
      case (wait: WaitFundingDoneData, CMDConfirmed(tx), SYNC)
        if wait.fundingTx.txid == tx.txid =>

        val our = makeFundingLocked(wait.commitments)
        me UPDATE wait.copy(our = Some apply our)


      // We're exiting a sync state but don't have enough locks so we keep waiting
      case (wait: WaitFundingDoneData, ChannelReestablish(channelId, 1, 0), SYNC)
        if channelId == wait.commitments.channelId =>

        BECOME(wait, WAIT_FUNDING_DONE)
        wait.our foreach SEND


      case (neg: NegotiationsData, channelReestablish: ChannelReestablish, SYNC)
        if channelReestablish.channelId == neg.commitments.channelId =>
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

        def maybeResendRevocation = if (c1.localCommit.index == cr.nextRemoteRevocationNumber + 1) {
          val localPerCommitmentSecret = Generators.perCommitSecret(c1.localParams.shaSeed, c1.localCommit.index - 1)
          val localNextPerCommitmentPoint = Generators.perCommitPoint(c1.localParams.shaSeed, c1.localCommit.index + 1)
          me SEND RevokeAndAck(channelId = c1.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
        } else if (c1.localCommit.index != cr.nextRemoteRevocationNumber) throw ExtendedException(norm)

        c1.remoteNextCommitInfo match {
          // We had sent a new sig and were waiting for their revocation
          // they didn't receive the new sig because of the disconnection
          // we resend the same updates and sig, also be careful about revocation
          case Left(wait) if wait.nextRemoteCommit.index == cr.nextLocalCommitmentNumber =>
            val revocationWasSentLast = c1.localCommit.index > wait.localCommitIndexSnapshot

            if (!revocationWasSentLast) maybeResendRevocation
            c1.localChanges.signed :+ wait.sent foreach SEND
            if (revocationWasSentLast) maybeResendRevocation

          // We had sent a new sig and were waiting for their revocation, they had received
          // the new sig but their revocation was lost during the disconnection, they'll resend us the revocation
          case Left(wait) if wait.nextRemoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case Right(_) if c1.remoteCommit.index + 1 == cr.nextLocalCommitmentNumber => maybeResendRevocation
          case _ => throw ExtendedException(norm)
        }

        BECOME(norm.copy(commitments = c1), NORMAL)
        norm.localShutdown foreach SEND
        doProcess(CMDHTLCProcess)


      // We just close a channel in any kind of irregular state
      case (some: HasCommitments, cr: ChannelReestablish, SYNC)
        if cr.channelId == some.commitments.channelId =>
        startLocalCurrentClose(some)


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
        lazy val (_, nextMessage) = Closing.makeClosing(neg.commitments,
          localScript, remoteScript, nextCloseFee)

        closeTxOpt match {
          case Some(closeTx) if closeFeeSat == remoteFeeSat => startMutualClose(neg, closeTx)
          case Some(closeTx) if nextCloseFee == remoteFeeSat => startMutualClose(neg, closeTx)
          case Some(_) => me UPDATE neg.copy(localClosingSigned = nextMessage) SEND nextMessage
          case _ => throw ExtendedException(neg)
        }


      // HANDLE FUNDING SPENT


      case (norm: HasCommitments, CMDSpent(spendTx), _)
        // GUARD: something which spends our funding is broadcasted, must react
        if spendTx.txIn.exists(_.outPoint == norm.commitments.commitInput.outPoint) =>
        defineClosingAction(norm, spendTx)


      // HANDLE INITIALIZATION


      case (null, init: InitData, null) => BECOME(init, WAIT_FOR_INIT)
      case (null, closing: ClosingData, null) => BECOME(closing, CLOSING)
      case (null, wait: WaitFundingDoneData, null) => BECOME(wait, SYNC)
      case (null, negs: NegotiationsData, null) => BECOME(negs, SYNC)
      case (null, norm: NormalData, null) => BECOME(norm, SYNC)


      // MISC


      case (some, CMDShutdown, WAIT_FOR_INIT | WAIT_FOR_ACCEPT | WAIT_FOR_FUNDING | WAIT_FUNDING_SIGNED) => BECOME(some, CLOSING)
      case (some: HasCommitments, CMDShutdown, WAIT_FUNDING_DONE | NEGOTIATIONS | SYNC) => startLocalCurrentClose(some)
      case (_: NormalData, add: PlainAddHtlc, SYNC) => throw PlainAddInSyncException(add)

      case _ =>
        // Let know if received an unhandled message
        Tools log s"Channel: unhandled $state : $change"
    }

    // After this change has been processed
    events onProcess Tuple3(me, data, change)
  }

  private def makeFundingLocked(cs: Commitments) = {
    val point = Generators.perCommitPoint(cs.localParams.shaSeed, index = 1)
    FundingLocked(cs.channelId, nextPerCommitmentPoint = point)
  }

  private def BECOMENormal(wait: HasCommitments, their: FundingLocked) = {
    val c1 = wait.commitments.copy(remoteNextCommitInfo = Right apply their.nextPerCommitmentPoint)
    BECOME(me STORE NormalData(wait.announce, c1), NORMAL)
  }

  private def startShutdown(norm: NormalData) = {
    val localShutdown = Shutdown(norm.commitments.channelId, norm.commitments.localParams.defaultFinalScriptPubKey)
    me UPDATE norm.copy(localShutdown = Some apply localShutdown) SEND localShutdown
  }

  private def startNegotiations(announce: NodeAnnouncement, cs: Commitments, local: Shutdown, remote: Shutdown) = {
    val firstSigned = Closing.makeFirstClosing(cs, local.scriptPubKey, remote.scriptPubKey, cs.localCommit.spec.feeratePerKw)
    BECOME(me STORE NegotiationsData(announce, cs, firstSigned, local, remote), NEGOTIATIONS) SEND firstSigned
  }

  private def startMutualClose(neg: NegotiationsData, closeTx: Transaction) =
    BECOME(me STORE ClosingData(neg.announce, neg.commitments, closeTx :: Nil), CLOSING)

  private def startLocalCurrentClose(some: HasCommitments) =
    // Something went wrong and we decided to spend our current commit transaction
    Closing.claimCurrentLocalCommitTxOutputs(some.commitments, some.commitments.localCommit.commitTx.tx, LNParams.bag) -> some match {
      case (claim: LocalCommitPublished, closing: ClosingData) => BECOME(data1 = me STORE closing.copy(localCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(data1 = me STORE ClosingData(some.announce, some.commitments, localCommit = claim :: Nil), CLOSING)
    }

  private def defineClosingAction(some: HasCommitments, tx: Transaction) =
    // We are not sure what kind of closing transaction this so finding out
    some.commitments.remoteNextCommitInfo.left.map(_.nextRemoteCommit) match {
      case Left(remoteCommit) if remoteCommit.txid == tx.txid => startRemoteNextClose(some, remoteCommit)
      case _ if some.commitments.remoteCommit.txid == tx.txid => startRemoteCurrentClose(some)
      case _ => startOtherClose(some, tx)
    }

  private def startRemoteCurrentClose(some: HasCommitments) =
    // They've decided to spend their CURRENT commit tx, we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, some.commitments.remoteCommit, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me STORE closing.copy(remoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, remoteCommit = claim :: Nil), CLOSING)
    }

  private def startRemoteNextClose(some: HasCommitments, nextRemoteCommit: RemoteCommit) =
    // They've decided to spend their NEXT commit tx, once again we need to take what's ours
    Closing.claimRemoteCommitTxOutputs(some.commitments, nextRemoteCommit, LNParams.bag) -> some match {
      case (claim: RemoteCommitPublished, closing: ClosingData) => BECOME(me STORE closing.copy(nextRemoteCommit = claim :: Nil), CLOSING)
      case (claim, _) => BECOME(me STORE ClosingData(some.announce, some.commitments, nextRemoteCommit = claim :: Nil), CLOSING)
    }

  private def startOtherClose(some: HasCommitments, tx: Transaction) =
    // They may have spent a revoked transaction so we can maybe take all the money
    Closing.claimRevokedRemoteCommitTxOutputs(commitments = some.commitments, tx) -> some match {
      case (Some(claim), close: ClosingData) => BECOME(me STORE close.modify(_.revokedCommits).using(claim +: _), CLOSING)
      case (Some(claim), _) => BECOME(me STORE ClosingData(some.announce, some.commitments, revokedCommits = claim :: Nil), CLOSING)
      case (None, close: ClosingData) if close.mutualClose.exists(_.txid == tx.txid) => Tools log "Disregarding mutual closing tx"
      case (None, neg: NegotiationsData) => startMutualClose(neg, tx)
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
  def reloadOnBecome(chan: Channel): Unit = {
    val trans = Tuple4(chan, chan.data, null, chan.state)
    if (onBecome isDefinedAt trans) onBecome(trans)
  }

  type Incoming = (Channel, ChannelData, Any)
  type Transition = (Channel, ChannelData, String, String)
  def onBecome: PartialFunction[Transition, Unit] = none
  def onProcess: PartialFunction[Incoming, Unit] = none
  def onError: PartialFunction[Throwable, Unit] = none
}