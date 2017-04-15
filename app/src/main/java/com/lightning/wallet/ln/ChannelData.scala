package com.lightning.wallet.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.Exceptions._

import com.lightning.wallet.ln.Tools.{LightningMessages, random}
import com.lightning.wallet.ln.crypto.{Generators, OnionPacket, ShaChain, ShaHashesWithIndex}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi, Transaction}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.PaymentRoute
import com.lightning.wallet.ln.MSat.satFactor


sealed trait Command

case class CMDOpenChannel(temporaryChannelId: BinaryData, fundingSatoshis: Long,
                          pushMsat: Long, initialFeeratePerKw: Long, localParams: LocalParams,
                          remoteInit: Init) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData,
                                failureCode: Int) extends Command

case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command
case class CMDUpdateFee(feeratePerKw: Long) extends Command

// STORABLE CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
sealed trait HasLastSent { val lastSent: LightningMessage }
sealed trait HasCommitments { val commitments: Commitments }

case class InitData(announce: NodeAnnouncement) extends ChannelData
case class ErrorData(announce: NodeAnnouncement, error: BinaryData) extends ChannelData

case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel,
                          lastSent: OpenChannel) extends ChannelData with HasLastSent

case class WaitFundingTxData(announce: NodeAnnouncement, cmd: CMDOpenChannel, remoteFirstPerCommitmentPoint: Point,
                             remoteParams: RemoteParams, lastSent: OpenChannel) extends ChannelData with HasLastSent

case class WaitFundingSignedData(announce: NodeAnnouncement, channelId: BinaryData, localParams: LocalParams, remoteParams: RemoteParams,
                                 fundingTx: Transaction, localSpec: CommitmentSpec, localCommitTx: CommitTx, remoteCommit: RemoteCommit,
                                 lastSent: FundingCreated) extends ChannelData with HasLastSent

case class WaitFundingConfirmedData(announce: NodeAnnouncement, commitments: Commitments, their: Option[FundingLocked],
                                    lastSent: LightningMessage) extends ChannelData with HasLastSent with HasCommitments

case class NormalData(announce: NodeAnnouncement, commitments: Commitments,
                      afterCommit: Option[Any] = None) extends ChannelData with HasCommitments

case class ShutdownData(announce: NodeAnnouncement, commitments: Commitments, localShutdown: Shutdown,
                        remoteShutdown: Shutdown) extends ChannelData with HasCommitments

case class NegotiationsData(announce: NodeAnnouncement, commitments: Commitments, localClosingSigned: ClosingSigned,
                            localShutdown: Shutdown, remoteShutdown: Shutdown) extends ChannelData with HasCommitments

// COMMITMENTS

case class LocalParams(chainHash: BinaryData, dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long,
                       channelReserveSatoshis: Long, htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                       fundingPrivKey: PrivateKey, revocationSecret: Scalar, paymentKey: PrivateKey, delayedPaymentKey: Scalar,
                       defaultFinalScriptPubKey: BinaryData, shaSeed: BinaryData, isFunder: Boolean,
                       globalFeatures: BinaryData, localFeatures: BinaryData)

case class RemoteParams(dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long, channelReserveSatoshis: Long,
                        htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubKey: PublicKey,
                        revocationBasepoint: Point, paymentBasepoint: Point, delayedPaymentBasepoint: Point,
                        globalFeatures: BinaryData, localFeatures: BinaryData)

case class LocalCommitPublished(claimMainDelayedOutputTx: Option[Transaction], htlcSuccessTxs: Seq[Transaction],
                                htlcTimeoutTxs: Seq[Transaction], claimHtlcSuccessTxs: Seq[Transaction],
                                claimHtlcTimeoutTxs: Seq[Transaction], commitTx: Transaction)

case class RemoteCommitPublished(claimMainOutputTx: Option[Transaction], claimHtlcSuccessTxs: Seq[Transaction],
                                 claimHtlcTimeoutTxs: Seq[Transaction], commitTx: Transaction)

case class RevokedCommitPublished(claimMainOutputTx: Option[Transaction], mainPenaltyTx: Option[Transaction],
                                  claimHtlcTimeoutTxs: Seq[Transaction], htlcTimeoutTxs: Seq[Transaction],
                                  htlcPenaltyTxs: Seq[Transaction], commitTx: Transaction)

case class CommitmentSpec(feeratePerKw: Long, toLocalMsat: Long, toRemoteMsat: Long,
                          htlcs: Set[Htlc] = Set.empty, fulfilled: Set[Htlc] = Set.empty,
                          failed: Set[Htlc] = Set.empty)

case class PublishableTxs(htlcTxsAndSigs: Seq[HtlcTxAndSigs], commitTx: CommitTx)
case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: BinaryData, remoteSig: BinaryData)
case class LocalCommit(index: Long, spec: CommitmentSpec, publishableTxs: PublishableTxs, commit: CommitSig)
case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: BinaryData, remotePerCommitmentPoint: Point)
case class Changes(proposed: LightningMessages, signed: LightningMessages, acked: LightningMessages)
case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig)

case class Commitments(localParams: LocalParams, remoteParams: RemoteParams,
                       localCommit: LocalCommit, remoteCommit: RemoteCommit,
                       localChanges: Changes, remoteChanges: Changes,
                       localNextHtlcId: Long, remoteNextHtlcId: Long,
                       remoteNextCommitInfo: Either[WaitingForRevocation, Point],
                       unackedMessages: LightningMessages, commitInput: InputInfo,
                       remotePerCommitmentSecrets: ShaHashesWithIndex,
                       channelId: BinaryData)

// PAYMENT REQUEST

case class Invoice(message: Option[String], nodeId: BinaryData,
                   sum: MilliSatoshi, paymentHash: BinaryData)

case class ExtendedInvoice(preimage: Option[BinaryData], fee: Option[Long],
                           invoice: Invoice, status: Long, incoming: Boolean, stamp: Long)

case class PaymentRouteOps(remaining: List[PaymentRoute], targetNodeId: PublicKey)
case class Htlc(incoming: Boolean, add: UpdateAddHtlc, routes: PaymentRouteOps,
                onion: OnionPacket, sort: String) extends ChannelMessage

// CONTROL OBJECTS

object Invoice {
  def serialize(inv: Invoice) = {
    val hash = inv.paymentHash.toString
    val node = inv.nodeId.toString
    val sum = inv.sum.amount
    s"$node:$sum:$hash"
  }

  def parse(raw: String) = {
    val Array(node, sum, hash) = raw.split(':')
    Invoice(None, node, MilliSatoshi(sum.toLong), hash)
  }
}

object Htlc {
  val PLAIN = "plain"
  val SILENT = "silent"
}

object Changes {
  def all(c: Changes): LightningMessages =
    c.proposed ++ c.signed ++ c.acked
}

object PaymentRouteOps {
  private def without(ops: PaymentRouteOps, predicate: Hop => Boolean) = ops.remaining.tail.filterNot(_ exists predicate)
  def withoutFailedChannel(ops: PaymentRouteOps, chanId: Long) = without(ops, _.lastUpdate.shortChannelId == chanId)
  def withoutFailedNode(ops: PaymentRouteOps, nodeId: BinaryData) = without(ops, _.nodeId == nodeId)
}

object CommitmentSpec {
  def findHtlcById(cs: CommitmentSpec, id: Long, isIncoming: Boolean): Option[Htlc] =
    cs.htlcs.find(htlc => htlc.add.id == id && htlc.incoming == isIncoming)

  def totalFunds(cs: CommitmentSpec): Long =
    cs.htlcs.toSeq.map(_.add.amountMsat).sum +
      cs.toLocalMsat + cs.toRemoteMsat

  private def add(cs: CommitmentSpec, htlc: Htlc) = htlc.incoming match {
    case true => cs.copy(htlcs = cs.htlcs + htlc, toRemoteMsat = cs.toRemoteMsat - htlc.add.amountMsat)
    case false => cs.copy(htlcs = cs.htlcs + htlc, toLocalMsat = cs.toLocalMsat - htlc.add.amountMsat)
  }

  private def fulfill(cs: CommitmentSpec, isIncoming: Boolean, update: UpdateFulfillHtlc) = findHtlcById(cs, update.id, isIncoming) match {
    case Some(x) if x.incoming => cs.copy(toLocalMsat = cs.toLocalMsat + x.add.amountMsat, htlcs = cs.htlcs - x, fulfilled = cs.fulfilled + x)
    case Some(x) => cs.copy(toRemoteMsat = cs.toRemoteMsat + x.add.amountMsat, htlcs = cs.htlcs - x, fulfilled = cs.fulfilled + x)
    case None => cs
  }

  private def fail(cs: CommitmentSpec, isIncoming: Boolean, update: UpdateFailHtlc) = findHtlcById(cs, update.id, isIncoming) match {
    case Some(x) if x.incoming => cs.copy(toRemoteMsat = cs.toRemoteMsat + x.add.amountMsat, htlcs = cs.htlcs - x, failed = cs.failed + x)
    case Some(x) => cs.copy(toLocalMsat = cs.toLocalMsat + x.add.amountMsat, htlcs = cs.htlcs - x, failed = cs.failed + x)
    case None => cs
  }

  def reduce(cs: CommitmentSpec, localChanges: LightningMessages, remoteChanges: LightningMessages) = {
    // On each reduce we collect failed and fulfilled HTLCs, we can use that info later to update our views

    val spec0 = cs.copy(fulfilled = Set.empty, failed = Set.empty)
    val spec1 = (spec0 /: localChanges) { case (spec, htlc: Htlc) => add(spec, htlc) case (spec, _) => spec }
    val spec2 = (spec1 /: remoteChanges) { case (spec, htlc: Htlc) => add(spec, htlc) case (spec, _) => spec }

    val spec3 = localChanges.foldLeft(spec2) {
      case (spec, msg: UpdateFulfillHtlc) => fulfill(spec, isIncoming = true, msg)
      case (spec, msg: UpdateFailHtlc) => fail(spec, isIncoming = true, msg)
      case (spec, _) => spec
    }

    val spec4 = remoteChanges.foldLeft(spec3) {
      case (spec, msg: UpdateFulfillHtlc) => fulfill(spec, isIncoming = false, msg)
      case (spec, msg: UpdateFailHtlc) => fail(spec, isIncoming = false, msg)
      case (spec, _) => spec
    }

    val spec5 = (localChanges ++ remoteChanges).foldLeft(spec4) {
      case (spec, u: UpdateFee) => spec.copy(feeratePerKw = u.feeratePerKw)
      case (spec, _) => spec
    }

    spec5
  }
}

object UnackedOps {
  def getUnackedShutdown(ms: LightningMessages) = ms.collectFirst { case down: Shutdown => down }
  def cutAcked(ms: LightningMessages) = ms.drop(ms.indexWhere { case _: CommitSig => true case _ => false } + 1)
  def replaceRevoke(ms: LightningMessages, r: RevokeAndAck) = ms.filterNot { case _: RevokeAndAck => true case _ => false } :+ r
}

object Commitments {
  def hasNoPendingHtlcs(c: Commitments): Boolean = c.remoteNextCommitInfo match {
    case Right(_) => c.localCommit.spec.htlcs.isEmpty && c.remoteCommit.spec.htlcs.isEmpty
    case Left(wait) => c.localCommit.spec.htlcs.isEmpty && c.remoteCommit.spec.htlcs.isEmpty &&
      wait.nextRemoteCommit.spec.htlcs.isEmpty
  }

  def hasTimedoutOutgoingHtlcs(c: Commitments, blockHeight: Long): Boolean =
    c.localCommit.spec.htlcs.exists(htlc => !htlc.incoming && blockHeight >= htlc.add.expiry) ||
      c.remoteCommit.spec.htlcs.exists(htlc => htlc.incoming && blockHeight >= htlc.add.expiry)

  def addLocalProposal(c: Commitments, proposal: LightningMessage): Commitments =
    c.modifyAll(_.unackedMessages, _.localChanges.proposed).using(_ :+ proposal)

  def addRemoteProposal(c: Commitments, proposal: LightningMessage): Commitments =
    c.modify(_.remoteChanges.proposed).using(_ :+ proposal)

  def localHasChanges(c: Commitments): Boolean = c.remoteChanges.acked.nonEmpty || c.localChanges.proposed.nonEmpty
  def remoteHasChanges(c: Commitments): Boolean = c.localChanges.acked.nonEmpty || c.remoteChanges.proposed.nonEmpty
  def revocationPreimage(seed: BinaryData, index: Long) = ShaChain.shaChainFromSeed(seed, ShaChain.largestIndex - index)
  def revocationHash(seed: BinaryData, index: Long): BinaryData = Crypto sha256 revocationPreimage(seed, index)

  def getHtlcCrossSigned(commitments: Commitments, incomingRelativeToLocal: Boolean, htlcId: Long) = {
    val remoteSigned = CommitmentSpec.findHtlcById(commitments.localCommit.spec, htlcId, incomingRelativeToLocal)

    val localSigned = commitments.remoteNextCommitInfo match {
      case Left(wait) => CommitmentSpec.findHtlcById(wait.nextRemoteCommit.spec, htlcId, !incomingRelativeToLocal)
      case _ => CommitmentSpec.findHtlcById(commitments.remoteCommit.spec, htlcId, !incomingRelativeToLocal)
    }

    for {
      htlcOut <- remoteSigned
      htlcIn <- localSigned
    } yield htlcIn.add
  }

  def sendAdd(c: Commitments, htlc: Htlc, blockCount: Int) =
    if (htlc.add.expiry <= blockCount) throw DetailedException(HTLC_EXPIRY_TOO_SOON, FinalExpiryTooSoon)
    else if (htlc.add.amountMsat < c.remoteParams.htlcMinimumMsat) throw DetailedException(HTLC_VALUE_TOO_SMALL, PermanentChannelFailure)
    else {

      val c1 = addLocalProposal(c, htlc).copy(localNextHtlcId = c.localNextHtlcId + 1)
      // Let's compute the current commitment *as seen by them* with this change taken into account
      val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.remoteParams.dustLimitSatoshis), reduced).amount else 0

      // The rest of the guards
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.remoteParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.remoteParams.maxAcceptedHtlcs

      if (htlcValueInFlightOverflow) throw DetailedException(HTLC_TOO_MUCH_VALUE_IN_FLIGHT, TemporaryChannelFailure)
      else if (acceptedHtlcsOverflow) throw DetailedException(HTLC_TOO_MANY_ACCEPTED, TemporaryChannelFailure)
      else if (feesOverflow) throw DetailedException(HTLC_MISSING_FEES, TemporaryChannelFailure)
      else c1
    }

  def receiveAdd(c: Commitments, add: UpdateAddHtlc, blockCount: Int) =

    if (add.id < c.remoteNextHtlcId) c
    else if (add.amountMsat < c.localParams.htlcMinimumMsat) throw ChannelException(HTLC_VALUE_TOO_SMALL)
    else if (add.expiry < blockCount + 6) throw ChannelException(HTLC_EXPIRY_TOO_SOON)
    else if (add.id != c.remoteNextHtlcId) throw ChannelException(HTLC_UNEXPECTED_ID)
    else {

      // Let's compute the current commitment *as seen by us* including this change
      val c1 = addRemoteProposal(c, add).copy(remoteNextHtlcId = c.remoteNextHtlcId + 1)
      val reduced = CommitmentSpec.reduce(c1.localCommit.spec, c1.localChanges.acked, c1.remoteChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.localParams.dustLimitSatoshis), reduced).amount else 0

      // The rest of the guards
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.localParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.localParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.localParams.maxAcceptedHtlcs

      if (htlcValueInFlightOverflow) throw ChannelException(HTLC_TOO_MUCH_VALUE_IN_FLIGHT)
      else if (acceptedHtlcsOverflow) throw ChannelException(HTLC_TOO_MANY_ACCEPTED)
      else if (feesOverflow) throw ChannelException(HTLC_MISSING_FEES)
      else c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val fulfill = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = true, htlcId = cmd.id) match {
      case Some(htlc) if sha256(cmd.preimage) == htlc.paymentHash => addLocalProposal(c, fulfill)
      case Some(htlc) => throw ChannelException(HTLC_INVALID_PREIMAGE)
      case _ => throw ChannelException(HTLC_UNKNOWN_PREIMAGE)
    }
  }

  def receiveFulfill(c: Commitments, fulfill: UpdateFulfillHtlc) = {
    val isOldFulfill: Boolean = Changes all c.remoteChanges contains fulfill
    if (isOldFulfill) c else getHtlcCrossSigned(c, incomingRelativeToLocal = false, fulfill.id) match {
      case Some(htlc) if sha256(fulfill.paymentPreimage) == htlc.paymentHash => addRemoteProposal(c, fulfill)
      case Some(htlc) => throw ChannelException(HTLC_INVALID_PREIMAGE)
      case _ => throw ChannelException(HTLC_UNKNOWN_PREIMAGE)
    }
  }

  def sendFail(c: Commitments, cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(c.channelId, cmd.id, cmd.reason)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw ChannelException(HTLC_UNKNOWN_PREIMAGE)
    else addLocalProposal(c, fail)
  }

  def sendFailMalformed(c: Commitments, cmd: CMDFailMalformedHtlc) = {
    val fail = UpdateFailMalformedHtlc(c.channelId, cmd.id, cmd.onionHash, cmd.failureCode)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw ChannelException(HTLC_UNEXPECTED_ID)
    else addLocalProposal(c, fail)
  }

  def receiveFail(c: Commitments, fail: FailHtlc) =
    if (Changes all c.remoteChanges contains fail) c else {
      val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
      if (found.isEmpty) throw ChannelException(HTLC_UNEXPECTED_ID)
      else addRemoteProposal(c, fail)
    }

  def sendFee(c: Commitments, cmd: CMDUpdateFee) = {
    val fee = UpdateFee(c.channelId, cmd.feeratePerKw)
    val c1 = addLocalProposal(c, fee)

    val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
    val fees = Scripts.commitTxFee(dustLimit = Satoshi(c1.remoteParams.dustLimitSatoshis), spec = reduced).amount
    val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
    if (feesOverflow) throw ChannelException(FEE_FUNDEE_CAN_NOT_PAY) else c1
  }

  def sendCommit(c: Commitments, remoteNextPerCommitmentPoint: Point) = {
    // Remote commitment will include all local proposed changes as well as remote acked changes
    val spec = CommitmentSpec.reduce(c.remoteCommit.spec, c.remoteChanges.acked, c.localChanges.proposed)
    val paymentKey = Generators.derivePrivKey(c.localParams.paymentKey, remoteNextPerCommitmentPoint)

    val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeRemoteTxs(c.remoteCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, remoteNextPerCommitmentPoint, spec)

    // Generate signatures
    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, paymentKey)

    // Update commitment data
    val commitSig = CommitSig(c.channelId, Scripts.sign(remoteCommitTx, c.localParams.fundingPrivKey), htlcSigs.toList)
    val remote1 = RemoteCommit(c.remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint)

    c.copy(remoteNextCommitInfo = Left apply WaitingForRevocation(remote1, commitSig),
      localChanges = c.localChanges.copy(proposed = Vector.empty, signed = c.localChanges.proposed),
      remoteChanges = c.remoteChanges.copy(acked = Vector.empty, signed = c.remoteChanges.acked),
      unackedMessages = c.unackedMessages :+ commitSig)
  }

  def receiveCommit(c: Commitments, commit: CommitSig) =
    c.localCommit.commit.hashCode == commit.hashCode match {
      case false if remoteHasChanges(c) => doReceiveCommit(c, commit)
      case false => throw ChannelException(COMMIT_RECEIVE_ATTEMPT_NO_CHANGES)
      case true => c
    }

  private def doReceiveCommit(c: Commitments, commit: CommitSig) = {
    val spec = CommitmentSpec.reduce(c.localCommit.spec, c.localChanges.acked, c.remoteChanges.proposed)
    val localPerCommitmentSecret = Generators.perCommitSecret(c.localParams.shaSeed, c.localCommit.index)
    val localPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 1)
    val localNextPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 2)
    val remotePaymentPubkey = Generators.derivePubKey(c.remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val localPaymentKey = Generators.derivePrivKey(c.localParams.paymentKey, localPerCommitmentPoint)
    val revocation = RevokeAndAck(c.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)

    // they sent us a signature for *their* view of *our* next commit tx
    // so in terms of rev.hashes and indexes we have:
    // ourCommit.index -> our current revocation hash, which is about to become our old revocation hash
    // ourCommit.index + 1 -> our next revocation hash, used by *them* to build the sig we've just received, and which
    // is about to become our current revocation hash
    // ourCommit.index + 2 -> which is about to become our next revocation hash
    // we will reply to this sig with our old revocation hash preimage (at index) and our next revocation hash (at index + 1)
    // and will increment our index

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeLocalTxs(c.localCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val signedCommitTx = Scripts.addSigs(localCommitTx, c.localParams.fundingPrivKey.publicKey,
      c.remoteParams.fundingPubKey, Scripts.sign(localCommitTx, c.localParams.fundingPrivKey),
      remoteSig = commit.signature)

    if (Scripts.checkSpendable(signedCommitTx).isFailure) throw ChannelException(COMMIT_RECEIVE_INVALID_SIGNATURE)
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw ChannelException(COMMIT_SIG_COUNT_MISMATCH)

    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localPaymentKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isFailure) throw ChannelException(COMMIT_RECEIVE_INVALID_SIGNATURE)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        val isSigValid = Scripts.checkSig(htlcTx, remoteSig, remotePaymentPubkey)
        if (!isSigValid) throw ChannelException(COMMIT_RECEIVE_INVALID_SIGNATURE)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    val unackedMessages1 = UnackedOps.replaceRevoke(c.unackedMessages, revocation)
    c.copy(localCommit = LocalCommit(c.localCommit.index + 1, spec, PublishableTxs(htlcTxsAndSigs, signedCommitTx), commit),
      remoteChanges = c.remoteChanges.copy(proposed = Vector.empty, acked = c.remoteChanges.acked ++ c.remoteChanges.proposed),
      localChanges = c.localChanges.copy(acked = Vector.empty), unackedMessages = unackedMessages1)
  }

  def receiveRevocation(c: Commitments, rev: RevokeAndAck) = c.remoteNextCommitInfo match {
    case Left(wait) if wait.nextRemoteCommit.remotePerCommitmentPoint == rev.nextPerCommitmentPoint => c
    case Left(_) if c.remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw ChannelException(REVOCATION_INVALID_PREIMAGE)

    case Left(wait: WaitingForRevocation) =>
      val nextIndex = ShaChain.largestTxIndex - c.remoteCommit.index
      val secrets1 = ShaChain.addHash(c.remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin, nextIndex)
      val lc1 = c.localChanges.copy(signed = Vector.empty, acked = c.localChanges.acked ++ c.localChanges.signed)

      c.copy(localChanges = lc1, remoteChanges = c.remoteChanges.copy(signed = Vector.empty),
        remoteCommit = wait.nextRemoteCommit, remoteNextCommitInfo = Right(rev.nextPerCommitmentPoint),
        unackedMessages = UnackedOps cutAcked c.unackedMessages, remotePerCommitmentSecrets = secrets1)

    case Right(point) if point == rev.nextPerCommitmentPoint => c
    case _ => throw ChannelException(REVOCATION_UNEXPECTED)
  }
}

trait InvoiceBag {
  def putPreimage(preimage: BinaryData): Unit
  def getExtendedInvoice(hash: BinaryData): Option[ExtendedInvoice]
  def newPreimage: BinaryData = BinaryData(random getBytes 32)
}