package com.lightning.wallet.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.Exceptions._

import com.lightning.wallet.ln.Tools.{random, LightningMessages}
import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi, Transaction}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.SeqPaymentRoute
import com.lightning.wallet.ln.crypto.Sphinx.OnionPacket
import com.lightning.wallet.ln.MSat.satFactor


sealed trait Command
case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, failureCode: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command
case class CMDUpdateFee(feeratePerKw: Long) extends Command


sealed trait ChannelData
sealed trait HasCommitments { val commitments: Commitments }
case class InitData(announce: NodeAnnouncement) extends ChannelData
case class ErrorData(announce: NodeAnnouncement, error: BinaryData) extends ChannelData

case class ChannelOpenData(temporaryChannelId: BinaryData, fundingSatoshis: Long, pushMsat: Long,
                           initialFeeratePerKw: Long, localParams: LocalParams, remoteInit: Init,
                           announce: NodeAnnouncement) extends ChannelData

case class WaitAcceptData(openData: ChannelOpenData, lastSent: OpenChannel) extends ChannelData
case class WaitFundingData(acceptData: WaitAcceptData, remoteFirstPerCommitmentPoint: Point,
                           remoteParams: RemoteParams) extends ChannelData


case class LocalParams(dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long, channelReserveSatoshis: Long,
                       htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPrivKey: PrivateKey, revocationSecret: Scalar,
                       paymentKey: PrivateKey, delayedPaymentKey: Scalar, defaultFinalScriptPubKey: BinaryData, shaSeed: BinaryData,
                       isFunder: Boolean, globalFeatures: BinaryData, localFeatures: BinaryData)

case class RemoteParams(dustLimitSatoshis: Long, maxHtlcValueInFlightMsat: Long, channelReserveSatoshis: Long,
                        htlcMinimumMsat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int, fundingPubKey: PublicKey, revocationBasepoint: Point,
                        paymentBasepoint: Point, delayedPaymentBasepoint: Point, globalFeatures: BinaryData, localFeatures: BinaryData)

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
case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, reSignAsap: Boolean = false)
case class Changes(proposed: LightningMessages, signed: LightningMessages, acked: LightningMessages)
case class PaymentRouteOps(remaining: SeqPaymentRoute, targetNodeId: PublicKey)

case class Commitments(localParams: LocalParams, remoteParams: RemoteParams,
                       localCommit: LocalCommit, remoteCommit: RemoteCommit,
                       localChanges: Changes, remoteChanges: Changes,
                       localNextHtlcId: Long, remoteNextHtlcId: Long,
                       remoteNextCommitInfo: Either[WaitingForRevocation, Point],
                       unackedMessages: LightningMessages, commitInput: InputInfo,
                       remotePerCommitmentSecrets: ShaHashesWithIndex,
                       channelId: BinaryData)

case class Invoice(message: Option[String], nodeId: BinaryData,
                   sum: MilliSatoshi, paymentHash: BinaryData)

case class ExtendedInvoice(preimage: Option[BinaryData], fee: Option[Long],
                           invoice: Invoice, status: Long, incoming: Boolean, stamp: Long)

case class Htlc(incoming: Boolean, add: UpdateAddHtlc, routes: PaymentRouteOps = null,
                onion: OnionPacket = null, sort: String = Htlc.PLAIN) extends ChannelMessage

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
  def withoutFailedChannel(ops: PaymentRouteOps, chanId: Long) = {
    def isBadHop(hop: Hop) = hop.lastUpdate.shortChannelId == chanId
    ops.remaining.tail.filterNot(_ exists isBadHop)
  }

  def withoutFailedNode(ops: PaymentRouteOps, nodeId: BinaryData) = {
    def isBadHop(hop: Hop) = hop.nodeId == nodeId || hop.nextNodeId == nodeId
    ops.remaining.tail.filterNot(_ exists isBadHop)
  }
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
  def ackShutdown(ms: LightningMessages) = ms.diff(getUnackedShutdown(ms).toList)
  def getUnackedShutdown(ms: LightningMessages) = ms.collectFirst { case down: Shutdown => down }
  def cutAcked(ms: LightningMessages) = ms.drop(ms.indexWhere { case _: CommitSig => true case _ => false } + 1)
  def replaceRevoke(ms: LightningMessages, r: RevokeAndAck) = ms.filterNot { case _: RevokeAndAck => true case _ => false } :+ r
}

object Commitments {
  def hasNoPendingHtlcs(c: Commitments): Boolean =
    c.localCommit.spec.htlcs.isEmpty && c.remoteCommit.spec.htlcs.isEmpty

  def hasTimedoutHtlcs(c: Commitments, blockHeight: Long): Boolean =
    c.localCommit.spec.htlcs.forall(_.add.expiry < blockHeight) ||
      c.remoteCommit.spec.htlcs.forall(_.add.expiry < blockHeight)

  def addLocalProposal(c: Commitments, proposal: LightningMessage): Commitments =
    c.modifyAll(_.unackedMessages, _.localChanges.proposed).using(_ :+ proposal)

  def addRemoteProposal(c: Commitments, proposal: LightningMessage): Commitments =
    c.modify(_.remoteChanges.proposed).using(_ :+ proposal)

  def addToUnackedMessages(c: Commitments, message: LightningMessage): Commitments =
    c.modify(_.unackedMessages).using(_ :+ message)

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

    if (htlc.add.expiry <= blockCount) Left(FinalExpiryTooSoon, HTLC_EXPIRY_TOO_SOON)
    else if (htlc.add.amountMsat < c.remoteParams.htlcMinimumMsat) Left(PermanentChannelFailure, HTLC_VALUE_TOO_SMALL)
    else {

      val c1 = addLocalProposal(c, htlc).copy(localNextHtlcId = c.localNextHtlcId + 1)
      // Let's compute the current commitment *as seen by them* with this change taken into account
      val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.remoteParams.dustLimitSatoshis), reduced).amount else 0

      // The rest of the guards
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.remoteParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.remoteParams.maxAcceptedHtlcs

      if (htlcValueInFlightOverflow) Left(TemporaryChannelFailure, HTLC_TOO_MUCH_VALUE_IN_FLIGHT)
      else if (acceptedHtlcsOverflow) Left(TemporaryChannelFailure, HTLC_TOO_MANY_ACCEPTED)
      else if (feesOverflow) Left(TemporaryChannelFailure, HTLC_MISSING_FEES)
      else Right(c1, htlc)
    }

  def receiveAdd(c: Commitments, add: UpdateAddHtlc, blockCount: Int) =

    if (add.id < c.remoteNextHtlcId) c
    else if (add.amountMsat < c.localParams.htlcMinimumMsat) throw new RuntimeException(HTLC_VALUE_TOO_SMALL)
    else if (add.expiry < blockCount + 6) throw new RuntimeException(HTLC_EXPIRY_TOO_SOON)
    else if (add.id != c.remoteNextHtlcId) throw new RuntimeException(HTLC_UNEXPECTED_ID)
    else {

      // Let's compute the current commitment *as seen by us* including this change
      val c1 = addRemoteProposal(c, add).copy(remoteNextHtlcId = c.remoteNextHtlcId + 1)
      val reduced = CommitmentSpec.reduce(c1.localCommit.spec, c1.localChanges.acked, c1.remoteChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.localParams.dustLimitSatoshis), reduced).amount else 0

      // The rest of the guards
      val htlcValueInFlightOverflow = reduced.htlcs.map(_.add.amountMsat).sum > c1.localParams.maxHtlcValueInFlightMsat
      val feesOverflow = reduced.toRemoteMsat / satFactor - c1.localParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.localParams.maxAcceptedHtlcs

      if (htlcValueInFlightOverflow) throw new RuntimeException(HTLC_TOO_MUCH_VALUE_IN_FLIGHT)
      else if (acceptedHtlcsOverflow) throw new RuntimeException(HTLC_TOO_MANY_ACCEPTED)
      else if (feesOverflow) throw new RuntimeException(HTLC_MISSING_FEES)
      else c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val fulfill = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id) match {
      case Some(htlc) if sha256(cmd.preimage) == htlc.paymentHash => addLocalProposal(c, fulfill) -> fulfill
      case Some(htlc) => throw ExRuntimeException(why = HTLC_INVALID_PREIMAGE, cmd.id)
      case _ => throw ExRuntimeException(HTLC_UNKNOWN_PREIMAGE, cmd.id)
    }
  }

  def receiveFulfill(c: Commitments, fulfill: UpdateFulfillHtlc) = {
    val isOldFulfill: Boolean = Changes.all(c.remoteChanges) contains fulfill
    if (isOldFulfill) c else getHtlcCrossSigned(c, incomingRelativeToLocal = false, fulfill.id) match {
      case Some(htlc) if sha256(fulfill.paymentPreimage) == htlc.paymentHash => addRemoteProposal(c, fulfill)
      case Some(htlc) => throw ExRuntimeException(why = HTLC_INVALID_PREIMAGE, plus = htlc)
      case _ => throw ExRuntimeException(HTLC_UNKNOWN_PREIMAGE, fulfill.id)
    }
  }

  def sendFail(c: Commitments, cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(c.channelId, cmd.id, cmd.reason)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw ExRuntimeException(HTLC_UNKNOWN_PREIMAGE, cmd.id)
    else addLocalProposal(c, fail) -> fail
  }

  def sendFailMalformed(c: Commitments, cmd: CMDFailMalformedHtlc) = {
    val fail = UpdateFailMalformedHtlc(c.channelId, cmd.id, cmd.onionHash, cmd.failureCode)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw ExRuntimeException(HTLC_UNEXPECTED_ID, cmd.id)
    else addLocalProposal(c, fail) -> fail
  }

  def receiveFail(c: Commitments, fail: FailHtlc) =
    if (Changes.all(c.remoteChanges) contains fail) c else {
      val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
      if (found.isEmpty) throw ExRuntimeException(HTLC_UNEXPECTED_ID, fail.id)
      else addRemoteProposal(c, fail)
    }

  def sendFee(c: Commitments, cmd: CMDUpdateFee) = {
    val fee = UpdateFee(c.channelId, cmd.feeratePerKw)
    val c1 = addLocalProposal(c, fee)

    val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
    val fees = Scripts.commitTxFee(dustLimit = Satoshi(c1.remoteParams.dustLimitSatoshis), spec = reduced).amount
    val feesOverflow = reduced.toRemoteMsat / satFactor - c1.remoteParams.channelReserveSatoshis - fees < 0
    if (feesOverflow) throw new RuntimeException(FEE_FUNDEE_CAN_NOT_PAY) else c1 -> fee
  }

  def sendCommit(c: Commitments) = c.remoteNextCommitInfo match {
    case Right(point) if localHasChanges(c) => doSendCommit(c, point)
    case Right(_) => throw new RuntimeException(COMMIT_SEND_ATTEMPT_NO_CHANGES)
    case Left(_) => throw new RuntimeException(COMMIT_ATTEMPT_NO_REVOCATION)
  }

  private def doSendCommit(c: Commitments, remoteNextPerCommitmentPoint: Point) = {
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
    val lc1 = c.localChanges.copy(proposed = Vector.empty, signed = c.localChanges.proposed)
    val rc1 = c.remoteChanges.copy(acked = Vector.empty, signed = c.remoteChanges.acked)
    val wait = WaitingForRevocation(remote1, commitSig)

    val c1 = c.copy(remoteNextCommitInfo = Left(wait), localChanges = lc1,
      remoteChanges = rc1, unackedMessages = c.unackedMessages :+ commitSig)

    c1 -> commitSig
  }

  def receiveCommit(c: Commitments, commit: CommitSig) = {
    val isOldCommit: Boolean = c.localCommit.commit == commit

    isOldCommit match {
      case false if remoteHasChanges(c) => doReceiveCommit(c, commit)
      case false => throw new RuntimeException(COMMIT_RECEIVE_ATTEMPT_NO_CHANGES)
      case true => Left(c)
    }
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
      commit.signature)

    if (Scripts.checkSpendable(signedCommitTx).isFailure) throw new RuntimeException(COMMIT_RECEIVE_INVALID_SIGNATURE)
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new RuntimeException(COMMIT_SIG_COUNT_MISMATCH)

    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localPaymentKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isFailure) throw new RuntimeException(COMMIT_RECEIVE_INVALID_SIGNATURE)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        val isSigValid = Scripts.checkSig(htlcTx, remoteSig, remotePaymentPubkey)
        if (!isSigValid) throw new RuntimeException(COMMIT_RECEIVE_INVALID_SIGNATURE)
        HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    val unackedMessages1 = UnackedOps.replaceRevoke(c.unackedMessages, revocation)
    val c1 = c.copy(localCommit = LocalCommit(c.localCommit.index + 1, spec, PublishableTxs(htlcTxsAndSigs, signedCommitTx), commit),
      remoteChanges = c.remoteChanges.copy(proposed = Vector.empty, acked = c.remoteChanges.acked ++ c.remoteChanges.proposed),
      localChanges = c.localChanges.copy(acked = Vector.empty), unackedMessages = unackedMessages1)

    Right(c1 -> revocation)
  }

  def receiveRevocation(c: Commitments, rev: RevokeAndAck) = c.remoteNextCommitInfo match {
    case Left(wait) if wait.nextRemoteCommit.remotePerCommitmentPoint == rev.nextPerCommitmentPoint => Left(c)
    case Left(_) if c.remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw new RuntimeException(REVOCATION_INVALID_PREIMAGE)

    case Left(wait: WaitingForRevocation) =>
      val nextIndex = ShaChain.largestTxIndex - c.remoteCommit.index
      val secrets1 = ShaChain.addHash(c.remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin, nextIndex)
      val lc1 = c.localChanges.copy(signed = Vector.empty, acked = c.localChanges.acked ++ c.localChanges.signed)

      val c1 = c.copy(localChanges = lc1, remoteChanges = c.remoteChanges.copy(signed = Vector.empty),
        remoteCommit = wait.nextRemoteCommit, remoteNextCommitInfo = Right(rev.nextPerCommitmentPoint),
        unackedMessages = UnackedOps.cutAcked(c.unackedMessages), remotePerCommitmentSecrets = secrets1)

      Right(c1)

    case Right(point) if point == rev.nextPerCommitmentPoint => Left(c)
    case _ => throw new RuntimeException(REVOCATION_UNEXPECTED)
  }
}

trait InvoiceBag {
  def putPreimage(preimage: BinaryData): Unit
  def getExtendedInvoice(hash: BinaryData): Option[ExtendedInvoice]
  def newPreimage: BinaryData = BinaryData(random getBytes 32)
}