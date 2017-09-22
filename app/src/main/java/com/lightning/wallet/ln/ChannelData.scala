package com.lightning.wallet.ln

import fr.acinq.bitcoin.Crypto._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.AddErrorCodes._

import fr.acinq.bitcoin.{BinaryData, Satoshi, Transaction}
import com.lightning.wallet.ln.LNParams.broadcaster.{csv, cltv, cltvAndCsv}
import com.lightning.wallet.ln.crypto.{Generators, ShaChain, ShaHashesWithIndex}
import com.lightning.wallet.ln.Helpers.Closing.{SuccessAndClaim, TimeoutAndClaim}
import com.lightning.wallet.ln.CommitmentSpec.HtlcFailure
import com.lightning.wallet.ln.Tools.LightningMessages
import fr.acinq.eclair.UInt64


sealed trait Command
case class CMDConfirmed(tx: Transaction) extends Command
case class CMDBestHeight(height: Int) extends Command
case class CMDSpent(tx: Transaction) extends Command
case class CMDFeerate(rate: Long) extends Command
case object CMDHTLCProcess extends Command
case object CMDShutdown extends Command
case object CMDOffline extends Command
case object CMDProceed extends Command
case object CMDOnline extends Command

case class CMDOpenChannel(localParams: LocalParams, temporaryChannelId: BinaryData, initialFeeratePerKw: Long,
                          pushMsat: Long, remoteInit: Init, fundingAmountSat: Long) extends Command

case class CMDFailMalformedHtlc(id: Long, onionHash: BinaryData, code: Int) extends Command
case class CMDFulfillHtlc(id: Long, preimage: BinaryData) extends Command
case class CMDFailHtlc(id: Long, reason: BinaryData) extends Command

sealed trait CMDAddHtlc extends Command { val out: OutgoingPayment }
case class SilentAddHtlc(out: OutgoingPayment) extends CMDAddHtlc
case class PlainAddHtlc(out: OutgoingPayment) extends CMDAddHtlc
case class RetryAddHtlc(out: OutgoingPayment) extends CMDAddHtlc

// CHANNEL DATA

sealed trait ChannelData { val announce: NodeAnnouncement }
case class InitData(announce: NodeAnnouncement) extends ChannelData
case class WaitAcceptData(announce: NodeAnnouncement, cmd: CMDOpenChannel) extends ChannelData
case class WaitFundingData(announce: NodeAnnouncement, cmd: CMDOpenChannel, accept: AcceptChannel) extends ChannelData

case class WaitFundingSignedData(announce: NodeAnnouncement, localParams: LocalParams, channelId: BinaryData,
                                 remoteParams: AcceptChannel, fundingTx: Transaction, localSpec: CommitmentSpec,
                                 localCommitTx: CommitTx, remoteCommit: RemoteCommit) extends ChannelData

// All the data below will be stored

sealed trait HasCommitments extends ChannelData { val commitments: Commitments }
case class WaitFundingDoneData(announce: NodeAnnouncement, our: Option[FundingLocked],
                               their: Option[FundingLocked], fundingTx: Transaction,
                               commitments: Commitments) extends HasCommitments

case class NormalData(announce: NodeAnnouncement,
                      commitments: Commitments, localShutdown: Option[Shutdown] = None,
                      remoteShutdown: Option[Shutdown] = None) extends HasCommitments {

  def isFinishing = localShutdown.isDefined || remoteShutdown.isDefined
}

case class NegotiationsData(announce: NodeAnnouncement, commitments: Commitments, localClosingSigned: ClosingSigned,
                            localShutdown: Shutdown, remoteShutdown: Shutdown) extends HasCommitments


trait EndingData extends HasCommitments {
  def isOutdated = lifespan < System.currentTimeMillis
  def lifespan = startedAt + 1000 * 3600 * 24 * 7
  val startedAt: Long
}

case class RefundingData(announce: NodeAnnouncement, commitments: Commitments,
                         startedAt: Long = System.currentTimeMillis) extends EndingData

trait CommitPublished {
  // Cumulative delay, final fee, final amount
  type PublishStatus = (Option[Long], Satoshi, Satoshi)
  val allTransactions: Seq[Transaction]
  def getAllStates: Seq[PublishStatus]
}

case class ClosingData(announce: NodeAnnouncement, commitments: Commitments, mutualClose: Seq[Transaction] = Nil,
                       localCommit: Seq[LocalCommitPublished] = Nil, remoteCommit: Seq[RemoteCommitPublished] = Nil,
                       nextRemoteCommit: Seq[RemoteCommitPublished] = Nil, revokedCommit: Seq[RevokedCommitPublished] = Nil,
                       startedAt: Long = System.currentTimeMillis) extends EndingData with CommitPublished {

  def getAllStates =
    localCommit.flatMap(_.getAllStates) ++ remoteCommit.flatMap(_.getAllStates) ++
      nextRemoteCommit.flatMap(_.getAllStates) ++ revokedCommit.flatMap(_.getAllStates)

  val allTransactions =
    mutualClose ++ localCommit.flatMap(_.allTransactions) ++ remoteCommit.flatMap(_.allTransactions) ++
      nextRemoteCommit.flatMap(_.allTransactions) ++ revokedCommit.flatMap(_.allTransactions)

  lazy val allCommits =
    mutualClose ++ localCommit.map(_.commitTx) ++ remoteCommit.map(_.commitTx) ++
      nextRemoteCommit.map(_.commitTx) ++ revokedCommit.map(_.commitTx)
}

case class LocalCommitPublished(claimMainDelayed: Seq[ClaimDelayedOutputTx], claimHtlcSuccess: Seq[SuccessAndClaim],
                                claimHtlcTimeout: Seq[TimeoutAndClaim], commitTx: Transaction) extends CommitPublished {

  val allTransactions = List(commitTx) ++ claimMainDelayed.map(_.tx) ++
    claimHtlcSuccess.map(_._1.tx) ++ claimHtlcTimeout.map(_._1.tx) ++
    claimHtlcSuccess.map(_._2.tx) ++ claimHtlcTimeout.map(_._2.tx)

  def getAllStates = {
    val mainInfo = for (tier1 <- claimMainDelayed) yield (csv(tier1), tier1 -- tier1, tier1.amount)
    val successInfo = for (tier1 \ tier2 <- claimHtlcSuccess) yield (csv(tier2), tier1 -- tier2, tier2.amount)
    val timeoutInfo = for (tier1 \ tier2 <- claimHtlcTimeout) yield (cltvAndCsv(tier1, tier2), tier1 -- tier2, tier2.amount)
    mainInfo ++ successInfo ++ timeoutInfo
  }
}

case class RemoteCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimHtlcSuccess: Seq[ClaimHtlcSuccessTx],
                                 claimHtlcTimeout: Seq[ClaimHtlcTimeoutTx], commitTx: Transaction) extends CommitPublished {

  // Not inlcuding commitTx here because by definition it has already been published by peer
  val allTransactions = claimMain.map(_.tx) ++ claimHtlcSuccess.map(_.tx) ++ claimHtlcTimeout.map(_.tx)

  def getAllStates = {
    val mainInfo = for (tier1 <- claimMain) yield (Some(0L), tier1 -- tier1, tier1.amount)
    val successInfo = for (tier1 <- claimHtlcSuccess) yield (Some(0L), tier1 -- tier1, tier1.amount)
    val timeoutInfo = for (tier1 <- claimHtlcTimeout) yield (Some apply cltv(tier1), tier1 -- tier1, tier1.amount)
    mainInfo ++ successInfo ++ timeoutInfo
  }
}

case class RevokedCommitPublished(claimMain: Seq[ClaimP2WPKHOutputTx], claimPenalty: Seq[MainPenaltyTx],
                                  commitTx: Transaction) extends CommitPublished {

  // Not inlcuding commitTx here because it has already been published by peer
  val allTransactions = claimMain.map(_.tx) ++ claimPenalty.map(_.tx)

  def getAllStates = {
    val mainInfo = for (tier1 <- claimMain) yield (Some(0L), tier1 -- tier1, tier1.amount)
    val penaltyInfo = for (tier1 <- claimPenalty) yield (Some(0L), tier1 -- tier1, tier1.amount)
    mainInfo ++ penaltyInfo
  }
}

// COMMITMENTS

case class Htlc(incoming: Boolean, add: UpdateAddHtlc)
case class CommitmentSpec(htlcs: Set[Htlc], fulfilled: Set[Htlc], failed: Set[HtlcFailure],
                          feeratePerKw: Long, toLocalMsat: Long, toRemoteMsat: Long)

object CommitmentSpec {
  type HtlcFailure = (Htlc, LightningMessage)
  def findHtlcById(cs: CommitmentSpec, id: Long, isIncoming: Boolean): Option[Htlc] =
    cs.htlcs.find(htlc => htlc.add.id == id && htlc.incoming == isIncoming)

  private def fulfill(cs: CommitmentSpec, in: Boolean, u: UpdateFulfillHtlc) =
    // Suffices to remember an HTLC only for fulfilled payments

    findHtlcById(cs, u.id, in) match {
      case Some(htlc) if htlc.incoming =>
        cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat,
          fulfilled = cs.fulfilled + htlc, htlcs = cs.htlcs - htlc)

      case Some(htlc) =>
        cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat,
          fulfilled = cs.fulfilled + htlc, htlcs = cs.htlcs - htlc)

      case None => cs
    }

  private def fail(cs: CommitmentSpec, in: Boolean, u: HasHtlcId) =
    // Should rememeber HTLC and failure reason to reduce routes

    findHtlcById(cs, u.id, in) match {
      case Some(htlc) if htlc.incoming =>
        cs.copy(toRemoteMsat = cs.toRemoteMsat + htlc.add.amountMsat,
          failed = cs.failed + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case Some(htlc) =>
        cs.copy(toLocalMsat = cs.toLocalMsat + htlc.add.amountMsat,
          failed = cs.failed + Tuple2(htlc, u), htlcs = cs.htlcs - htlc)

      case None => cs
    }

  private def plusOutgoing(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = false, add = data),
      toLocalMsat = cs.toLocalMsat - data.amountMsat)

  private def plusIncoming(data: UpdateAddHtlc, cs: CommitmentSpec) =
    cs.copy(htlcs = cs.htlcs + Htlc(incoming = true, add = data),
      toRemoteMsat = cs.toRemoteMsat - data.amountMsat)

  def reduce(cs: CommitmentSpec, localChanges: LightningMessages, remoteChanges: LightningMessages) = {
    // Before reducing fresh changes we need to get rid of previous fulfilled and failed messages

    val spec1 = cs.copy(fulfilled = Set.empty, failed = Set.empty)
    val spec2 = (spec1 /: localChanges) { case (s, add: UpdateAddHtlc) => plusOutgoing(add, s) case s \ _ => s }
    val spec3 = (spec2 /: remoteChanges) { case (s, add: UpdateAddHtlc) => plusIncoming(add, s) case s \ _ => s }

    val spec4 = (spec3 /: localChanges) {
      case (s, msg: UpdateFailHtlc) => fail(s, in = true, msg)
      case (s, msg: UpdateFailMalformedHtlc) => fail(s, in = true, msg)
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, in = true, msg)
      case s \ _ => s
    }

    (spec4 /: remoteChanges) {
      case (s, msg: UpdateFailHtlc) => fail(s, in = false, msg)
      case (s, msg: UpdateFailMalformedHtlc) => fail(s, in = false, msg)
      case (s, msg: UpdateFee) => s.copy(feeratePerKw = msg.feeratePerKw)
      case (s, msg: UpdateFulfillHtlc) => fulfill(s, in = false, msg)
      case s \ _ => s
    }
  }
}

case class LocalParams(maxHtlcValueInFlightMsat: UInt64, channelReserveSat: Long, toSelfDelay: Int, maxAcceptedHtlcs: Int,
                       fundingPrivKey: PrivateKey, revocationSecret: Scalar, paymentKey: PrivateKey, delayedPaymentKey: Scalar,
                       defaultFinalScriptPubKey: BinaryData, shaSeed: BinaryData, isFunder: Boolean)

case class WaitingForRevocation(nextRemoteCommit: RemoteCommit, sent: CommitSig, localCommitIndexSnapshot: Long)
case class LocalCommit(index: Long, spec: CommitmentSpec, htlcTxsAndSigs: Seq[HtlcTxAndSigs], commitTx: CommitTx)
case class RemoteCommit(index: Long, spec: CommitmentSpec, txid: BinaryData, remotePerCommitmentPoint: Point)
case class HtlcTxAndSigs(txinfo: TransactionWithInputInfo, localSig: BinaryData, remoteSig: BinaryData)
case class Changes(proposed: LightningMessages, signed: LightningMessages, acked: LightningMessages)

case class Commitments(localParams: LocalParams, remoteParams: AcceptChannel, localCommit: LocalCommit,
                       remoteCommit: RemoteCommit, localChanges: Changes, remoteChanges: Changes, localNextHtlcId: Long,
                       remoteNextHtlcId: Long, remoteNextCommitInfo: Either[WaitingForRevocation, Point], commitInput: InputInfo,
                       remotePerCommitmentSecrets: ShaHashesWithIndex, channelId: BinaryData)

object Commitments {
  def pendingHtlcs(c: Commitments): Set[Htlc] =
    c.localCommit.spec.htlcs ++ c.remoteCommit.spec.htlcs ++
      c.remoteNextCommitInfo.left.toSeq.flatMap(_.nextRemoteCommit.spec.htlcs)

  def localHasChanges(c: Commitments): Boolean = c.remoteChanges.acked.nonEmpty || c.localChanges.proposed.nonEmpty
  def remoteHasChanges(c: Commitments): Boolean = c.localChanges.acked.nonEmpty || c.remoteChanges.proposed.nonEmpty
  def addRemoteProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.remoteChanges.proposed).using(_ :+ proposal)
  def addLocalProposal(c: Commitments, proposal: LightningMessage) = c.modify(_.localChanges.proposed).using(_ :+ proposal)

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

  def sendAdd(c: Commitments, cmd: CMDAddHtlc) =
    if (cmd.out.routing.amountWithFee < c.remoteParams.htlcMinimumMsat) throw AddException(cmd, ERR_REMOTE_AMOUNT_LOW)
    else if (cmd.out.request.amount.get > maxHtlcValue) throw AddException(cmd, ERR_AMOUNT_OVERFLOW)
    else if (cmd.out.request.paymentHash.size != 32) throw AddException(cmd, ERR_FAILED)
    else {

      // Let's compute the current commitment
      // *as seen by them* with this change taken into account
      val add = UpdateAddHtlc(c.channelId, c.localNextHtlcId, cmd.out.routing.amountWithFee,
        cmd.out.request.paymentHash, cmd.out.routing.expiry, cmd.out.routing.onion.packet.serialize)

      val c1 = addLocalProposal(c, add).modify(_.localNextHtlcId).using(_ + 1)
      val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
      val fees = if (c1.localParams.isFunder) Scripts.commitTxFee(Satoshi(c1.remoteParams.dustLimitSatoshis), reduced).amount else 0
      val htlcValueInFlightOverflow = UInt64(reduced.htlcs.map(_.add.amountMsat).sum) > c1.remoteParams.maxHtlcValueInFlightMsat
      val isOverflow = reduced.toRemoteMsat / 1000L - c1.remoteParams.channelReserveSatoshis - fees < 0
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.remoteParams.maxAcceptedHtlcs

      // The rest of the guards
      if (htlcValueInFlightOverflow) throw AddException(cmd, ERR_TOO_MANY_HTLC)
      else if (acceptedHtlcsOverflow) throw AddException(cmd, ERR_TOO_MANY_HTLC)
      else if (isOverflow) throw AddException(cmd, ERR_REMOTE_FEE_OVERFLOW)
      else c1 -> add
    }

  def receiveAdd(c: Commitments, add: UpdateAddHtlc, blockLimit: Int) =
    if (add.amountMsat < htlcMinimumMsat) throw new LightningException
    else if (add.id != c.remoteNextHtlcId) throw new LightningException
    else if (add.paymentHash.size != 32) throw new LightningException
    else if (add.expiry < blockLimit) throw new LightningException
    else {

      // Let's compute the current commitment *as seen by us* including this change
      val c1 = addRemoteProposal(c, add).copy(remoteNextHtlcId = c.remoteNextHtlcId + 1)
      val reduced = CommitmentSpec.reduce(c1.localCommit.spec, c1.localChanges.acked, c1.remoteChanges.proposed)
      val htlcValueInFlightOverflow = UInt64(reduced.htlcs.map(_.add.amountMsat).sum) > c1.localParams.maxHtlcValueInFlightMsat
      val localFees = if (c1.localParams.isFunder) Scripts.commitTxFee(dustLimit, reduced).amount else 0L
      val isOverflow = reduced.toRemoteMsat / 1000L - c1.localParams.channelReserveSat - localFees < 0L
      val acceptedHtlcsOverflow = reduced.htlcs.count(_.incoming) > c1.localParams.maxAcceptedHtlcs

      // The rest of the guards
      if (htlcValueInFlightOverflow) throw new LightningException
      else if (acceptedHtlcsOverflow) throw new LightningException
      else if (isOverflow) throw new LightningException
      else c1
    }

  def sendFulfill(c: Commitments, cmd: CMDFulfillHtlc) = {
    val fulfill = UpdateFulfillHtlc(c.channelId, cmd.id, cmd.preimage)
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = true, cmd.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash => addLocalProposal(c, fulfill) -> fulfill
      case None => throw new LightningException
    }
  }

  def receiveFulfill(c: Commitments, fulfill: UpdateFulfillHtlc) =
    getHtlcCrossSigned(commitments = c, incomingRelativeToLocal = false, fulfill.id) match {
      case Some(add) if fulfill.paymentHash == add.paymentHash => addRemoteProposal(c, fulfill)
      case None => throw new LightningException
    }

  def sendFail(c: Commitments, cmd: CMDFailHtlc) = {
    val fail = UpdateFailHtlc(c.channelId, cmd.id, cmd.reason)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  def sendFailMalformed(c: Commitments, cmd: CMDFailMalformedHtlc) = {
    val fail = UpdateFailMalformedHtlc(c.channelId, cmd.id, cmd.onionHash, cmd.code)
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = true, htlcId = cmd.id)
    if (found.isEmpty) throw new LightningException else addLocalProposal(c, fail) -> fail
  }

  def receiveFail(c: Commitments, fail: UpdateFailHtlc) = {
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
    if (found.isEmpty) throw new LightningException else addRemoteProposal(c, fail)
  }

  def receiveFailMalformed(c: Commitments, fail: UpdateFailMalformedHtlc) = {
    val found = getHtlcCrossSigned(c, incomingRelativeToLocal = false, fail.id)
    // A receiving node MUST fail the channel if the BADONION bit is not set
    val isBadOnion = (fail.failureCode & FailureMessageCodecs.BADONION) == 0

    if (isBadOnion) throw new LightningException
    else if (found.isEmpty) throw new LightningException
    else addRemoteProposal(c, fail)
  }

  def sendFee(c: Commitments, ratePerKw: Long) = {
    val updateFee = UpdateFee(c.channelId, ratePerKw)
    val c1 = addLocalProposal(c, updateFee)

    val reduced = CommitmentSpec.reduce(c1.remoteCommit.spec, c1.remoteChanges.acked, c1.localChanges.proposed)
    val fees = Scripts.commitTxFee(dustLimit = Satoshi(c1.remoteParams.dustLimitSatoshis), spec = reduced).amount
    val feesOverflow = reduced.toRemoteMsat / 1000L - c1.remoteParams.channelReserveSatoshis - fees < 0
    if (feesOverflow) throw new LightningException else c1 -> updateFee
  }

  def sendCommit(c: Commitments, remoteNextPerCommitmentPoint: Point) = {
    val spec = CommitmentSpec.reduce(c.remoteCommit.spec, c.remoteChanges.acked, c.localChanges.proposed)
    val paymentKey = Generators.derivePrivKey(c.localParams.paymentKey, remoteNextPerCommitmentPoint)

    val (remoteCommitTx, htlcTimeoutTxs, htlcSuccessTxs, _, _) =
      Helpers.makeRemoteTxs(c.remoteCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, remoteNextPerCommitmentPoint, spec)

    // Generate signatures
    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, paymentKey)

    // Update commitment data
    val remoteChanges1 = c.remoteChanges.copy(acked = Vector.empty, signed = c.remoteChanges.acked)
    val localChanges1 = c.localChanges.copy(proposed = Vector.empty, signed = c.localChanges.proposed)
    val remoteCommit1 = RemoteCommit(index = c.remoteCommit.index + 1, spec, remoteCommitTx.tx.txid, remoteNextPerCommitmentPoint)
    val commitSig = CommitSig(c.channelId, Scripts.sign(remoteCommitTx, c.localParams.fundingPrivKey), htlcSigs.toList)
    val waiting = WaitingForRevocation(remoteCommit1, commitSig, c.localCommit.index)

    val c1 = c.copy(remoteNextCommitInfo = Left apply waiting,
      localChanges = localChanges1, remoteChanges = remoteChanges1)

    c1 -> commitSig
  }

  def receiveCommit(c: Commitments, commit: CommitSig) = {
    val spec = CommitmentSpec.reduce(c.localCommit.spec, c.localChanges.acked, c.remoteChanges.proposed)
    val localPerCommitmentSecret = Generators.perCommitSecret(c.localParams.shaSeed, c.localCommit.index)
    val localPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 1)
    val localNextPerCommitmentPoint = Generators.perCommitPoint(c.localParams.shaSeed, c.localCommit.index + 2)
    val remotePaymentPubkey = Generators.derivePubKey(c.remoteParams.paymentBasepoint, localPerCommitmentPoint)
    val localPaymentKey = Generators.derivePrivKey(c.localParams.paymentKey, localPerCommitmentPoint)

    val (localCommitTx, htlcTimeoutTxs, htlcSuccessTxs) =
      Helpers.makeLocalTxs(c.localCommit.index + 1, c.localParams,
        c.remoteParams, c.commitInput, localPerCommitmentPoint, spec)

    val sortedHtlcTxs = (htlcTimeoutTxs ++ htlcSuccessTxs).sortBy(_.input.outPoint.index)
    val signedCommitTx = Scripts.addSigs(localCommitTx, c.localParams.fundingPrivKey.publicKey,
      c.remoteParams.fundingPubkey, Scripts.sign(localCommitTx, c.localParams.fundingPrivKey),
      remoteSig = commit.signature)

    if (Scripts.checkSpendable(signedCommitTx).isEmpty) throw new LightningException
    if (commit.htlcSignatures.size != sortedHtlcTxs.size) throw new LightningException

    val htlcSigs = for (info <- sortedHtlcTxs) yield Scripts.sign(info, localPaymentKey)
    val combined = (sortedHtlcTxs, htlcSigs, commit.htlcSignatures).zipped.toList

    val htlcTxsAndSigs = combined.collect {
      case (htlcTx: HtlcTimeoutTx, localSig, remoteSig) =>
        val check = Scripts checkSpendable Scripts.addSigs(htlcTx, localSig, remoteSig)
        if (check.isEmpty) throw new LightningException else HtlcTxAndSigs(htlcTx, localSig, remoteSig)

      case (htlcTx: HtlcSuccessTx, localSig, remoteSig) =>
        val isSigValid = Scripts.checkSig(htlcTx, remoteSig, remotePaymentPubkey)
        if (!isSigValid) throw new LightningException else HtlcTxAndSigs(htlcTx, localSig, remoteSig)
    }

    val localCommit1 = LocalCommit(c.localCommit.index + 1, spec, htlcTxsAndSigs, signedCommitTx)
    val remoteChanges1 = c.remoteChanges.copy(proposed = Vector.empty, acked = c.remoteChanges.acked ++ c.remoteChanges.proposed)
    val c1 = c.copy(localChanges = c.localChanges.copy(acked = Vector.empty), remoteChanges = remoteChanges1, localCommit = localCommit1)
    val revokeAndAck = RevokeAndAck(c.channelId, localPerCommitmentSecret, localNextPerCommitmentPoint)
    c1 -> revokeAndAck
  }

  def receiveRevocation(c: Commitments, rev: RevokeAndAck) = c.remoteNextCommitInfo match {
    case Left(_) if c.remoteCommit.remotePerCommitmentPoint != rev.perCommitmentSecret.toPoint =>
      throw new LightningException

    case Left(wait: WaitingForRevocation) =>
      val nextIndex = ShaChain.largestTxIndex - c.remoteCommit.index
      val secrets1 = ShaChain.addHash(c.remotePerCommitmentSecrets, rev.perCommitmentSecret.toBin, nextIndex)
      val localChanges1 = c.localChanges.copy(signed = Vector.empty, acked = c.localChanges.acked ++ c.localChanges.signed)
      val remoteChanges1 = c.remoteChanges.copy(signed = Vector.empty)

      c.copy(localChanges = localChanges1, remoteChanges = remoteChanges1, remoteCommit = wait.nextRemoteCommit,
        remoteNextCommitInfo = Right apply rev.nextPerCommitmentPoint, remotePerCommitmentSecrets = secrets1)

    // Unexpected revocation when we have Point
    case _ => throw new LightningException
  }
}