package com.lightning.wallet.ln

import com.softwaremill.quicklens._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.crypto.{Generators, ShaHashesWithIndex}
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Exceptions._
import com.lightning.wallet.ln.Helpers.Funding
import fr.acinq.bitcoin.{BinaryData, Transaction}
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}


class Channel(state: List[String], data: ChannelData, bag: InvoiceBag)
extends StateMachine[ChannelData](state, data) { me =>

  def extractState = (state, data)
  def doProcess(change: Any) = (change, data, state.head) match {
    case (cmd: CMDOpenChannel, InitData(announce), WAIT_FOR_INIT) =>

      val lp = cmd.localParams
      val open = OpenChannel(cmd.temporaryChannelId, cmd.fundingSatoshis, cmd.pushMsat, lp.dustLimitSatoshis,
        lp.maxHtlcValueInFlightMsat, lp.channelReserveSatoshis, lp.htlcMinimumMsat, cmd.initialFeeratePerKw,
        lp.toSelfDelay, lp.maxAcceptedHtlcs, lp.fundingPrivKey.publicKey, lp.revocationSecret.toPoint,
        lp.paymentKey.toPoint, Generators.perCommitPoint(lp.shaSeed, 0), lp.delayedPaymentKey.toPoint)

      val data1 = WaitAcceptData(announce, cmd, open)
      become(data1, state1 = WAIT_FOR_ACCEPT)

    // GUARD: remote requires local to keep too much in reserve
    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT)
      if LNParams.exceedsReserve(accept.channelReserveSatoshis, waitAccept.cmd.fundingSatoshis) =>
      become(ErrorData(waitAccept.announce, CHANNEL_RESERVE_TOO_HIGH), state1 = CLOSED)

    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT) =>
      val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat,
        accept.channelReserveSatoshis, accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs,
        accept.fundingPubkey, accept.revocationBasepoint, accept.paymentBasepoint, accept.delayedPaymentBasepoint,
        waitAccept.cmd.remoteInit.globalFeatures, waitAccept.cmd.remoteInit.localFeatures)

      become(WaitFundingDataInternal(waitAccept.announce, waitAccept.cmd,
        accept.firstPerCommitmentPoint, remoteParams, waitAccept.lastSent),
        state1 = WAIT_FUNDING_CREATED)

    // We now have a funding tx with funding out
    case (Tuple2(funding: Transaction, outIndex: Int),
      wait: WaitFundingDataInternal, WAIT_FUNDING_CREATED) =>

      val (localSpec, localCommitTx, remoteSpec, remoteCommitTx) =
        Funding.makeFirstFunderCommitTxs(wait.cmd.localParams, wait.remoteParams,
          wait.cmd.fundingSatoshis, wait.cmd.pushMsat, wait.cmd.initialFeeratePerKw,
          funding.hash, outIndex, wait.remoteFirstPerCommitmentPoint)

      val localSigOfRemoteTx = Scripts.sign(remoteCommitTx, wait.cmd.localParams.fundingPrivKey)
      val fundingCreated = FundingCreated(wait.cmd.temporaryChannelId, funding.hash, outIndex, localSigOfRemoteTx)
      val firstRemoteCommit = RemoteCommit(0, remoteSpec, remoteCommitTx.tx.txid, wait.remoteFirstPerCommitmentPoint)

      become(WaitFundingSignedData(wait.announce, Tools.toLongId(funding.hash, outIndex),
        wait.cmd.localParams, wait.remoteParams, funding, localSpec, localCommitTx,
        firstRemoteCommit, fundingCreated), state1 = WAIT_FUNDING_SIGNED)

    // They have signed our first commit tx, we can broadcast a funding tx
    case (remote: FundingSigned, wait: WaitFundingSignedData, WAIT_FUNDING_SIGNED) =>
      val signedLocalCommitTx = Scripts.addSigs(wait.localCommitTx, wait.localParams.fundingPrivKey.publicKey,
        wait.remoteParams.fundingPubKey, Scripts.sign(wait.localCommitTx, wait.localParams.fundingPrivKey), remote.signature)

      val failed = Scripts.checkSpendable(signedLocalCommitTx).isFailure
      if (failed) become(ErrorData(wait.announce, CHANNEL_REMOTE_SIG_INVALID), state1 = CLOSED) else {
        val localCommit = LocalCommit(0L, wait.localSpec, PublishableTxs(Nil, signedLocalCommitTx), null)
        val commitments = Commitments(wait.localParams, wait.remoteParams, localCommit, wait.remoteCommit,
          localChanges = Changes(proposed = Vector.empty, signed = Vector.empty, acked = Vector.empty),
          remoteChanges = Changes(proposed = Vector.empty, signed = Vector.empty, Vector.empty),
          localNextHtlcId = 0L, remoteNextHtlcId = 0L, remoteNextCommitInfo = Right(null),
          unackedMessages = Vector.empty, commitInput = wait.localCommitTx.input,
          ShaHashesWithIndex(Map.empty, None), wait.channelId)

        // At this point funding tx should be broadcasted
        become(WaitFundingConfirmedData(wait.announce, commitments,
          None, wait.lastSent), state1 = WAIT_FUNDING_DONE)
      }

    case (FundingTimeout, wait, WAIT_FUNDING_DONE) =>
      val error = ErrorData(wait.announce, CHANNEL_FUNDING_TIMEOUT)
      become(error, state1 = CLOSED)

    // We have not yet sent a FundingLocked to them but just got a FundingLocked from them so we keep it
    case (their: FundingLocked, wait @ WaitFundingConfirmedData(_, _, _, _: FundingCreated), WAIT_FUNDING_DONE) =>
      val wait1 = wait.modify(_.their) setTo Some(their)
      me stayWith wait1

    // We have got a FundingDepthOk blockchain event but have not yet got a confirmation from them
    case (FundingDepthOk, wait @ WaitFundingConfirmedData(_, commitments, None, _), WAIT_FUNDING_DONE) =>
      val nextPerCommitmentPoint = Generators.perCommitPoint(commitments.localParams.shaSeed, 1)
      val our = FundingLocked(commitments.channelId, nextPerCommitmentPoint)
      me stayWith wait.copy(lastSent = our)

    // We have already sent them a FundingLocked some time ago, now we got a FundingLocked from them
    case (their: FundingLocked, wait @ WaitFundingConfirmedData(announce, commitments, _, _: FundingLocked), WAIT_FUNDING_DONE) =>
      val commitments1 = commitments.modify(_.remoteNextCommitInfo) setTo Right(their.nextPerCommitmentPoint)
      become(NormalData(announce, commitments1), state1 = NORMAL)

    // They have already sent us a FundingLocked so we got it saved and now we get a FundingDepthOk blockchain event
    case (FundingDepthOk, wait @ WaitFundingConfirmedData(announce, commitments, Some(their), _), WAIT_FUNDING_DONE) =>
      val commitments1 = commitments.modify(_.remoteNextCommitInfo) setTo Right(their.nextPerCommitmentPoint)
      become(NormalData(announce, commitments1), state1 = NORMAL)

    case otherwise =>
      // Let know if received an unhandled message
      android.util.Log.d("Channel", s"Unhandled $otherwise")
  }
}

object Channel {
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FUNDING_CREATED = "WaitFundingCreated"
  val WAIT_FUNDING_SIGNED = "WaitFundingSigned"
  val WAIT_FUNDING_DONE = "WaitFundingDone"

  val SHUTDOWN = "Shutdown"
  val NORMAL = "Normal"
  val CLOSED = "Closed"

  val FundingTimeout = "FundingTimeout"
  val FundingDepthOk = "FundingDepthOk"
  val FundingDeeplyBuried = "FundingDeeplyBuried"
}