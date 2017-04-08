package com.lightning.wallet.ln

import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.crypto.Generators
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Exceptions._
import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}


class Channel(state: List[String], data: ChannelData, bag: InvoiceBag)
extends StateMachine[ChannelData](state, data) { me =>

  def extractState = (state, data)
  def doProcess(change: Any) = (change, data, state.head) match {
    case (cod: ChannelOpenData, InitData(announce), WAIT_FOR_INIT) =>

      val lp = cod.localParams
      val open = OpenChannel(cod.temporaryChannelId, cod.fundingSatoshis, cod.pushMsat, lp.dustLimitSatoshis,
        lp.maxHtlcValueInFlightMsat, lp.channelReserveSatoshis, lp.htlcMinimumMsat, cod.initialFeeratePerKw,
        lp.toSelfDelay, lp.maxAcceptedHtlcs, lp.fundingPrivKey.publicKey, lp.revocationSecret.toPoint,
        lp.paymentKey.toPoint, Generators.perCommitPoint(lp.shaSeed, 0),
        lp.delayedPaymentKey.toPoint)

      val data1 = WaitAcceptData(cod, open)
      become(data1, state1 = WAIT_FOR_ACCEPT)

    // remote requires local to keep this much satoshis
    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT)
      if LNParams.exceedsReserve(accept.channelReserveSatoshis, waitAccept.openData.fundingSatoshis) =>
      become(ErrorData(waitAccept.openData.announce, CHANNEL_RESERVE_TOO_HIGH), state1 = CLOSED)

    case (accept: AcceptChannel, waitAccept: WaitAcceptData, WAIT_FOR_ACCEPT) =>
      val remoteParams = RemoteParams(accept.dustLimitSatoshis, accept.maxHtlcValueInFlightMsat,
        accept.channelReserveSatoshis, accept.htlcMinimumMsat, accept.toSelfDelay, accept.maxAcceptedHtlcs,
        accept.fundingPubkey, accept.revocationBasepoint, accept.paymentBasepoint, accept.delayedPaymentBasepoint,
        waitAccept.openData.remoteInit.globalFeatures, waitAccept.openData.remoteInit.localFeatures)

      val firstPoint: Point = accept.firstPerCommitmentPoint
      val data1 = WaitFundingData(waitAccept, firstPoint, remoteParams)
      become(data1, state1 = WAIT_FUNDING_CREATED)

    case otherwise =>
      // Let know if received an unhandled message
      android.util.Log.d("Channel", s"Unhandled $otherwise")
  }
}

object Channel {
  val WAIT_FOR_INIT = "WaitForInit"
  val WAIT_FOR_ACCEPT = "WaitForAccept"
  val WAIT_FUNDING_CREATED = "WaitFundingCreated"
  val CLOSED = "Closed"
}

//trait ChannelListener {
//  def onHtlcAddRejected(htlc: Htlc): Unit = none
//  def onFulfilled(paymentPreimage: BinaryData): Unit = none
//  def onRevokedAndAcked(spec: CommitmentSpec): Unit = none
//}
//
//class ChannelEventProxy extends ChannelListener {
//  def addListener(listener: ChannelListener) = listeners += listener
//  def removeListener(listener: ChannelListener) = listeners -= listener
//  var listeners = Set.empty[ChannelListener]
//
//  // Proxy methods
//  override def onHtlcAddRejected(htlc: Htlc): Unit = for (lst <- listeners) lst onHtlcAddRejected htlc
//  override def onFulfilled(paymentPreimage: BinaryData) = for (lst <- listeners) lst onFulfilled paymentPreimage
//  override def onRevokedAndAcked(spec: CommitmentSpec) = for (lst <- listeners) lst onRevokedAndAcked spec
//}