package com.lightning.wallet.ln

import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.wire._
import fr.acinq.bitcoin.BinaryData


abstract class Channel(state: List[String], data: ChannelData)
extends StateMachine[ChannelData](state, data) { me =>
  def extractState = (state, data)

  val bag: InvoiceBag
  val transport: TransportHandler

  def doProcess(change: Any) = (change, data, state) match {

    case otherwise =>
      // Let know if received an unhandled message
      android.util.Log.d("Channel", s"Unhandled $otherwise")
  }
}

object Channel {
  val INITIALIZED = "Initialized"
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