package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.ln.wire.FailureMessageCodecs.BADONION
import com.lightning.wallet.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt

import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import scala.util.{Success, Try}


trait PaymentInfo {
  val preimage: BinaryData
  val request: PaymentRequest
  val received: MilliSatoshi
  val chanId: BinaryData
  val status: Long
}

case class IncomingPayment(preimage: BinaryData, request: PaymentRequest, received: MilliSatoshi,
                           chanId: BinaryData, status: Long) extends PaymentInfo

case class RoutingData(routes: Vector[PaymentRoute], onion: SecretsAndPacket, amountWithFee: Long, expiry: Long)
case class OutgoingPayment(routing: RoutingData, preimage: BinaryData, request: PaymentRequest, received: MilliSatoshi,
                           chanId: BinaryData, status: Long) extends PaymentInfo {

  def isPending: Boolean = status == TEMP || status == WAITING
  def isFailed: Boolean = !isFulfilled && status == FAILURE
  def isFulfilled: Boolean = preimage != NOIMAGE
}

trait PaymentInfoBag {
  def failPending(status: Long, chanId: BinaryData): Unit
  def updateStatus(status: Long, hash: BinaryData): Unit
  def updatePreimage(update: UpdateFulfillHtlc): Unit
  def updateRouting(out: OutgoingPayment): Unit
  def updateReceived(add: UpdateAddHtlc): Unit

  def newPreimage: BinaryData = BinaryData(random getBytes 32)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def putPaymentInfo(info: PaymentInfo): Unit
}

object PaymentInfo {
  final val TEMP = 0L
  final val HIDDEN = 1L
  final val WAITING = 2L
  final val SUCCESS = 3L
  final val FAILURE = 4L

  // Used for unresolved outgoing payment infos
  val NOIMAGE = BinaryData("empty" getBytes "UTF-8")

  def nodeFee(baseMsat: Long, proportional: Long, msat: Long) =
    baseMsat + (proportional * msat) / 1000000L

  def buildPayloads(finalAmountMsat: Long, finalExpiry: Int, hops: PaymentRoute) = {
    val paymentPayloads = Vector apply PerHopPayload(0L, finalAmountMsat, finalExpiry)
    val startValues = (paymentPayloads, finalAmountMsat, finalExpiry)

    (startValues /: hops.reverse) { case (loads, msat, expiry) ~ hop =>
      val nextFee = nodeFee(hop.lastUpdate.feeBaseMsat, hop.lastUpdate.feeProportionalMillionths, msat)
      val perHopPayloads = PerHopPayload(hop.lastUpdate.shortChannelId, msat, expiry) +: loads
      (perHopPayloads, msat + nextFee, expiry + hop.lastUpdate.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assocData: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(perHopPayloadCodec.encode).map(serialize(_).toArray), assocData)
  }

  private def without(routes: Vector[PaymentRoute], predicate: Hop => Boolean) = routes.filterNot(_ exists predicate)
  private def withoutChannel(routes: Vector[PaymentRoute], chanId: Long) = without(routes, _.lastUpdate.shortChannelId == chanId)
  private def withoutNode(routes: Vector[PaymentRoute], nodeId: BinaryData) = without(routes, _.nodeId == nodeId)

  def cutRoutes(fail: UpdateFailHtlc, payment: OutgoingPayment) =
    parseErrorPacket(payment.routing.onion.sharedSecrets, fail.reason) map {
      case ErrorPacket(nodeId, _: Perm) if payment.request.nodeId == nodeId =>
        // Permanent error from a final node, nothing we can do here
        Vector.empty

      case ErrorPacket(nodeId, _: Node) =>
        // Look for channels left without this node
        withoutNode(payment.routing.routes, nodeId.toBin)

      case ErrorPacket(nodeId, message: Update) =>
        // This node may have other channels left so try them out
        withoutChannel(payment.routing.routes, message.update.shortChannelId)

      case _ =>
        // Nothing to cut
        payment.routing.routes

      // Could not parse, try the rest of routes
    } getOrElse payment.routing.routes

  private def failHtlc(sharedSecret: BinaryData, add: UpdateAddHtlc, failure: FailureMessage) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag) = Try {
    val packet = parsePacket(nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    val payload = LightningMessageCodecs.perHopPayloadCodec decode BitVector(packet.payload)
    Tuple3(payload, packet.nextPacket, packet.sharedSecret)
  } map {
    case (_, nextPacket, sharedSecret) if nextPacket.isLast =>
      // We are the final HTLC recipient, the only viable option

      bag getPaymentInfo add.paymentHash match {
        case Success(pay) if pay.request.amount.exists(add.amountMsat > _.amount * 2) =>
          // GUARD: they have sent too much funds, this is a protective measure against that
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Success(pay) if pay.request.amount.exists(add.amountMsat < _.amount) =>
          // GUARD: amount is less than what we requested, we won't accept such payment
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Success(pay: IncomingPayment) =>
          // We have a valid *incoming* payment
          CMDFulfillHtlc(add.id, pay.preimage)

        case _ =>
          // Payment spec has not been found
          failHtlc(sharedSecret, add, UnknownPaymentHash)
      }

    case (Attempt.Successful(_), _, sharedSecret) =>
      // We don't route so can't find the next node
      failHtlc(sharedSecret, add, UnknownNextPeer)

    case (Attempt.Failure(_), _, sharedSecret) =>
      // Payload could not be parsed at all so fail it
      failHtlc(sharedSecret, add, PermanentNodeFailure)

  } getOrElse {
    val hash = sha256(add.onionRoutingPacket)
    CMDFailMalformedHtlc(add.id, hash, BADONION)
  }
}