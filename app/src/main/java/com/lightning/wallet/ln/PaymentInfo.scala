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
  def actualStatus: Int
  val preimage: BinaryData
  val request: PaymentRequest
  val chanId: BinaryData
  val status: Int
}

case class IncomingPayment(received: MilliSatoshi, preimage: BinaryData, request: PaymentRequest,
                           chanId: BinaryData, status: Int) extends PaymentInfo {

  def actualStatus = status
}

case class RoutingData(routes: Vector[PaymentRoute], onion: SecretsAndPacket, amountWithFee: Long, expiry: Long)
case class OutgoingPayment(routing: RoutingData, preimage: BinaryData, request: PaymentRequest,
                           chanId: BinaryData, status: Int) extends PaymentInfo {

  def actualStatus = if (preimage != NOIMAGE) SUCCESS else status
}

trait PaymentInfoBag {
  def updateStatus(pre: Int, post: Int): Unit
  def updateStatus(status: Int, hash: BinaryData): Unit
  def updatePreimage(update: UpdateFulfillHtlc): Unit
  def updateRouting(out: OutgoingPayment): Unit
  def updateReceived(add: UpdateAddHtlc): Unit

  def newPreimage: BinaryData = BinaryData(random getBytes 32)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def putPaymentInfo(info: PaymentInfo): Unit
}

object PaymentInfo {
  // Used for unresolved outgoing payment infos
  val NOIMAGE = BinaryData("empty" getBytes "UTF-8")
  val FROMBLACKLISTED = "fromblacklisted"
  val NOROUTEFOUND = "noroutefound"

  final val TEMP = 0
  final val HIDDEN = 1
  final val WAITING = 2
  final val SUCCESS = 3
  final val FAILURE = 4
  final val REFUND = 5

  def nodeFee(baseMsat: Long, proportional: Long, msat: Long) =
    baseMsat + (proportional * msat) / 1000000L

  def buildPayloads(finalAmountMsat: Long, finalExpiry: Int, hops: PaymentRoute) = {
    val paymentPayloads = Vector apply PerHopPayload(0L, finalAmountMsat, finalExpiry)
    val startValues = (paymentPayloads, finalAmountMsat, finalExpiry)

    (startValues /: hops.reverse) { case (loads, msat, expiry) \ hop =>
      val nextFee = nodeFee(hop.lastUpdate.feeBaseMsat, hop.lastUpdate.feeProportionalMillionths, msat)
      val perHopPayloads = PerHopPayload(hop.lastUpdate.shortChannelId, msat, expiry) +: loads
      (perHopPayloads, msat + nextFee, expiry + hop.lastUpdate.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assocData: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(perHopPayloadCodec.encode).map(serialize(_).toArray), assocData)
  }

  def without(routes: Vector[PaymentRoute], predicate: Hop => Boolean) = routes.filterNot(_ exists predicate)
  def withoutChannel(routes: Vector[PaymentRoute], chanId: Long) = without(routes, _.lastUpdate.shortChannelId == chanId)
  def withoutUnsupportedAmount(routes: Vector[PaymentRoute], amount: Long) = without(routes, amount < _.lastUpdate.htlcMinimumMsat)
  def withoutNode(routes: Vector[PaymentRoute], nodeId: BinaryData) = without(routes, _.nodeId == nodeId)

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

  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, minExpiry: Int) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    val payload = LightningMessageCodecs.perHopPayloadCodec decode BitVector(packet.payload)
    Tuple3(payload, packet.nextPacket, packet.sharedSecret)
  } map {
    case (Attempt.Successful(decoded), nextPacket, sharedSecret) if nextPacket.isLast =>
      // We are the final HTLC recipient, the only viable option since we don't route

      bag getPaymentInfo add.paymentHash match {
        case Success(_) if add.expiry < minExpiry =>
          // GUARD: not enough time to redeem it on-chain
          failHtlc(sharedSecret, FinalExpiryTooSoon, add)

        case Success(_) if decoded.value.outgoing_cltv_value != add.expiry =>
          // GUARD: final outgoing CLTV value does not equal the one from message
          failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

        case Success(pay) if pay.request.amount.exists(add.amountMsat > _.amount * 2) =>
          // GUARD: they have sent too much funds, this is a protective measure against that
          failHtlc(sharedSecret, IncorrectPaymentAmount, add)

        case Success(pay) if pay.request.amount.exists(add.amountMsat < _.amount) =>
          // GUARD: amount is less than what we requested, we won't accept such payment
          failHtlc(sharedSecret, IncorrectPaymentAmount, add)

        case Success(pay: IncomingPayment) =>
          // We have a valid *incoming* payment
          CMDFulfillHtlc(add.id, pay.preimage)

        case _ =>
          // Payment spec has not been found
          failHtlc(sharedSecret, UnknownPaymentHash, add)
      }

    case (Attempt.Successful(_), _, sharedSecret) =>
      // We don't route so can't find the next node
      failHtlc(sharedSecret, UnknownNextPeer, add)

    case (Attempt.Failure(_), _, sharedSecret) =>
      // Payload could not be parsed at all so fail it
      failHtlc(sharedSecret, PermanentNodeFailure, add)

  } getOrElse {
    val hash = sha256(add.onionRoutingPacket)
    // BADONION bit must be set in failure_code
    CMDFailMalformedHtlc(add.id, hash, BADONION)
  }
}