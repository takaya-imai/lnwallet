package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import scala.util.{Success, Try}

import com.lightning.wallet.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt


trait PaymentInfo {
  def actualStatus: Int
  val preimage: BinaryData
  val request: PaymentRequest
  val chanId: BinaryData
  val status: Int
}

case class IncomingPayment(received: MilliSatoshi, preimage: BinaryData,
                           request: PaymentRequest, chanId: BinaryData,
                           status: Int) extends PaymentInfo {

  def actualStatus = status
}

case class RoutingData(routes: Vector[PaymentRoute], badNodes: Set[PublicKey],
                       badChannels: Set[Long], onion: SecretsAndPacket,
                       amountWithFee: Long, expiry: Long)

case class OutgoingPayment(routing: RoutingData, preimage: BinaryData,
                           request: PaymentRequest, chanId: BinaryData,
                           status: Int) extends PaymentInfo {

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
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assocData)
  }

  // If we have routes available AND channel has an id AND request has some definite amount
  def buildPayment(rs: Vector[PaymentRoute], badNodes: Set[PublicKey], badChannels: Set[Long],
                   pr: PaymentRequest, chan: Channel): Option[OutgoingPayment] = for {

    route <- rs.headOption
    chanId <- chan.pull(_.channelId)
    MilliSatoshi(amount) <- pr.amount

    (payloads, amountWithFees, finalExpiry) = buildPayloads(amount, LNParams.sendExpiry, route)
    onion = buildOnion(chan.data.announce.nodeId +: route.map(_.nextNodeId), payloads, pr.paymentHash)
    routing = RoutingData(rs.tail, badNodes, badChannels, onion, amountWithFees, finalExpiry)
  } yield OutgoingPayment(routing, NOIMAGE, pr, chanId, TEMP)

  private def without(rs: Vector[PaymentRoute], test: Hop => Boolean) = rs.filterNot(_ exists test)
  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  def cutRoutes(fail: UpdateFailHtlc, routing: RoutingData, finalNodeId: PublicKey) = {
    val RoutingData(routes, badNodes, badChannels, onion: SecretsAndPacket, _, _) = routing

    parseErrorPacket(onion.sharedSecrets, fail.reason) collect {
      case ErrorPacket(nodeId, _: Perm) if finalNodeId == nodeId =>
        // Permanent error from a final node, nothing we can do here
        (Vector.empty, badNodes, badChannels)

      case ErrorPacket(nodeId, _: Node) =>
        // Look for channels left without this node, also remember this node
        (without(routes, _.nodeId == nodeId), badNodes + nodeId, badChannels)

      case ErrorPacket(nodeId, message: Update) =>
        // This node may have other channels left so try them out too, also remember this channel as failed
        val routesWithoutChannel = without(routes, _.lastUpdate.shortChannelId == message.update.shortChannelId)
        (routesWithoutChannel, badNodes, badChannels + message.update.shortChannelId)

      // Nothing to cut so we try other routes
    } getOrElse (routes, badNodes, badChannels)
  }

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
    CMDFailMalformedHtlc(add.id, hash, BADONION)
  }
}