package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import scala.util.{Success, Try}

import com.lightning.wallet.ln.PaymentHop.PaymentRoute
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
                       badChannels: Set[Long], onion: SecretsAndPacket = null,
                       amountWithFee: Long = 0L, expiry: Long = 0L)

case class OutgoingPayment(routing: RoutingData, preimage: BinaryData,
                           request: PaymentRequest, chanId: BinaryData,
                           status: Int) extends PaymentInfo {

  def actualStatus = if (preimage != NOIMAGE) SUCCESS else status
}

trait PaymentInfoBag { me =>
  def updateStatus(status: Int, hash: BinaryData): Unit
  def updatePreimage(update: UpdateFulfillHtlc): Unit
  def updateReceived(add: UpdateAddHtlc): Unit

  def newPreimage: BinaryData = BinaryData(random getBytes 32)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def upsertPaymentInfo(info: PaymentInfo): Unit

  def extractPreimage(tx: Transaction): Unit = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, preimg, _) if preimg.size == 32 => me updatePreimage UpdateFulfillHtlc(null, 0L, preimg)
    case Seq(_, _, _, preimg, _) if preimg.size == 32 => me updatePreimage UpdateFulfillHtlc(null, 0L, preimg)
  }
}

object PaymentInfo {
  // Used for unresolved outgoing payment infos
  val NOIMAGE = BinaryData("empty" getBytes "UTF-8")
  val FROMBLACKLISTED = "fromblacklisted"

  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  def buildPayloads(hops: PaymentRoute, finalAmountMsat: Long, finalExpiry: Int) = {
    // First PerHopPayload is for recipient so it has a zero shortChannelId and zero fee

    val finalPayload = PerHopPayload(0L, finalAmountMsat, finalExpiry)
    val startValues = (Vector(finalPayload), finalAmountMsat, finalExpiry)

    (startValues /: hops.reverse) {
      case Tuple3(payloads, amountMsat, expiry) \ hop =>
        Tuple3(PerHopPayload(hop.shortChannelId, amountMsat, expiry) +: payloads,
          amountMsat + hop.nextFee(amountMsat), expiry + hop.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def buildPayment(rd: RoutingData, pr: PaymentRequest, chan: Channel) = for {
    // Iff we have routes available AND channel has an id AND request has amount
    // Build a new OutgoingPayment given previous RoutingData

    route <- rd.routes.headOption
    chanId <- chan.pull(_.channelId)
    MilliSatoshi(amount) <- pr.amount

    (payloads, firstAmount, firstExpiry) = buildPayloads(route, amount, LNParams.sendExpiry)
    onion = buildOnion(nodes = route.map(_.nodeId) :+ pr.nodeId, payloads, assoc = pr.paymentHash)
    rd1 = RoutingData(rd.routes.tail, rd.badNodes, rd.badChannels, onion, firstAmount, firstExpiry)
  } yield OutgoingPayment(rd1, NOIMAGE, pr, chanId, WAITING)

  private def without(rs: Vector[PaymentRoute], test: PaymentHop => Boolean) = rs.filterNot(_ exists test)
  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  // Additionally reduce remaining routes and remember failed nodes and channels
  def cutRoutes(fail: UpdateFailHtlc, rd: RoutingData, finalNodeId: PublicKey) =

    parseErrorPacket(rd.onion.sharedSecrets, fail.reason) collect {
      case ErrorPacket(nodeKey, _: Perm) if finalNodeId == nodeKey =>
        // Permanent error from a final node, nothing we can do
        rd.copy(routes = Vector.empty)

      case ErrorPacket(nodeKey, _: Node) =>
        // Look for channels left without this node, also remember this node as failed
        rd.copy(routes = without(rd.routes, _.nodeId == nodeKey), badNodes = rd.badNodes + nodeKey)

      case ErrorPacket(_, message: Update) =>
        // Node may have other channels left so try them out, also remember this channel as failed
        val routesWithoutChannel = without(rd.routes, _.shortChannelId == message.update.shortChannelId)
        rd.copy(routes = routesWithoutChannel, badChannels = rd.badChannels + message.update.shortChannelId)

      // Nothing to cut
    } getOrElse rd

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, minExpiry: Int) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    Tuple3(perHopPayloadCodec decode BitVector(packet.payload), packet.nextPacket, packet.sharedSecret)
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