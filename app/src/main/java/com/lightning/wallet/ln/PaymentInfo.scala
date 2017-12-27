package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRoute
import com.lightning.wallet.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt


object PaymentInfo {
  // Used as placeholder for unresolved outgoing payments
  val NOIMAGE = BinaryData("00000000" getBytes "UTF-8")
  val FROMBLACKLISTED = "fromblacklisted"

  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  private def build(hops: PaymentRoute, pr: PaymentRequest, lastExpiry: Long) = {
    val firstPayload = Vector apply PerHopPayload(0L, pr.finalSum.amount, lastExpiry)
    val start = (firstPayload, Vector.empty[PublicKey], pr.finalSum.amount, lastExpiry)

    (start /: hops.reverse) {
      case (payloads, nodes, msat, expiry) \ hop =>
        val fee = hop.feeBaseMsat + (hop.feeProportionalMillionths * msat) / 1000000L
        val nextPayloads = PerHopPayload(hop.shortChannelId, msat, expiry) +: payloads
        (nextPayloads, hop.nodeId +: nodes, msat + fee, expiry + hop.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def completeRD(rd: RoutingData): Option[RoutingData] = rd.routes.headOption map { firstRoute =>
    val lastChanExpiry = LNParams.broadcaster.currentHeight + rd.pr.minFinalCltvExpiry.getOrElse(9L)
    val Tuple4(payloads, nodeIds, firstAmount, firstExpiry) = build(firstRoute, rd.pr, lastChanExpiry)
    // Reject route if fees are 1.5+ times larger than payment itself or if cltv delta is way too large
    val nope = firstAmount.toDouble / rd.pr.finalSum.amount > 1.5 || firstExpiry - lastChanExpiry > 1440L
    if (nope) rd.copy(routes = rd.routes.tail) else rd.copy(routes = rd.routes.tail, expiry = firstExpiry,
      onion = buildOnion(nodeIds :+ rd.pr.nodeId, payloads, rd.pr.paymentHash), amountWithFee = firstAmount)
  }

  def emptyRD(pr: PaymentRequest) = {
    val packet = Packet(Array.empty, random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(Vector.empty, Set.empty, Set.empty, pr, SecretsAndPacket(Vector.empty, packet), 0L, 0L)
  }

  private def without(rs: Vector[PaymentRoute], fn: Hop => Boolean) = rs.filterNot(_ exists fn)
  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) = {
    // Will send an error onion packet which contains a detailed description back to payment sender
    val reason = createErrorPacket(sharedSecret, failure)
    CMDFailHtlc(add.id, reason)
  }

  private def withoutNode(bad: PublicKeyVec, rd: RoutingData) = {
    val updatedPaymentRoutes = without(rd.routes, bad contains _.nodeId)
    rd.copy(routes = updatedPaymentRoutes, badNodes = rd.badNodes ++ bad)
  }

  def cutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) =
    Try apply parseErrorPacket(rd.onion.sharedSecrets, fail.reason) map {
      // Try to reduce remaining routes and remember bad nodes and channels
      // excluded nodes and channels will be needed for further calls

      case ErrorPacket(nodeKey, UnknownNextPeer) =>
        val _ \ nodePublicKeys = rd.onion.sharedSecrets.unzip
        nodePublicKeys drop 1 zip nodePublicKeys collectFirst {
          case failedId \ previousId if previousId == nodeKey =>
            // Remove all the routes containing next node
            withoutNode(Vector(failedId), rd)
        } getOrElse rd

      case ErrorPacket(_, message: Update) =>
        // There may be other operational chans so try them out, also remember this chan as failed
        val updatedPaymentRoutes = without(rd.routes, _.shortChannelId == message.update.shortChannelId)
        rd.copy(routes = updatedPaymentRoutes, badChannels = rd.badChannels + message.update.shortChannelId)

      case ErrorPacket(nodeKey, InvalidRealm) => withoutNode(Vector(nodeKey), rd)
      case ErrorPacket(nodeKey, _: Node) => withoutNode(Vector(nodeKey), rd)
      case _ => rd

    } getOrElse {
      val _ \ nodeIds = rd.onion.sharedSecrets.unzip
      withoutNode(nodeIds drop 1 dropRight 2, rd)
    }

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, minExpiry: Long) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    Tuple3(perHopPayloadCodec decode BitVector(packet.payload), packet.nextPacket, packet.sharedSecret)
  } map {
    // We are the final HTLC recipient, sanity checks first
    case (Attempt.Successful(decoded), nextPacket, sharedSecret)
      if nextPacket.isLast && decoded.value.outgoingCltv != add.expiry =>
      failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

    case (Attempt.Successful(_), nextPacket, sharedSecret)
      if nextPacket.isLast && add.expiry < minExpiry =>
      failHtlc(sharedSecret, FinalExpiryTooSoon, add)

    case (Attempt.Successful(_), nextPacket, ss) if nextPacket.isLast =>
      // We are the final HTLC recipient and it's sane, check if we have a request
      bag.getRoutingData(add.paymentHash) -> bag.getPaymentInfo(add.paymentHash) match {
        // Payment request may not have an amount which means it's a donation and should not be checked for overflow
        case Success(rd) \ _ if rd.pr.amount.exists(add.amountMsat > _.amount * 2) => failHtlc(ss, IncorrectPaymentAmount, add)
        case Success(rd) \ _ if rd.pr.amount.exists(add.amountMsat < _.amount) => failHtlc(ss, IncorrectPaymentAmount, add)
        case _ \ Success(info) if info.incoming == 1 => CMDFulfillHtlc(add.id, info.preimage)
        case _ => failHtlc(ss, UnknownPaymentHash, add)
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

// Used on UI to quickly display payment details
case class PaymentInfo(hash: BinaryData, incoming: Int, preimage: BinaryData,
                       amount: MilliSatoshi, status: Int, stamp: Long, text: String) {

  def actualStatus = incoming match {
    // Once we have a preimage it is a SUCCESS
    // but only if this is an outgoing payment
    case 0 if preimage != NOIMAGE => SUCCESS
    // Incoming payment always has preimage
    // so we should always look at status
    case _ => status
  }
}

case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltv: Long)
case class RoutingData(routes: Vector[PaymentRoute], badNodes: Set[PublicKey], badChannels: Set[Long],
                       pr: PaymentRequest, onion: SecretsAndPacket, amountWithFee: Long, expiry: Long)

trait PaymentInfoBag { me =>
  def upsertRoutingData(rd: RoutingData): Unit
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def getRoutingData(hash: BinaryData): Try[RoutingData]
  def updateStatus(status: Int, hash: BinaryData): Unit
  def updatePreimg(update: UpdateFulfillHtlc): Unit
  def updateIncoming(add: UpdateAddHtlc): Unit

  def extractPreimage(tx: Transaction): Unit = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, pre, _) if pre.size == 32 => me updatePreimg UpdateFulfillHtlc(null, 0L, pre)
    case Seq(_, _, _, pre, _) if pre.size == 32 => me updatePreimg UpdateFulfillHtlc(null, 0L, pre)
  }
}