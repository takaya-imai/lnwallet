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
  val NOAMOUNT = MilliSatoshi(0L)

  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  private def build(hops: PaymentRoute, pr: PaymentRequest) = {
    val firstExpiry = LNParams.broadcaster.currentHeight + pr.minFinalCltvExpiry.getOrElse(default = 9L)
    val firstPayload = Vector apply PerHopPayload(shortChannelId = 0L, pr.finalSum.amount, firstExpiry)
    val start = (firstPayload, Vector.empty[PublicKey], pr.finalSum.amount, firstExpiry)

    (start /: hops.reverse) {
      case (payloads, nodes, msat, expiry) \ Hop(nodeId, update) =>
        val fee = update.feeBaseMsat + (update.feeProportionalMillionths * msat) / 1000000L
        val nextPayloads = PerHopPayload(update.shortChannelId, msat, expiry) +: payloads
        (nextPayloads, nodeId +: nodes, msat + fee, expiry + update.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def completeRD(rd: RoutingData) = for {
    firstUnusedPaymentRoute <- rd.routes.headOption
    (payloads, nodeIds, firstAmount, firstExpiry) = build(firstUnusedPaymentRoute, rd.pr)
  } yield rd.copy(onion = buildOnion(nodeIds :+ rd.pr.nodeId, payloads, rd.pr.paymentHash),
    routes = rd.routes.tail, amountWithFee = firstAmount, expiry = firstExpiry)

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

  private def withoutNode(bad: PublicKey, rd: RoutingData) = {
    val updatedPaymentRoutes = without(rd.routes, _.nodeId == bad)
    rd.copy(routes = updatedPaymentRoutes, badNodes = rd.badNodes + bad)
  }

  def cutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) =
    parseErrorPacket(rd.onion.sharedSecrets, fail.reason) collect {
      // Reduce remaining routes and remember bad nodes and channels

      case ErrorPacket(nodeKey, UnknownNextPeer) =>
        val _ \ nodeIds = rd.onion.sharedSecrets.unzip

        nodeIds drop 1 zip nodeIds collectFirst {
          case nextId \ prevId if prevId == nodeKey =>
            // Remove all the routes containing next node
            withoutNode(nextId, rd)
        } getOrElse rd

      case ErrorPacket(_, message: Update) =>
        // There may be other operational channels so try them out, also remember this channel as failed
        val updatedPaymentRoutes = without(rd.routes, _.lastUpdate.shortChannelId == message.update.shortChannelId)
        rd.copy(routes = updatedPaymentRoutes, badChannels = rd.badChannels + message.update.shortChannelId)

      case ErrorPacket(nodeKey, InvalidRealm) => withoutNode(nodeKey, rd)
      case ErrorPacket(nodeKey, _: Node) => withoutNode(nodeKey, rd)

      // Nothing to cut
      // try the next route
    } getOrElse rd

  // After mutually signed HTLCs are present we need to parse and fail/fulfill them
  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentInfoBag, minExpiry: Long) = Try {
    val packet = parsePacket(privateKey = nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    Tuple3(perHopPayloadCodec decode BitVector(packet.payload), packet.nextPacket, packet.sharedSecret)
  } map {
    // We are the final HTLC recipient, the only viable option since we don't route
    case (Attempt.Successful(decoded), nextPacket, sharedSecret) if nextPacket.isLast =>

      bag getPaymentInfo add.paymentHash match {
        case Success(_) if add.expiry < minExpiry =>
          // GUARD: not enough time to redeem it on-chain
          failHtlc(sharedSecret, FinalExpiryTooSoon, add)

        case Success(_) if decoded.value.outgoingCltv != add.expiry =>
          // GUARD: final outgoing CLTV does not equal the one from message
          failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

        case Success(pay) if pay.amount != NOAMOUNT && add.amount > pay.amount * 2 =>
          // GUARD: not a domation case and they have sent too much funds so reject it
          failHtlc(sharedSecret, IncorrectPaymentAmount, add)

        case Success(pay) if pay.amount != NOAMOUNT && add.amount < pay.amount =>
          // GUARD: not a domation case and amount is less than asked so reject it
          failHtlc(sharedSecret, IncorrectPaymentAmount, add)

        case Success(pay) if pay.incoming == 1 =>
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
  def upsertPaymentInfo(info: PaymentInfo): Unit
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