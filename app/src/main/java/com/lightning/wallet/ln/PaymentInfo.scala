package com.lightning.wallet.ln

import java.nio.ByteOrder._
import fr.acinq.bitcoin.Protocol._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.crypto.MultiStreamUtils._
import com.lightning.wallet.ln.wire.FailureMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.ln.Tools.random
import scala.language.postfixOps
import scodec.bits.BitVector
import scodec.Attempt

import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import scala.util.{Success, Try}


object PaymentInfo {
  type PublicPaymentRoute = Vector[Hop]
  type ExtraPaymentRoute = Vector[ExtraHop]
  // Used as placeholder for unresolved outgoing payments
  val NOIMAGE = BinaryData("empty" getBytes "UTF-8")
  val FROMBLACKLISTED = "fromblacklisted"

  final val HIDDEN = 0
  final val WAITING = 1
  final val SUCCESS = 2
  final val FAILURE = 3

  def buildRelativeRoute(hops: Vector[PaymentHop], finalAmountMsat: Long) = {
    val start = RelativeCLTVRoute(Vector apply PerHopPayload(0L, finalAmountMsat, 0),
      nodeIds = Vector.empty, finalAmountMsat, finalRelativeExpiry = 0)

    (start /: hops.reverse) {
      case RelativeCLTVRoute(payloads, nodes, msat, expiry) \ hop =>
        RelativeCLTVRoute(PerHopPayload(hop.shortChannelId, msat, expiry) +: payloads,
          hop.nodeId +: nodes, msat + hop.nextFee(msat), expiry + hop.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assoc: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size, "Payload count mismatch: there should be exactly as much payloads as node pubkeys")
    makePacket(PrivateKey(random getBytes 32), nodes, payloads.map(php => serialize(perHopPayloadCodec encode php).toArray), assoc)
  }

  def buildPayment(rd: RoutingData, pr: PaymentRequest, chan: Channel) = for {
    // Build a new OutgoingPayment if we have routes available AND channel has an id
    // provided RoutingData may have bad nodes and channels which have to be preserved

    chanId <- chan.pull(_.channelId)
    cltvExpiry = LNParams.broadcaster.currentHeight + pr.minFinalCltvExpiry
    RelativeCLTVRoute(relPayloads, nodeIds, firstAmount, relFirstExpiry) <- rd.routes.headOption
    // relPayloads CLTV values start from zero so should be updated when payment is actually being built
    rd1 = RoutingData(rd.routes.tail, rd.badNodes, rd.badChannels, buildOnion(nodes = nodeIds :+ pr.nodeId,
      for (payload <- relPayloads) yield payload.modify(_.outgoingCltv).using(cltvExpiry+),
      pr.paymentHash), firstAmount, relFirstExpiry + cltvExpiry)

    // Save the rest of unused routes in case we might need them
  } yield OutgoingPayment(rd1, NOIMAGE, pr, chanId, WAITING)

  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  // Additionally reduce remaining routes and remember failed nodes and channels
  def cutRoutes(fail: UpdateFailHtlc, rd: RoutingData, finalNodeId: PublicKey) =

    parseErrorPacket(rd.onion.sharedSecrets, fail.reason) collect {
      case ErrorPacket(nodeKey, _: Perm) if finalNodeId == nodeKey =>
        // Permanent error from a final node, nothing we can do
        rd.copy(routes = Vector.empty)

      case ErrorPacket(_, message: Update) =>
        // There may be other operational channels left so try them out, also remember this channel as failed
        val routes1 = rd.routes.filterNot(_.payloads.map(_.shortChannelId) contains message.update.shortChannelId)
        rd.copy(routes = routes1, badChannels = rd.badChannels + message.update.shortChannelId)

      case ErrorPacket(nodeKey, _: Node) =>
        // Remove routes with this node, also mark this node as failed
        val routes1 = rd.routes.filterNot(_.nodeIds contains nodeKey)
        rd.copy(routes = routes1, badNodes = rd.badNodes + nodeKey)

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

        case Success(_) if decoded.value.outgoingCltv != add.expiry =>
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

// Route metadata with relative CLTV PerHopPayload values which start from zero
case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltv: Int)
case class RelativeCLTVRoute(payloads: Vector[PerHopPayload], nodeIds: Vector[PublicKey],
                             finalMsat: Long, finalRelativeExpiry: Int)

case class RoutingData(routes: Vector[RelativeCLTVRoute], badNodes: Set[PublicKey],
                       badChannels: Set[Long], onion: SecretsAndPacket = null,
                       amountWithFee: Long = 0L, expiry: Long = 0L)

case class OutgoingPayment(routing: RoutingData, preimage: BinaryData,
                           request: PaymentRequest, chanId: BinaryData,
                           status: Int) extends PaymentInfo {

  def actualStatus = if (preimage != NOIMAGE) SUCCESS else status
}

trait PaymentInfoBag { me =>
  def upsertPaymentInfo(info: PaymentInfo): Unit
  def updateStatus(status: Int, hash: BinaryData): Unit
  def updatePreimage(update: UpdateFulfillHtlc): Unit
  def updateReceived(add: UpdateAddHtlc): Unit

  def newPreimage: BinaryData = BinaryData(random getBytes 32)
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]

  def extractPreimage(tx: Transaction): Unit = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, preimg, _) if preimg.size == 32 => me updatePreimage UpdateFulfillHtlc(null, 0L, preimg)
    case Seq(_, _, _, preimg, _) if preimg.size == 32 => me updatePreimage UpdateFulfillHtlc(null, 0L, preimg)
  }
}

trait PaymentHop {
  def nextFee(msat: Long): Long
  def shortChannelId: Long
  def cltvExpiryDelta: Int
  def nodeId: PublicKey
}

case class ExtraHop(nodeId: PublicKey, shortChannelId: Long, fee: Long, cltvExpiryDelta: Int) extends PaymentHop {
  def pack = aconcat(nodeId.toBin.data.toArray, writeUInt64(shortChannelId, BIG_ENDIAN), writeUInt64(fee, BIG_ENDIAN),
    writeUInt16(cltvExpiryDelta, BIG_ENDIAN).data.toArray, Array.emptyByteArray)

  // Already pre-calculated
  def nextFee(msat: Long) = fee
}

case class Hop(nodeId: PublicKey, lastUpdate: ChannelUpdate) extends PaymentHop {
  // Fee is not pre-calculated for public hops so we need to derive it accoring to rules specified in BOLT 04
  def nextFee(msat: Long) = lastUpdate.feeBaseMsat + (lastUpdate.feeProportionalMillionths * msat) / 1000000L
  def cltvExpiryDelta = lastUpdate.cltvExpiryDelta
  def shortChannelId = lastUpdate.shortChannelId
}