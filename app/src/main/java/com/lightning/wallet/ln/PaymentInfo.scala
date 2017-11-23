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
      nodeIds = Vector.empty, finalMsat = finalAmountMsat, finalRelativeExpiry = 0)

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

  def completeRoutingData(rd: RoutingData) = for {
    // Build a new RoutingData if we have routes available
    // provided RoutingData may have bad nodes and channels which have to be preserved
    RelativeCLTVRoute(relPayloads, nodeIds, firstAmount, firstExpiry) <- rd.routes.headOption
    absoluteExpiry = LNParams.broadcaster.currentHeight + rd.pr.minFinalCltvExpiry.getOrElse(default = 9)
    absolutePayloads = for (payload <- relPayloads) yield payload.modify(_.outgoingCltv).using(absoluteExpiry+)
    // First expiry is also relative so it should be updated with absolute part just like payloads above
  } yield rd.copy(onion = buildOnion(nodeIds :+ rd.pr.nodeId, absolutePayloads, rd.pr.paymentHash),
    routes = rd.routes.tail, amountWithFee = firstAmount, expiry = firstExpiry + absoluteExpiry)

  def emptyRD(pr: PaymentRequest) = {
    val packet = Packet(Array.empty, random getBytes 33, random getBytes DataLength, random getBytes MacLength)
    RoutingData(Vector.empty, Set.empty, Set.empty, pr, SecretsAndPacket(Vector.empty, packet), 0L, 0L)
  }

  private def failHtlc(sharedSecret: BinaryData, failure: FailureMessage, add: UpdateAddHtlc) = {
    // Will send an error onion packet which contains a detailed description back to payment sender
    val reason = createErrorPacket(sharedSecret, failure)
    CMDFailHtlc(add.id, reason)
  }

  private def withoutNode(faultyId: PublicKey, rd: RoutingData) = {
    val updatedRoutes = rd.routes.filterNot(_.nodeIds contains faultyId)
    rd.copy(routes = updatedRoutes, badNodes = rd.badNodes + faultyId)
  }

  def cutRoutes(fail: UpdateFailHtlc)(rd: RoutingData) = {
    val x = parseErrorPacket(rd.onion.sharedSecrets, fail.reason)
    println(x)

    x collect {
      // Reduce remaining routes and remember failed nodes and channels

      case ErrorPacket(nodeKey, UnknownNextPeer) =>
        val _ \ nodeIds = rd.onion.sharedSecrets.unzip

        nodeIds drop 1 zip nodeIds collectFirst {
          case nextId \ prevId if prevId == nodeKey =>
            // Remove all the routes containing next node
            withoutNode(nextId, rd)
        } getOrElse rd

      case ErrorPacket(_, message: Update) =>
        // There may be other operational channels left so try them out, also remember this channel as failed
        val routes1 = rd.routes.filterNot(_.payloads.map(_.shortChannelId) contains message.update.shortChannelId)
        rd.copy(routes = routes1, badChannels = rd.badChannels + message.update.shortChannelId)

      case ErrorPacket(nodeKey, InvalidRealm) => withoutNode(nodeKey, rd)
      case ErrorPacket(nodeKey, _: Node) => withoutNode(nodeKey, rd)

      // Nothing to cut
      // try the next route
    } getOrElse rd
  }

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
          // GUARD: final outgoing CLTV does not equal the one from message
          failHtlc(sharedSecret, FinalIncorrectCltvExpiry(add.expiry), add)

        case Success(pay) if add.amount > pay.amount * 2 =>
          // GUARD: they have sent too much funds, reject it
          failHtlc(sharedSecret, IncorrectPaymentAmount, add)

        case Success(pay) if add.amount < pay.amount =>
          // GUARD: amount is less than requested, reject it
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
case class PaymentInfo(hash: BinaryData, incoming: Int,
                       preimage: BinaryData, amount: MilliSatoshi,
                       status: Int, stamp: Long, text: String) {

  def actualStatus = incoming match {
    // Once we have a preimage it is a SUCCESS
    // but only if this is an outgoing payment
    case 0 if preimage != NOIMAGE => SUCCESS
    // Incoming payment always has preimage
    // so we should always look at status
    case _ => status
  }
}

// Route metadata with relative CLTV PerHopPayload values which start from zero!
case class PerHopPayload(shortChannelId: Long, amtToForward: Long, outgoingCltv: Int)
case class RelativeCLTVRoute(payloads: Vector[PerHopPayload], nodeIds: Vector[PublicKey],
                             finalMsat: Long, finalRelativeExpiry: Int)

// Used by outgoing payments to store routes, onion, bad nodes and channels
case class RoutingData(routes: Vector[RelativeCLTVRoute], badNodes: Set[PublicKey], badChannels: Set[Long],
                       pr: PaymentRequest, onion: SecretsAndPacket, amountWithFee: Long, expiry: Long)

trait PaymentInfoBag { me =>
  def upsertRoutingData(rd: RoutingData): Unit
  def upsertPaymentInfo(info: PaymentInfo): Unit
  def getPaymentInfo(hash: BinaryData): Try[PaymentInfo]
  def getRoutingData(hash: BinaryData): Try[RoutingData]
  def updateStatus(status: Int, hash: BinaryData): Unit
  def updatePreimg(update: UpdateFulfillHtlc): Unit
  def updateAmount(add: UpdateAddHtlc): Unit

  def extractPreimage(tx: Transaction): Unit = tx.txIn.map(_.witness.stack) collect {
    case Seq(_, pre, _) if pre.size == 32 => me updatePreimg UpdateFulfillHtlc(null, 0L, pre)
    case Seq(_, _, _, pre, _) if pre.size == 32 => me updatePreimg UpdateFulfillHtlc(null, 0L, pre)
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