package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import scala.util.{Success, Try}
import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import com.lightning.wallet.ln.wire.FailureMessageCodecs.BADONION
import com.lightning.wallet.ln.Tools.random
import fr.acinq.bitcoin.BinaryData
import scodec.bits.BitVector
import scodec.Attempt


trait PaymentSpec { val invoice: Invoice }
case class ExtendedPaymentInfo(spec: PaymentSpec, status: String, stamp: Long)
case class IncomingPaymentSpec(invoice: Invoice, preimage: BinaryData, kind: String = "IncomingPaymentSpec") extends PaymentSpec
case class OutgoingPaymentSpec(invoice: Invoice, preimage: Option[BinaryData], routes: Vector[PaymentRoute], onion: SecretsAndPacket,
                               amountWithFee: Long, expiry: Long, kind: String = "OutgoingPaymentSpec") extends PaymentSpec

trait PaymentSpecBag {
  def putInfo(info: ExtendedPaymentInfo): Unit
  def getRecentInfos: Vector[ExtendedPaymentInfo]
  def getInfoByHash(hash: BinaryData): Try[ExtendedPaymentInfo]
  def updatePaymentStatus(hash: BinaryData, status: String): Unit
  def updateOutgoingPaymentSpec(spec: OutgoingPaymentSpec): Unit
  def newPreimage: BinaryData = random getBytes 32
}

object PaymentSpec {
  // PaymentSpec states
  final val HIDDEN = "WaitHidden"
  final val VISIBLE = "WaitVisible"
  final val SUCCESS = "Success"
  final val FAIL = "Fail"

  // The fee (in msat) that a node should be paid to forward an HTLC of 'amount' millisatoshis
  def nodeFee(baseMsat: Long, proportional: Long, msat: Long): Long = baseMsat + (proportional * msat) / 1000000

  // finalAmountMsat = final amount specified by the recipient
  // finalExpiryBlockCount = final expiry specified by the recipient
  def buildRoute(finalAmountMsat: Long, finalExpiryBlockCount: Int, hops: PaymentRoute) = {
    val startConditions = (Vector.empty[PerHopPayload], finalAmountMsat, finalExpiryBlockCount)

    (startConditions /: hops.reverse) { case Tuple3(payloads, msat, expiry) ~ hop =>
      val feeMsat = nodeFee(hop.lastUpdate.feeBaseMsat, hop.lastUpdate.feeProportionalMillionths, msat)
      val perHopPayload = PerHopPayload(hop.lastUpdate.shortChannelId, msat, expiry) +: payloads
      (perHopPayload, msat + feeMsat, expiry + hop.lastUpdate.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assocData: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size + 1, "Payload count mismatch: there should be one less payload than " + nodes.size)
    val payloadsBin = payloads.map(perHopPayloadCodec.encode).map(serialize) :+ BinaryData("00" * PayloadLength)
    makePacket(PrivateKey(random getBytes 32), nodes, payloadsBin.map(_.toArray), assocData)
  }

  def makeOutgoingSpec(rest: Vector[PaymentRoute], inv: Invoice, firstExpiry: Int) = rest.headOption map { route =>
    val (perHopPayloads, amountWithAllFees, expiryWithAllDeltas) = buildRoute(inv.sum.amount, firstExpiry, route drop 1)
    OutgoingPaymentSpec(invoice = inv, onion = buildOnion(route.map(_.nextNodeId), perHopPayloads, inv.paymentHash),
      preimage = None, routes = rest, expiry = expiryWithAllDeltas, amountWithFee = amountWithAllFees)
  }

  private def without(routes: Vector[PaymentRoute], predicate: Hop => Boolean) = routes.filterNot(_ exists predicate)
  private def withoutChannel(routes: Vector[PaymentRoute], chanId: Long) = without(routes, _.lastUpdate.shortChannelId == chanId)
  private def withoutNode(routes: Vector[PaymentRoute], nodeId: BinaryData) = without(routes, _.nodeId == nodeId)

  def reduceRoutes(fail: UpdateFailHtlc, spec: OutgoingPaymentSpec) =
    parseErrorPacket(spec.onion.sharedSecrets, packet = fail.reason) map {
      case ErrorPacket(nodeId, _: Perm) if spec.invoice.nodeId == nodeId =>
        // Permanent error from a final node, nothing we can do here
        Vector.empty

      case ErrorPacket(nodeId, _: Node) =>
        // Look for channels without this node
        withoutNode(spec.routes, nodeId.toBin)

      case ErrorPacket(nodeId, message: Update) =>
        // This node may have other channels so try them out
        withoutChannel(spec.routes, message.update.shortChannelId)

      case _ =>
        // Just try another
        spec.routes drop 1

      // Could not parse, try another
    } getOrElse spec.routes drop 1

  private def failHtlc(sharedSecret: BinaryData, add: UpdateAddHtlc, failure: FailureMessage) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: PaymentSpecBag) = Try {
    val packet = parsePacket(nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    val payload = LightningMessageCodecs.perHopPayloadCodec decode BitVector(packet.payload)
    Tuple3(payload, packet.nextPacket, packet.sharedSecret)
  } map {
    case (_, nextPacket, sharedSecret) if nextPacket.isLast =>
      // We are the final HTLC recipient, the only viable option

      bag.getInfoByHash(add.paymentHash).map(_.spec) match {
        case Success(spec) if add.amountMsat > spec.invoice.sum.amount * 2 =>
          // GUARD: they have sent too much funds, this is a protective measure
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Success(spec) if add.amountMsat < spec.invoice.sum.amount =>
          // GUARD: amount is less than what we requested, this won't do
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Success(spec) if add.expiry < LNParams.myHtlcExpiry =>
          // GUARD: we may not have enough time until expiration
          failHtlc(sharedSecret, add, FinalExpiryTooSoon)

        case Success(spec: IncomingPaymentSpec) =>
          // We have a valid *incoming* payment spec
          CMDFulfillHtlc(add.id, spec.preimage)

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
