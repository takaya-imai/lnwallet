package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, sha256}
import com.lightning.wallet.ln.wire.FailureMessageCodecs.BADONION
import com.lightning.wallet.ln.Tools.random
import fr.acinq.bitcoin.BinaryData
import scodec.bits.BitVector
import scodec.Attempt
import scala.util.Try


object PaymentOps {
  // First amount is larger than final amount since intermediary nodes collect fees
  // The fee (in msat) that a node should be paid to forward an HTLC of 'amount' millisatoshis
  def nodeFee(baseMsat: Long, proportional: Long, msat: Long): Long = baseMsat + (proportional * msat) / 1000000
  def buildCommand(finalAmountMsat: Long, finalExpiryBlockCount: Int, paymentHash: BinaryData, hops: PaymentRoute) = {
    val (perHopPayloads, firstAmountMsat, firstExpiry) = buildRoute(finalAmountMsat, finalExpiryBlockCount, hops drop 1)
    val onion = buildOnion(hops.map(PublicKey apply _.nextNodeId).toVector, perHopPayloads, paymentHash)
    (firstAmountMsat, onion, firstExpiry)
  }

  def buildRoute(finalAmountMsat: Long, finalExpiryBlockCount: Int, hops: PaymentRoute) = {
    val startConditions = (Vector.empty[PerHopPayload], finalAmountMsat, finalExpiryBlockCount)
    hops.reverse.foldLeft(startConditions) { case (Tuple3(payloads, msat: Long, expiry: Int), hop) =>
      val feeMsat = nodeFee(hop.lastUpdate.feeBaseMsat, hop.lastUpdate.feeProportionalMillionths, msat)
      val perHopPayload1 = PerHopPayload(hop.lastUpdate.shortChannelId, msat, expiry) +: payloads
      (perHopPayload1, msat + feeMsat, expiry + hop.lastUpdate.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assocData: BinaryData): SecretsAndPacket = {
    require(nodes.size == payloads.size + 1, "Payload count mismatch: there should be one less payload than " + nodes.size)
    val payloadsBin = payloads.map(perHopPayloadCodec.encode).map(serializationResult) :+ BinaryData("00" * PayloadLength)
    makePacket(PrivateKey(random getBytes 32), nodes, payloadsBin.map(_.toArray), assocData)
  }

  def reduceRoutes(fail: UpdateFailHtlc, packet: SecretsAndPacket,
                   ops: PaymentRouteOps): List[PaymentRoute] =

    parseErrorPacket(packet.sharedSecrets, fail.reason) map {
      case ErrorPacket(nodeId, _: Perm) if ops.targetNodeId == nodeId =>
        // Permanent error from target node, nothing we can do here
        Nil

      case ErrorPacket(nodeId, message: Update) =>
        // This node may have other channels so try them
        val failedChannelId = message.update.shortChannelId
        PaymentRouteOps.withoutFailedChannel(ops, failedChannelId)

      case ErrorPacket(nodeId, _: Node) =>
        // This node has failed so try to use routes without it
        PaymentRouteOps.withoutFailedNode(ops, nodeId.toBin)

      case ErrorPacket(nodeId, _) =>
        // Just try the other route
        ops.remaining.tail

    } getOrElse Nil

  private def failHtlc(sharedSecret: BinaryData, add: UpdateAddHtlc, failure: FailureMessage) =
    CMDFailHtlc(reason = createErrorPacket(sharedSecret, failure), id = add.id)

  def resolveHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc, bag: InvoiceBag) = Try {
    val packet = parsePacket(nodeSecret, associatedData = add.paymentHash, add.onionRoutingPacket)
    val payload = LightningMessageCodecs.perHopPayloadCodec decode BitVector(packet.payload)
    Tuple3(payload, packet.nextPacket, packet.sharedSecret)
  } map {
    case (_, nextPacket, sharedSecret) if nextPacket.isLast =>
      // We are the final recipient of HTLC, the only viable option

      bag getExtendedInvoice add.paymentHash match {
        case Some(inv) if add.amountMsat > inv.invoice.sum.amount * 2 =>
          // They have sent too much funds, this is a protective measure
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Some(inv) if add.amountMsat < inv.invoice.sum.amount =>
          // An amount is less than what we requested, this won't do
          failHtlc(sharedSecret, add, IncorrectPaymentAmount)

        case Some(inv) if inv.preimage.isDefined =>
          CMDFulfillHtlc(add.id, inv.preimage.get)

        case None =>
          // Could not find a payment preimage
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
