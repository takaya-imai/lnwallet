package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.crypto._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.lightning.wallet.ln.Tools.random
import scodec.bits.BitVector
import scodec.Attempt
import scala.util.Try


object PaymentOps {
  // First amount is larger than final amount since intermediary nodes collect fees
  // The fee (in msat) that a node should be paid to forward an HTLC of 'amount' millisatoshis
  def nodeFee(baseMsat: Long, proportional: Long, msat: Long): Long = baseMsat + (proportional * msat) / 1000000
  def buildCommand(finalAmountMsat: Long, finalExpiryBlockCount: Int, paymentHash: BinaryData, hops: PaymentRoute) = {
    val (perHopPayloads, firstAmountMsat, firstExpiry) = buildRoute(finalAmountMsat, finalExpiryBlockCount, hops drop 1)
    (firstAmountMsat, buildOnion(hops.map(PublicKey apply _.nextNodeId).toVector, perHopPayloads, paymentHash), firstExpiry)
  }

  def buildRoute(finalAmountMsat: Long, finalExpiryBlockCount: Int, hops: PaymentRoute) = {
    val startConditions = (Vector.empty[PerHopPayload], finalAmountMsat, finalExpiryBlockCount)

    hops.reverse.foldLeft(startConditions) { case (Tuple3(payloads, msat, expiry), hop) =>
      val feeMsat = nodeFee(hop.lastUpdate.feeBaseMsat, hop.lastUpdate.feeProportionalMillionths, msat)
      val perHopPayload1 = PerHopPayload(hop.lastUpdate.shortChannelId, msat, expiry) +: payloads
      (perHopPayload1, msat + feeMsat, expiry + hop.lastUpdate.cltvExpiryDelta)
    }
  }

  def buildOnion(nodes: PublicKeyVec, payloads: Vector[PerHopPayload], assocData: BinaryData): OnionPacket = {
    require(nodes.size == payloads.size + 1, s"Сount mismatch: there should be one less payload than ${nodes.size}")
    val payloadsBin = payloads.map(perHopPayloadCodec.encode).map(serializationResult) :+ BinaryData("00" * PayloadLength)
    makePacket(PrivateKey(random getBytes 32), nodes, payloadsBin.map(_.toArray), assocData)
  }

  def reduceRoutes(fail: UpdateFailHtlc, packet: OnionPacket,
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

  def failHtlc(nodeSecret: PrivateKey, htlc: Htlc, failure: FailureMessage): CMDFailHtlc = {
    val parsedPacked = parsePacket(nodeSecret, htlc.add.paymentHash, htlc.add.onionRoutingPacket)
    val errorPacket = createErrorPacket(parsedPacked.sharedSecret, failure)
    CMDFailHtlc(htlc.add.id, errorPacket)
  }

  def parseIncomingHtlc(nodeSecret: PrivateKey, add: UpdateAddHtlc) = Try {
    val packet: ParsedPacket = parsePacket(nodeSecret, add.paymentHash, add.onionRoutingPacket)
    val payload = LightningMessageCodecs.perHopPayloadCodec decode BitVector(packet.payload)
    Tuple3(payload, packet.nextPacket, packet.sharedSecret)
  } map {
    case (_, nextPacket, sharedSecret) if Packet isLastPacket nextPacket =>
      // Looks like we are the final recipient of HTLC, the only viable option
      Right(add, sharedSecret)

    case (Attempt.Successful(_), _, sharedSecret) =>
      // We don't route so could not resolve downstream node address
      val reason = createErrorPacket(sharedSecret, UnknownNextPeer)
      val fail = CMDFailHtlc(add.id, reason)
      Left(fail)

    case (Attempt.Failure(_), _, sharedSecret) =>
      // Payload could not be parsed at all so we can only fail an HTLC
      val reason = createErrorPacket(sharedSecret, PermanentNodeFailure)
      val fail = CMDFailHtlc(add.id, reason)
      Left(fail)

  } getOrElse {
    val code = FailureMessageCodecs.BADONION
    val hash = Crypto sha256 add.onionRoutingPacket
    val fail = CMDFailMalformedHtlc(add.id, hash, code)
    Left(fail)
  }
}
