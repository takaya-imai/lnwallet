package com.lightning.wallet.test

import com.lightning.wallet.ln.{LNParams, PaymentOps}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.lightning.wallet.ln.Tools.random
import com.lightning.wallet.ln.crypto.{OnionPacket, ParsedPacket, Sphinx}
import com.lightning.wallet.ln.wire.{ChannelUpdate, Hop, LightningMessageCodecs, PerHopPayload}
import scodec.bits.BitVector


class HtlcGenerationSpec {
  import PaymentOps._

  def randomKey = PrivateKey(random getBytes 32)
  val (priv_a, priv_b, priv_c, priv_d, priv_e) = (randomKey, randomKey, randomKey, randomKey, randomKey)
  val (a, b, c, d, e) = (priv_a.publicKey, priv_b.publicKey, priv_c.publicKey, priv_d.publicKey, priv_e.publicKey)
  val sig = Crypto.encodeSignature(Crypto.sign(BinaryData.empty, priv_a)) :+ 1.toByte
  val defaultChannelUpdate = ChannelUpdate(sig, 0, 0, "0000", 0, 42000, 0, 0)
  val channelUpdate_ab = defaultChannelUpdate.copy(shortChannelId = 1, cltvExpiryDelta = 4, feeBaseMsat = 642000, feeProportionalMillionths = 7)
  val channelUpdate_bc = defaultChannelUpdate.copy(shortChannelId = 2, cltvExpiryDelta = 5, feeBaseMsat = 153000, feeProportionalMillionths = 4)
  val channelUpdate_cd = defaultChannelUpdate.copy(shortChannelId = 3, cltvExpiryDelta = 10, feeBaseMsat = 60000, feeProportionalMillionths = 1)
  val channelUpdate_de = defaultChannelUpdate.copy(shortChannelId = 4, cltvExpiryDelta = 7, feeBaseMsat = 766000, feeProportionalMillionths = 10)

  val currentBlockCount = 420000
  val expiryDeltaBlocks = 144
  // simple route a -> b -> c -> d -> e

  val hops =
    Hop(a, b, channelUpdate_ab) ::
      Hop(b, c, channelUpdate_bc) ::
      Hop(c, d, channelUpdate_cd) ::
      Hop(d, e, channelUpdate_de) :: Nil

  val finalAmountMsat = 42000000L
  val paymentPreimage = BinaryData("42" * 32)
  val paymentHash = Crypto.sha256(paymentPreimage)

  val expiry_de = currentBlockCount + expiryDeltaBlocks
  val amount_de = finalAmountMsat
  val fee_d = nodeFee(channelUpdate_de.feeBaseMsat, channelUpdate_de.feeProportionalMillionths, amount_de)

  val expiry_cd = expiry_de + channelUpdate_de.cltvExpiryDelta
  val amount_cd = amount_de + fee_d
  val fee_c = nodeFee(channelUpdate_cd.feeBaseMsat, channelUpdate_cd.feeProportionalMillionths, amount_cd)

  val expiry_bc = expiry_cd + channelUpdate_cd.cltvExpiryDelta
  val amount_bc = amount_cd + fee_c
  val fee_b = nodeFee(channelUpdate_bc.feeBaseMsat, channelUpdate_bc.feeProportionalMillionths, amount_bc)

  val expiry_ab = expiry_bc + channelUpdate_bc.cltvExpiryDelta
  val amount_ab = amount_bc + fee_b

  def allTests = {

    {
      println("compute fees")

      val feeBaseMsat = 150000L
      val feeProportionalMillionth = 4L
      val htlcAmountMsat = 42000000
      // spec: fee-base-msat + htlc-amount-msat * fee-proportional-millionths / 1000000
      val ref = feeBaseMsat + htlcAmountMsat * feeProportionalMillionth / 1000000
      val fee = nodeFee(feeBaseMsat, feeProportionalMillionth, htlcAmountMsat)
      println(ref == fee)
    }

    {
      println("compute route with fees and expiry delta")

      val (payloads, firstAmountMsat, firstExpiry) = buildRoute(finalAmountMsat, currentBlockCount + expiryDeltaBlocks, hops.drop(1))

      println(firstAmountMsat == amount_ab)
      println(firstExpiry == expiry_ab)
      println(payloads ==
        Vector(PerHopPayload(channelUpdate_bc.shortChannelId, amount_bc, expiry_bc),
          PerHopPayload(channelUpdate_cd.shortChannelId, amount_cd, expiry_cd),
          PerHopPayload(channelUpdate_de.shortChannelId, amount_de, expiry_de)))
    }

    {
      println("build onion")

      val (payloads, _, _) = buildRoute(finalAmountMsat, currentBlockCount + expiryDeltaBlocks, hops.drop(1))
      val nodes = hops.map(_.nextNodeId)
      val OnionPacket(_, packet_b) = buildOnion(nodes.map(PublicKey.apply).toVector, payloads, paymentHash)
      println(packet_b.length == Sphinx.PacketLength)

      // let's peel the onion
      val ParsedPacket(bin_b, packet_c, _) = Sphinx.parsePacket(priv_b, paymentHash, packet_b)
      val payload_b = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_b)).toOption.get.value
      println(packet_c.length == Sphinx.PacketLength)
      println(payload_b.amt_to_forward == amount_bc)
      println(payload_b.outgoing_cltv_value == expiry_bc)

      val ParsedPacket(bin_c, packet_d, _) = Sphinx.parsePacket(priv_c, paymentHash, packet_c)
      val payload_c = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_c)).toOption.get.value
      println(packet_d.length == Sphinx.PacketLength)
      println(payload_c.amt_to_forward == amount_cd)
      println(payload_c.outgoing_cltv_value == expiry_cd)

      val ParsedPacket(bin_d, packet_e, _) = Sphinx.parsePacket(priv_d, paymentHash, packet_d)
      val payload_d = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_d)).toOption.get.value
      println(packet_e.length == Sphinx.PacketLength)
      println(payload_d.amt_to_forward == amount_de)
      println(payload_d.outgoing_cltv_value == expiry_de)

      val ParsedPacket(bin_e, packet_random, _) = Sphinx.parsePacket(priv_e, paymentHash, packet_e)
      println(BinaryData(bin_e) == BinaryData("00" * Sphinx.PayloadLength))
      println(packet_random.length == Sphinx.PacketLength)
    }

    {
      println("build a command including the onion")

      val (amountMsat, onion, expiry) = buildCommand(finalAmountMsat, currentBlockCount + expiryDeltaBlocks, paymentHash, hops)

      println(amountMsat > finalAmountMsat)
      println(expiry == currentBlockCount + expiryDeltaBlocks + channelUpdate_de.cltvExpiryDelta + channelUpdate_cd.cltvExpiryDelta + channelUpdate_bc.cltvExpiryDelta)
      println(onion.onionPacket.length == Sphinx.PacketLength)

      // let's peel the onion
      val ParsedPacket(bin_b, packet_c, _) = Sphinx.parsePacket(priv_b, paymentHash, onion.onionPacket)
      val payload_b = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_b)).toOption.get.value
      println(packet_c.length == Sphinx.PacketLength)
      println(payload_b.amt_to_forward == amount_bc)
      println(payload_b.outgoing_cltv_value == expiry_bc)

      val ParsedPacket(bin_c, packet_d, _) = Sphinx.parsePacket(priv_c, paymentHash, packet_c)
      val payload_c = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_c)).toOption.get.value
      println(packet_d.length == Sphinx.PacketLength)
      println(payload_c.amt_to_forward == amount_cd)
      println(payload_c.outgoing_cltv_value == expiry_cd)

      val ParsedPacket(bin_d, packet_e, _) = Sphinx.parsePacket(priv_d, paymentHash, packet_d)
      val payload_d = LightningMessageCodecs.perHopPayloadCodec.decode(BitVector(bin_d)).toOption.get.value
      println(packet_e.length == Sphinx.PacketLength)
      println(payload_d.amt_to_forward == amount_de)
      println(payload_d.outgoing_cltv_value == expiry_de)

      val ParsedPacket(bin_e, packet_random, _) = Sphinx.parsePacket(priv_e, paymentHash, packet_e)
      println(BinaryData(bin_e) == BinaryData("00" * Sphinx.PayloadLength))
      println(packet_random.length == Sphinx.PacketLength)
    }

    {
      println("build a command with no hops")

      val (amountMsat, onion, expiry) = buildCommand(finalAmountMsat, currentBlockCount + expiryDeltaBlocks, paymentHash, hops.take(1))

      println(amountMsat == finalAmountMsat)
      println(expiry == currentBlockCount + expiryDeltaBlocks)
      println(onion.onionPacket.length == Sphinx.PacketLength)

      // let's peel the onion
      val ParsedPacket(bin_b, packet_random, _) = Sphinx.parsePacket(priv_b, paymentHash, onion.onionPacket)
      println(BinaryData(bin_b) == BinaryData("00" * Sphinx.PayloadLength))
      println(packet_random.length == Sphinx.PacketLength)
    }

  }
}