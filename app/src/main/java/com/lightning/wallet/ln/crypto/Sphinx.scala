package com.lightning.wallet.ln.crypto

import com.lightning.wallet.ln.Exceptions._
import com.lightning.wallet.ln.crypto.Sphinx._
import com.lightning.wallet.ln.crypto.MultiStreamUtils._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey, Scalar}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import fr.acinq.bitcoin.{Crypto, Protocol}

import com.lightning.wallet.ln.wire.FailureMessageCodecs.failureMessageCodec
import com.lightning.wallet.ln.wire.FailureMessage
import com.lightning.wallet.ln.Tools.Bytes
import org.bitcoinj.core.Sha256Hash
import scodec.bits.BitVector
import java.math.BigInteger
import java.nio.ByteOrder


case class ParsedPacket(payload: Bytes, nextPacket: Bytes, sharedSecret: Bytes)
case class OnionPacket(sharedSecrets: Vector[BytesAndKey], onionPacket: Bytes)
case class ErrorPacket(originNode: PublicKey, failureMessage: FailureMessage)

object Sphinx { me =>
  type BytesVec = Vector[Bytes]
  type PublicKeyVec = Vector[PublicKey]
  type BytesAndKey = (Bytes, PublicKey)

  val Version = 1.toByte
  val PayloadLength = 33
  val MacLength = 32
  val MaxHops = 20

  /*
    error packet format:
    +----------------+----------------------------------+-----------------+----------------------+-----+
    | HMAC(20 bytes) | failure message length (2 bytes) | failure message | pad length (2 bytes) | pad |
    +----------------+----------------------------------+-----------------+----------------------+-----+
    with failure message length + pad length = 128
   */

  val ErrorPacketLength = MacLength + 128 + 2 + 2
  val PacketLength = 1 + 33 + MacLength + MaxHops * (PayloadLength + MacLength)
  val LAST_PACKET: Bytes = Version +: zeroes(PacketLength - 1)
  val LAST_ADDRESS: Bytes = zeroes(PayloadLength)

  def zeroes(length: Int): Bytes = Array.fill[Byte](length)(0)
  def xor(a: Bytes, b: Bytes): Bytes = a zip b map { case (x, y) => x.^(y).&(0xFF).toByte }
  def mac(key: Bytes, message: Bytes): Bytes = Digests.hmacSha256(key, message) take MacLength

  def generateKey(keyType: Bytes, secret: Bytes): Bytes = Digests.hmacSha256(keyType, secret)
  def generateKey(keyType: String, secret: Bytes): Bytes = generateKey(keyType getBytes "UTF-8", secret)
  def computeblindingFactor(pub: PublicKey, secret: Bytes): Bytes = Sha256Hash hash aconcat(pub.toBin, secret)
  def blind(pub: PublicKey, blindingFactors: BytesVec): PublicKey = blindingFactors.foldLeft(pub)(blind)

  def blind(pub: PublicKey, blindingFactor: Bytes): PublicKey = {
    val scalar = Scalar apply new BigInteger(1, blindingFactor)
    PublicKey(pub.multiply(scalar).normalize)
  }

  def computeSharedSecret(pub: PublicKey, secret: PrivateKey): Bytes =
    Sha256Hash.hash(pub.multiply(secret).normalize getEncoded true)

  def generateStream(key: Bytes, length: Int): Bytes =
    ChaCha20Legacy.process(zeroes(length), key, zeroes(8),
      encrypt = true, skipBlock = false)

  def computeEphemerealPublicKeysAndSharedSecrets(publicKeys: PublicKeyVec, sessionKey: PrivateKey): (PublicKeyVec, BytesVec) = {
    val firstEphemerealPublicKey = blind(pub = PublicKey(value = Crypto.curve.getG, compressed = true), sessionKey.value.toByteArray)
    val firstSecret: Bytes = computeSharedSecret(pub = publicKeys.head, secret = sessionKey)
    val firstBlindingFactor = computeblindingFactor(firstEphemerealPublicKey, firstSecret)

    computeEphemerealPublicKeysAndSharedSecrets(publicKeys.tail,
      Vector(firstEphemerealPublicKey), Vector(firstBlindingFactor),
      Vector(firstSecret), sessionKey)
  }

  def computeEphemerealPublicKeysAndSharedSecrets(publicKeys: PublicKeyVec, ephemerealPublicKeys: PublicKeyVec,
                                                  blindingFactors: BytesVec, sharedSecrets: BytesVec,
                                                  sessionKey: PrivateKey): (PublicKeyVec, BytesVec) =

    if (publicKeys.isEmpty) (ephemerealPublicKeys, sharedSecrets) else {
      val nextEphemerealPublicKey = blind(ephemerealPublicKeys.last, blindingFactors.last)
      val nextSecret = computeSharedSecret(blind(publicKeys.head, blindingFactors), sessionKey)
      computeEphemerealPublicKeysAndSharedSecrets(publicKeys.tail, ephemerealPublicKeys :+ nextEphemerealPublicKey,
        blindingFactors :+ computeblindingFactor(nextEphemerealPublicKey, nextSecret), sharedSecrets :+ nextSecret, sessionKey)
    }

  def generateFiller(keyType: String, sharedSecrets: BytesVec,
                     hopSize: Int, maxHops: Int = MaxHops): Bytes =

    (Array.emptyByteArray /: sharedSecrets) { case (padding: Bytes, secret: Bytes) =>
      val stream = generateStream(generateKey(keyType, secret), (maxHops + 1) * hopSize)
      val pad: Bytes = aconcat(padding, me zeroes hopSize)
      xor(pad, stream takeRight pad.length)
    }

  case class Packet(v: Bytes, publicKey: Bytes, routingInfo: Bytes, hmac: Bytes) {
    require(routingInfo.length == (PayloadLength + MacLength) * MaxHops, "Invalid routing info length")
    require(hmac.length == MacLength, s"Onion header hmac length should be exactly $MacLength bytes")
    require(publicKey.length == 33, "Onion header public key length should be exactly 33 bytes")
  }

  object Packet { self =>
    def read(in: Bytes): Packet = self read new ByteArrayInputStream(in)
    def write(packet: Packet): Bytes = write(new ByteArrayOutputStream(PacketLength), packet)
    def isLastPacket(packet: Bytes) = read(packet).hmac sameElements zeroes(MacLength)

    private def read(stream: ByteArrayInputStream) = {
      val routingInfoLength = (PayloadLength + MacLength) * MaxHops
      val res = aread(stream, 1, 33, routingInfoLength, MacLength)
      val Seq(version, publicKey, routingInfo, hmac) = res
      Packet(version, publicKey, routingInfo, hmac)
    }

    private def write(out: ByteArrayOutputStream, header: Packet) =
      awrite(out, header.v, header.publicKey, header.routingInfo, header.hmac).toByteArray
  }

  def parsePacket(privateKey: PrivateKey, associatedData: Bytes, rawPacket: Bytes): ParsedPacket = {
    require(rawPacket.length == PacketLength, s"Onion packet length is ${rawPacket.length}, it should be $PacketLength")

    val packet = Packet read rawPacket
    val message = aconcat(packet.routingInfo, associatedData)
    val sharedSecret = computeSharedSecret(PublicKey(packet.publicKey), privateKey)
    require(mac(generateKey("mu", sharedSecret), message) sameElements packet.hmac, "Invalid header mac")

    val routingInfo1 = aconcat(packet.routingInfo, me zeroes PayloadLength + MacLength)
    val length1 = PayloadLength + MacLength + (PayloadLength + MacLength) * MaxHops
    val stream1 = generateStream(generateKey("rho", sharedSecret), length1)
    val bin = xor(routingInfo1, stream1)

    val hmac = bin.slice(PayloadLength, PayloadLength + MacLength)
    val nextRoutinfo = bin.drop(PayloadLength + MacLength)
    val payload = bin.take(PayloadLength)

    val factor = computeblindingFactor(PublicKey(packet.publicKey), sharedSecret)
    val pubKey = blind(PublicKey(packet.publicKey), factor).toBin.toArray
    val packet1 = Packet(Array(Version), pubKey, nextRoutinfo, hmac)
    ParsedPacket(payload, Packet write packet1, sharedSecret)
  }

  def extractSharedSecrets(packet: Bytes, privateKey: PrivateKey,
                           associatedData: Bytes, acc: BytesVec): BytesVec = {

    val ParsedPacket(nextAddress, nextPacket, sharedSecret) =
      parsePacket(privateKey, associatedData, packet)

    if (nextAddress sameElements LAST_ADDRESS) acc :+ sharedSecret
    else extractSharedSecrets(nextPacket, privateKey, associatedData, acc :+ sharedSecret)
  }

  def makeNextPacket(payload: Bytes, associatedData: Bytes,
                     ephemerealPublicKey: Bytes, sharedSecret: Bytes,
                     packet: Bytes, routingInfoFiller: Bytes): Bytes = {

    def nextRoutingInfo = {
      val onion = Packet read packet
      val stream1 = generateStream(generateKey("rho", sharedSecret), (PayloadLength + MacLength) * MaxHops)
      val routingInfo1 = aconcat(payload, onion.hmac, onion.routingInfo dropRight PayloadLength + MacLength)
      aconcat(xor(routingInfo1, stream1).dropRight(routingInfoFiller.length), routingInfoFiller)
    }

    require(payload.length == PayloadLength)
    val msg = aconcat(nextRoutingInfo, associatedData)
    val nextHmac = mac(key = generateKey("mu", sharedSecret), message = msg)
    val nextOnion = Packet(Array(Version), ephemerealPublicKey, nextRoutingInfo, nextHmac)
    Packet write nextOnion
  }

  // Builds an encrypted onion packet that contains payloads and routing information for all nodes
  // Returns an onion packet that can be sent to the first node in the list

  def makePacket(seskey: PrivateKey, pubKeys: PublicKeyVec, payloads: BytesVec, assocData: Bytes) = {
    def loop(packet: Bytes, hopPayloads: BytesVec, ephemeralKeys: PublicKeyVec, sharedSecrets: BytesVec): Bytes =
      if (hopPayloads.isEmpty) packet else loop(makeNextPacket(hopPayloads.last, assocData, ephemeralKeys.last.toBin,
        sharedSecrets.last, packet, Array.emptyByteArray), hopPayloads dropRight 1, ephemeralKeys dropRight 1,
        sharedSecrets dropRight 1)

    val (ephemerealPublicKeys, sharedsecrets) = computeEphemerealPublicKeysAndSharedSecrets(pubKeys, seskey)
    val filler = generateFiller(keyType = "rho", sharedsecrets dropRight 1, hopSize = PayloadLength + MacLength, maxHops = MaxHops)
    val lastPacket = makeNextPacket(payloads.last, assocData, ephemerealPublicKeys.last.toBin, sharedsecrets.last, LAST_PACKET, filler)
    val packet = loop(lastPacket, payloads dropRight 1, ephemerealPublicKeys dropRight 1, sharedsecrets dropRight 1)
    OnionPacket(sharedsecrets zip pubKeys, packet)
  }

  // Error packets

  def createErrorPacket(sharedSecret: Bytes, failure: FailureMessage) = {
    val message: Bytes = failureMessageCodec.encode(failure).require.toByteArray
    if (message.length > 128) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = aconcat(Protocol.writeUInt16(message.length, ByteOrder.BIG_ENDIAN),
      message, Protocol.writeUInt16(128 - message.length, ByteOrder.BIG_ENDIAN),
        me zeroes 128 - message.length)

    val key: Bytes = generateKey("um", sharedSecret)
    val packet = aconcat(mac(key, payload), payload)
    forwardErrorPacket(packet, sharedSecret)
  }

  def extractFailureMessage(packet: Bytes): FailureMessage = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = packet drop Sphinx.MacLength
    val length = Protocol.uint16(payload, ByteOrder.BIG_ENDIAN)
    val failureMessage = BitVector apply payload.slice(2, length + 2)
    failureMessageCodec.decode(failureMessage).require.value
  }

  def forwardErrorPacket(packet: Bytes, sharedSecret: Bytes): Bytes = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)
    val stream = generateStream(generateKey("ammag", sharedSecret), ErrorPacketLength)
    xor(packet, stream)
  }

  def parseErrorPacket(sharedSecrets: Vector[BytesAndKey], packet: Bytes): Option[ErrorPacket] = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    sharedSecrets match {
      case (secret, pubkey) +: tail =>
        val packet1 = forwardErrorPacket(packet, secret)
        val (providedMac, payload) = packet1 splitAt MacLength
        val macCheck = mac(Sphinx.generateKey("um", secret), payload)
        val macOk = providedMac sameElements macCheck

        if (!macOk) parseErrorPacket(tail, packet1) else {
          val failureMessage = extractFailureMessage(packet1)
          val errPack = ErrorPacket(pubkey, failureMessage)
          Some(errPack)
        }

      case _ =>
        None
    }
  }
}