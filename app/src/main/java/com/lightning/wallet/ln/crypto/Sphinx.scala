package com.lightning.wallet.ln.crypto

import com.lightning.wallet.ln.Exceptions._
import com.lightning.wallet.ln.crypto.Sphinx._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{BinaryData, Crypto, Protocol}

import com.lightning.wallet.ln.wire.FailureMessageCodecs.failureMessageCodec
import com.lightning.wallet.ln.wire.FailureMessage
import com.lightning.wallet.ln.Tools.Bytes
import scodec.bits.BitVector
import java.nio.ByteOrder


case class ParsedPacket(payload: BinaryData, nextPacket: BinaryData, sharedSecret: BinaryData)
case class OnionPacket(sharedSecrets: Vector[BinaryAndKey], onionPacket: BinaryData)
case class ErrorPacket(originNode: PublicKey, failureMessage: FailureMessage)

object Sphinx {
  type ByteVec = Vector[Byte]
  type PublicKeyVec = Vector[PublicKey]
  type BinaryDataVec = Vector[BinaryData]
  type BinaryAndKey = (BinaryData, PublicKey)

  val Version = 1.toByte
  val PayloadLength = 33
  val MacLength = 20
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
  val LAST_PACKET: BinaryData = Version +: zeroes(PacketLength - 1).data
  val LAST_ADDRESS: BinaryData = zeroes(PayloadLength)

  def zeroes(length: Int): BinaryData = Seq.fill[Byte](length)(0)
  def xor(a: ByteVec, b: ByteVec): ByteVec = a zip b map { case (x, y) => x.^(y).&(0xFF).toByte }
  def generateKey(keyType: BinaryData, secret: BinaryData): BinaryData = Digests.hmacSha256(keyType, secret)
  def generateKey(keyType: String, secret: BinaryData): BinaryData = generateKey(keyType getBytes "UTF-8", secret)
  def mac(key: BinaryData, message: BinaryData): BinaryData = Digests.hmacSha256(key, message) take MacLength

  def computeblindingFactor(pub: PublicKey, secret: BinaryData): BinaryData = Crypto.sha256(pub.toBin ++ secret)
  def blind(pub: PublicKey, blindingFactor: BinaryData): PublicKey = PublicKey(pub.multiply(blindingFactor).normalize)
  def blind(pub: PublicKey, blindingFactors: BinaryDataVec): PublicKey = blindingFactors.foldLeft(pub)(blind)

  def computeSharedSecret(pub: PublicKey, secret: PrivateKey): BinaryData =
    Crypto.sha256(pub.multiply(secret).normalize getEncoded true)

  def generateStream(key: BinaryData, length: Int): BinaryData =
    ChaCha20Legacy.process(zeroes(length), key, zeroes(8),
      encrypt = true, skipBlock = false)

  def computeEphemerealPublicKeysAndSharedSecrets(publicKeys: PublicKeyVec, sessionKey: PrivateKey): (PublicKeyVec, BinaryDataVec) = {
    val firstEphemerealPublicKey = blind(pub = PublicKey(value = Crypto.curve.getG, compressed = true), blindingFactor = sessionKey.value)
    val firstSecret: BinaryData = computeSharedSecret(pub = publicKeys.head, secret = sessionKey)
    val firstBlindingFactor = computeblindingFactor(firstEphemerealPublicKey, firstSecret)

    computeEphemerealPublicKeysAndSharedSecrets(publicKeys.tail,
      Vector(firstEphemerealPublicKey), Vector(firstBlindingFactor),
      Vector(firstSecret), sessionKey)
  }

  def computeEphemerealPublicKeysAndSharedSecrets(publicKeys: PublicKeyVec, ephemerealPublicKeys: PublicKeyVec,
                                                  blindingFactors: BinaryDataVec, sharedSecrets: BinaryDataVec,
                                                  sessionKey: PrivateKey): (PublicKeyVec, BinaryDataVec) =

    if (publicKeys.isEmpty) (ephemerealPublicKeys, sharedSecrets) else {
      val nextEphemerealPublicKey = blind(ephemerealPublicKeys.last, blindingFactors.last)
      val nextSecret = computeSharedSecret(blind(publicKeys.head, blindingFactors), sessionKey)
      computeEphemerealPublicKeysAndSharedSecrets(publicKeys.tail, ephemerealPublicKeys :+ nextEphemerealPublicKey,
        blindingFactors :+ computeblindingFactor(nextEphemerealPublicKey, nextSecret), sharedSecrets :+ nextSecret, sessionKey)
    }

  def generateFiller(keyType: String, sharedSecrets: BinaryDataVec,
                     hopSize: Int, maxHops: Int = MaxHops): BinaryData =

    (BinaryData.empty /: sharedSecrets) { case (padding, secret: BinaryData) =>
      val stream = generateStream(generateKey(keyType, secret), (maxHops + 1) * hopSize)
      val pad: ByteVec = (padding ++ zeroes(hopSize).data).toVector
      xor(pad, stream.takeRight(pad.length).toVector)
    }

  case class Packet(v: BinaryData, publicKey: BinaryData, hmac: BinaryData, routingInfo: BinaryData) {
    require(routingInfo.length == (PayloadLength + MacLength) * MaxHops, "Invalid routing info length")
    require(hmac.length == MacLength, s"Onion header hmac length should be exactly $MacLength bytes")
    require(publicKey.length == 33, "Onion header public key length should be exactly 33 bytes")
    def isLastPacket: Boolean = hmac == zeroes(MacLength)
    def version: Int = v.head
  }

  object Packet { me =>
    def read(in: BinaryData): Packet = me read new ByteArrayInputStream(in)
    def write(header: Packet): Bytes = write(new ByteArrayOutputStream(PacketLength), header)
    def isLastPacket(packet: BinaryData): Boolean = read(packet).hmac == zeroes(MacLength)

    private def read(stream: ByteArrayInputStream) =
      MultiStreamUtils.read(stream, 1, 33, MacLength, (PayloadLength + MacLength) * MaxHops) match {
        case Seq(version, publicKey, hmac, routingInfo) => Packet(version, publicKey, hmac, routingInfo)
      }

    private def write(out: ByteArrayOutputStream, header: Packet) =
      MultiStreamUtils.write(out, header.v, header.publicKey,
        header.hmac, header.routingInfo).toByteArray
  }

  def parsePacket(privateKey: PrivateKey, associatedData: BinaryData, rawPacket: BinaryData): ParsedPacket = {
    require(rawPacket.length == PacketLength, s"Onion packet length is ${rawPacket.length}, it should be $PacketLength")

    val packet = Packet.read(rawPacket)
    val sharedSecret = computeSharedSecret(PublicKey(packet.publicKey), privateKey)
    val check: BinaryData = mac(generateKey("mu", sharedSecret), packet.routingInfo ++ associatedData)
    require(check == packet.hmac, "Invalid header mac")

    val routingInfo1 = packet.routingInfo ++ zeroes(PayloadLength + MacLength)
    val length1 = PayloadLength + MacLength + (PayloadLength + MacLength) * MaxHops
    val stream1 = generateStream(generateKey("rho", sharedSecret), length1)
    val bin = xor(routingInfo1.toVector, stream1.data.toVector)

    val hmac = bin.slice(PayloadLength, PayloadLength + MacLength)
    val nextRoutinfo = bin.drop(PayloadLength + MacLength)
    val payload = bin.take(PayloadLength)

    val factor = computeblindingFactor(pub = PublicKey(packet.publicKey), secret = sharedSecret)
    val packet1 = Packet(Version :: Nil, blind(PublicKey(packet.publicKey), factor), hmac, nextRoutinfo)
    ParsedPacket(payload, Packet write packet1, sharedSecret)
  }

  def extractSharedSecrets(packet: BinaryData, privateKey: PrivateKey,
                           associatedData: BinaryData, acc: BinaryDataVec): BinaryDataVec = {

    val ParsedPacket(nextAddress, nextPacket, sharedSecret) = parsePacket(privateKey, associatedData, packet)
    if (nextAddress != LAST_ADDRESS) extractSharedSecrets(nextPacket, privateKey, associatedData, acc :+ sharedSecret)
    else acc :+ sharedSecret
  }

  def makeNextPacket(payload: BinaryData, associatedData: BinaryData,
                     ephemerealPublicKey: BinaryData, sharedSecret: BinaryData,
                     packet: BinaryData, routingInfoFiller: BinaryData): BinaryData = {

    def nextRoutingInfo = {
      val onion = Packet read packet
      val routingInfo1 = payload ++ onion.hmac ++ onion.routingInfo.dropRight(PayloadLength + MacLength)
      val stream1 = generateStream(generateKey("rho", sharedSecret), (PayloadLength + MacLength) * MaxHops)
      xor(routingInfo1.toVector, stream1.data.toVector).dropRight(routingInfoFiller.length) ++ routingInfoFiller
    }

    require(payload.length == PayloadLength)
    val nextHmac: BinaryData = mac(generateKey("mu", sharedSecret), nextRoutingInfo ++ associatedData)
    val nextOnion = Packet(Version :: Nil, ephemerealPublicKey, nextHmac, nextRoutingInfo)
    Packet write nextOnion
  }

  // Builds an encrypted onion packet that contains payloads and routing information for all nodes
  // Returns an onion packet that can be sent to the first node in the list

  def makePacket(sessionKey: PrivateKey, publicKeys: PublicKeyVec, payloads: BinaryDataVec, associatedData: BinaryData) = {
    def loop(packet: BinaryData, hoppayloads: BinaryDataVec, ephkeys: PublicKeyVec, sharedSecrets: BinaryDataVec): BinaryData =
      if (hoppayloads.isEmpty) packet else loop(makeNextPacket(hoppayloads.last, associatedData, ephkeys.last,
        sharedSecrets.last, packet, BinaryData.empty), hoppayloads dropRight 1, ephkeys dropRight 1,
        sharedSecrets dropRight 1)

    val (ephemerealPublicKeys, sharedsecrets) = computeEphemerealPublicKeysAndSharedSecrets(publicKeys, sessionKey)
    val filler = generateFiller(keyType = "rho", sharedsecrets dropRight 1, hopSize = PayloadLength + MacLength, maxHops = MaxHops)
    val lastPacket = makeNextPacket(payloads.last, associatedData, ephemerealPublicKeys.last, sharedsecrets.last, LAST_PACKET, filler)
    val packet = loop(lastPacket, payloads dropRight 1, ephemerealPublicKeys dropRight 1, sharedsecrets dropRight 1)
    OnionPacket(sharedsecrets zip publicKeys, packet)
  }

  // Error packets

  def createErrorPacket(sharedSecret: BinaryData, failure: FailureMessage) = {
    val message = BinaryData(failureMessageCodec.encode(failure).require.toByteArray)
    if (message.length > 128) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = Protocol.writeUInt16(message.length, ByteOrder.BIG_ENDIAN) ++ message ++
      Protocol.writeUInt16(128 - message.length, ByteOrder.BIG_ENDIAN) ++ zeroes(128 - message.length)

    val key = generateKey("um", sharedSecret)
    val packet = mac(key, payload) ++ payload
    forwardErrorPacket(packet, sharedSecret)
  }

  def extractFailureMessage(packet: BinaryData): FailureMessage = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = packet drop Sphinx.MacLength
    val length = Protocol.uint16(payload, ByteOrder.BIG_ENDIAN)
    val failureMessage = BitVector apply payload.slice(2, length + 2)
    failureMessageCodec.decode(failureMessage).require.value
  }

  def forwardErrorPacket(packet: BinaryData, sharedSecret: BinaryData): BinaryData = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)
    val stream = generateStream(generateKey("ammag", sharedSecret), ErrorPacketLength)
    xor(packet.data.toVector, stream.data.toVector)
  }

  def parseErrorPacket(sharedSecrets: Vector[BinaryAndKey], packet: BinaryData): Option[ErrorPacket] = {
    if (packet.length != ErrorPacketLength) throw ChannelException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    sharedSecrets match {
      case (secret, pubkey) +: tail =>
        val packet1 = forwardErrorPacket(packet, secret)
        val (macPart, payload) = packet1.data splitAt MacLength
        val macCheck = mac(Sphinx.generateKey("um", secret), payload)
        val macOk = macPart == macCheck.data

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