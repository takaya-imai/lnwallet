package com.lightning.wallet.ln.crypto

import com.lightning.wallet.ln.Exceptions._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{BinaryData, Crypto, Protocol}
import com.lightning.wallet.ln.wire.FailureMessageCodecs.failureMessageCodec
import com.lightning.wallet.ln.wire.FailureMessage
import com.lightning.wallet.ln.Tools.Bytes
import scodec.bits.BitVector
import java.nio.ByteOrder


object Sphinx {
  type ByteSeq = Seq[Byte]
  type PublicKeySeq = Seq[PublicKey]
  type BinaryDataSeq = Seq[BinaryData]
  type BinaryAndKey = (BinaryData, PublicKey)

  val maxHops = 20
  val macLength = 20
  val addressLength = 20
  val perHopPayloadLength = 20
  val errLength = macLength + 128 + 2 + 2

  val headerLength = 1 + 33 + macLength + maxHops * (addressLength + macLength)
  val packetLength = headerLength + maxHops * perHopPayloadLength

  val LAST_ADDRESS: BinaryData = zeroes(addressLength)
  val LAST_PACKET: BinaryData = 1.toByte +: zeroes(packetLength - 1)

  case class ErrorPacket(originNode: PublicKey, failureMessage: FailureMessage)
  case class OnionPacket(sharedSecrets: Seq[BinaryAndKey], onionPacket: BinaryData)

  def zeroes(length: Int): BinaryData = Seq.fill[Byte](length)(0)
  def xor(a: ByteSeq, b: ByteSeq): ByteSeq = a zip b map { case (x, y) => x.^(y).&(0xFF).toByte }
  def generateKey(keyType: BinaryData, secret: BinaryData): BinaryData = Digests.hmacSha256(keyType, secret)
  def generateKey(keyType: String, secret: BinaryData): BinaryData = generateKey(keyType getBytes "UTF-8", secret)
  def mac(key: BinaryData, message: BinaryData): BinaryData = Digests.hmacSha256(key, message) take macLength

  def computeblindingFactor(pub: PublicKey, secret: BinaryData): BinaryData = Crypto.sha256(pub.toBin ++ secret)
  def blind(pub: PublicKey, blindingFactor: BinaryData): PublicKey = PublicKey(pub.multiply(blindingFactor).normalize)
  def blind(pub: PublicKey, blindingFactors: BinaryDataSeq): PublicKey = blindingFactors.foldLeft(pub)(blind)

  def computeSharedSecret(pub: PublicKey, secret: PrivateKey): BinaryData =
    Crypto.sha256(pub.multiply(secret).normalize getEncoded true)

  def generateStream(key: BinaryData, length: Int): BinaryData =
    ChaCha20Legacy.process(zeroes(length), key, zeroes(8),
      encrypt = true, skipBlock = false)

  def computeEphemerealPublicKeysAndSharedSecrets(sessionKey: PrivateKey, publicKeys: PublicKeySeq): (PublicKeySeq, BinaryDataSeq) = {
    val firstEphemerealPublicKey = blind(PublicKey(Crypto.curve.getG, compressed = true), sessionKey.value)
    val firstSecret = computeSharedSecret(publicKeys.head, sessionKey)

    computeEphemerealPublicKeysAndSharedSecrets(sessionKey, publicKeys.tail, firstEphemerealPublicKey :: Nil,
      computeblindingFactor(firstEphemerealPublicKey, firstSecret) :: Nil, firstSecret :: Nil)
  }

  def computeEphemerealPublicKeysAndSharedSecrets(sessionKey: PrivateKey, publicKeys: PublicKeySeq,
                                                  ephemerealPublicKeys: PublicKeySeq, blindingFactors: BinaryDataSeq,
                                                  sharedSecrets: BinaryDataSeq): (PublicKeySeq, BinaryDataSeq) =

    if (publicKeys.isEmpty) (ephemerealPublicKeys, sharedSecrets) else {
      val nextEphemerealPublicKey = blind(ephemerealPublicKeys.last, blindingFactors.last)
      val nextSecret = computeSharedSecret(blind(publicKeys.head, blindingFactors), sessionKey)

      computeEphemerealPublicKeysAndSharedSecrets(sessionKey, publicKeys.tail, ephemerealPublicKeys :+ nextEphemerealPublicKey,
        blindingFactors :+ computeblindingFactor(nextEphemerealPublicKey, nextSecret), sharedSecrets :+ nextSecret)
    }

  def generateFiller(keyType: String, sharedSecrets: BinaryDataSeq, hopSize: Int,
                     maxNumberOfHops: Int = maxHops): BinaryData =

    (BinaryData.empty /: sharedSecrets) {
      case (padding, secret: BinaryData) =>
        val padding1 = padding ++ zeroes(hopSize)
        xor(padding1, generateStream(generateKey(keyType, secret),
          (maxNumberOfHops + 1) * hopSize) takeRight padding1.length)
    }

  case class Header(v: BinaryData, publicKey: BinaryData, hmac: BinaryData, routingInfo: BinaryData) {
    require(routingInfo.length == (addressLength + macLength) * maxHops, "Invalid routing info length")
    require(hmac.length == macLength, s"Onion header hmac length should be exactly $macLength bytes")
    require(publicKey.length == 33, "Onion header public key length should be exactly 33 bytes")
    def version = v.head
  }

  object Header { me =>
    def read(in: BinaryData): Header = me read new ByteArrayInputStream(in)
    def write(header: Header): Bytes = write(new ByteArrayOutputStream(headerLength), header)

    private def read(stream: ByteArrayInputStream) =
      MultiStreamUtils.read(stream, 1, 33, macLength, (addressLength + macLength) * maxHops) match {
        case Seq(version, publicKey, hmac, routingInfo) => Header(version, publicKey, hmac, routingInfo)
      }

    private def write(out: ByteArrayOutputStream, header: Header) =
      MultiStreamUtils.write(out, header.v, header.publicKey,
        header.hmac, header.routingInfo).toByteArray
  }

  case class ParsedPacket(payload: BinaryData, nextAddress: BinaryData, nextPacket: BinaryData, sharedSecret: BinaryData)
  def parsePacket(privateKey: PrivateKey, associatedData: BinaryData, packet: BinaryData): ParsedPacket = {

    val header = Header read packet
    val perHopPayload = packet drop headerLength
    val sharedSecret = computeSharedSecret(PublicKey(header.publicKey), privateKey)
    val checkData = header.routingInfo ++ perHopPayload ++ associatedData
    val muKey = generateKey("mu", sharedSecret)

    require(header.hmac == mac(muKey, checkData),
      "Onion header mac check has failed")

    val rho = generateKey("rho", sharedSecret)
    val rhoStream = generateStream(rho, addressLength + macLength + (addressLength + macLength) * maxHops)
    val bin = xor(header.routingInfo ++ zeroes(addressLength + macLength), rhoStream)

    val address = bin take addressLength
    val hmac = bin.slice(addressLength, addressLength + macLength)
    val nextRoutinfo = bin.drop(addressLength + macLength)

    val factor = computeblindingFactor(PublicKey(header.publicKey), sharedSecret)
    val nextPubKey = blind(PublicKey(header.publicKey), factor)

    val gamma = generateKey("gamma", sharedSecret)
    val gammaStream = generateStream(gamma, perHopPayloadLength + maxHops * perHopPayloadLength)
    val bin1 = xor(perHopPayload ++ zeroes(perHopPayloadLength), gammaStream)

    val payload = bin1 take perHopPayloadLength
    val nextPerHopPayloads = bin1 drop perHopPayloadLength
    val header1 = Header(1.toByte :: Nil, nextPubKey, hmac, nextRoutinfo)
    ParsedPacket(payload, address, Header.write(header1) ++ nextPerHopPayloads, sharedSecret)
  }

  def extractSharedSecrets(packet: BinaryData, privateKey: PrivateKey,
                           associatedData: BinaryData, acc: BinaryDataSeq = Nil): BinaryDataSeq =

    parsePacket(privateKey, associatedData, packet) match {
      case ParsedPacket(_, nextAddress, _, ss) if nextAddress == LAST_ADDRESS => acc :+ ss
      case ParsedPacket(_, _, np, ss) => extractSharedSecrets(np, privateKey, associatedData, acc :+ ss)
    }

  def makeNextPacket(address: BinaryData, payload: BinaryData, associatedData: BinaryData, ephemerealPublicKey: BinaryData,
                     sharedSecret: BinaryData, packet: BinaryData, routingInfoFiller: BinaryData = BinaryData.empty,
                     payloadsFiller: BinaryData = BinaryData.empty): BinaryData = {

    val header = Header read packet
    val hopPayloads = packet drop headerLength

    val nextRoutingInfo = {
      val stream = generateStream(generateKey("rho", sharedSecret), (addressLength + macLength) * maxHops)
      val routingInfo1 = address ++ header.hmac ++ header.routingInfo.dropRight(addressLength + macLength)
      xor(routingInfo1, stream).dropRight(routingInfoFiller.length) ++ routingInfoFiller
    }

    val nexHopPayloads = {
      val hopPayloads1 = payload ++ hopPayloads.dropRight(perHopPayloadLength)
      val stream = generateStream(generateKey("gamma", sharedSecret), maxHops * perHopPayloadLength)
      xor(hopPayloads1, stream).dropRight(payloadsFiller.length) ++ payloadsFiller
    }

    val checkData = nextRoutingInfo ++ nexHopPayloads ++ associatedData
    val nextHmac: BinaryData = mac(generateKey("mu", sharedSecret), checkData.toArray)
    val nextHeader = Header(1.toByte :: Nil, ephemerealPublicKey, nextHmac, nextRoutingInfo)
    Header.write(nextHeader) ++ nexHopPayloads
  }

  // Builds an encrypted onion packet that contains payloads and routing information for all nodes
  // Returns an onion packet that can be sent to the first node in the list

  def makePacket(sessionKey: PrivateKey, publicKeys: PublicKeySeq, payloads: BinaryDataSeq, associatedData: BinaryData): OnionPacket = {
    def loop(packet: BinaryData, pubKeys: PublicKeySeq, hopPayloads: BinaryDataSeq, ephKeys: PublicKeySeq, ss: BinaryDataSeq): BinaryData =
      if (hopPayloads.isEmpty) packet else loop(makeNextPacket(pubKeys.last.hash160, hopPayloads.last, associatedData, ephKeys.last,
        ss.last, packet), pubKeys dropRight 1, hopPayloads dropRight 1, ephKeys dropRight 1, ss dropRight 1)

    val (ephemerealPublicKeys, sharedsecrets) = computeEphemerealPublicKeysAndSharedSecrets(sessionKey, publicKeys)
    val filler = generateFiller("rho", sharedsecrets dropRight 1, addressLength + macLength, maxHops)
    val hopFiller = generateFiller("gamma", sharedsecrets dropRight 1, perHopPayloadLength, maxHops)

    val packet = loop(makeNextPacket(LAST_ADDRESS, payloads.last, associatedData, ephemerealPublicKeys.last,
      sharedsecrets.last, LAST_PACKET, filler, hopFiller), publicKeys, payloads dropRight 1,
      ephemerealPublicKeys dropRight 1, ss = sharedsecrets dropRight 1)

    OnionPacket(sharedsecrets zip publicKeys, packet)
  }

  // Error packets

  def createErrorPacket(sharedSecret: BinaryData, failure: FailureMessage) = {
    val message = BinaryData(failureMessageCodec.encode(failure).require.toByteArray)
    if (message.length > 128) throw new RuntimeException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = Protocol.writeUInt16(message.length, ByteOrder.BIG_ENDIAN) ++
      message ++ Protocol.writeUInt16(128 - message.length, ByteOrder.BIG_ENDIAN) ++
      zeroes(128 - message.length)

    val key = generateKey("um", sharedSecret)
    val packet = mac(key, payload) ++ payload
    forwardErrorPacket(packet, sharedSecret)
  }

  def extractFailureMessage(packet: BinaryData): FailureMessage = {
    if (packet.length != errLength) throw new RuntimeException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val payload = packet.data drop macLength
    val len = Protocol.uint16(payload, ByteOrder.BIG_ENDIAN)
    if (len > 128) throw new RuntimeException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    val vec = BitVector apply payload.slice(2, len + 2)
    failureMessageCodec.decode(vec).require.value
  }

  def forwardErrorPacket(packet: BinaryData, sharedSecret: BinaryData): BinaryData = {
    if (packet.length != errLength) throw new RuntimeException(SPHINX_ERR_PACKET_WRONG_LENGTH)
    val filler = generateFiller("ammag", sharedSecret :: Nil, errLength, maxNumberOfHops = 1)
    xor(packet, filler)
  }

  def checkMac(sharedSecret: BinaryData, packet: BinaryData): Boolean =
    packet.data splitAt macLength match { case (mac1, payload) =>
      val um = Sphinx.generateKey("um", sharedSecret)
      BinaryData(mac1) == mac(um, payload)
    }

  def parseErrorPacket(sharedSecrets: Seq[BinaryAndKey], packet: BinaryData): Option[ErrorPacket] = {
    if (packet.length != errLength) throw new RuntimeException(SPHINX_ERR_PACKET_WRONG_LENGTH)

    sharedSecrets match {
      case (secret, pubkey) :: tail =>
        val packet1 = forwardErrorPacket(packet, secret)
        val ok = checkMac(sharedSecret = secret, packet1)

        if (!ok) parseErrorPacket(tail, packet1) else {
          val failureMessage = extractFailureMessage(packet1)
          val packet = ErrorPacket(pubkey, failureMessage)
          Some(packet)
        }

      case Nil =>
        None
    }
  }

}