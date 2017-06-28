package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.ln.PaymentRequest._
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}

import fr.acinq.bitcoin.Bech32.Int5
import java.math.BigInteger
import java.nio.ByteOrder
import scala.util.Try


case class PaymentRequest(prefix: String, amount: Option[MilliSatoshi], unit: Char, timestamp: Long,
                          nodeId: PublicKey, tags: List[Tag], signature: BinaryData) {

  lazy val paymentHash = tags.collectFirst { case hashTag: PaymentHashTag => hashTag.hash }.get
  lazy val fallbackAddress = tags.collectFirst { case btcTag: FallbackAddressTag => btcTag.hash }.get
  lazy val descriptionOpt = tags.collectFirst { case infoTag: DescriptionTag => infoTag.description }
  lazy val routingPaths = tags.collect { case routeTag: RoutingInfoTag => routeTag }
  lazy val data: Int5Seq = Timestamp.encode(timestamp) ++ tags.flatMap(_.toInt5s)

  def isExpired: Boolean = {
    val expiry = tags.collectFirst { case ex: ExpiryTag => ex.seconds }
    timestamp + expiry.getOrElse(3600L) < System.currentTimeMillis / 1000L
  }

  def requestHash: BinaryData = {
    val base = prefix + Amount.encode(amount, unit)
    Crypto.sha256(base.getBytes("UTF-8") ++ data)
  }

  def sign(priv: PrivateKey) = {
    val (r, s) = Crypto.sign(requestHash, priv)
    val (pub1, _) = Crypto.recoverPublicKey(r -> s, requestHash)
    val recid = if (nodeId == pub1) 0.toByte else 1.toByte
    val signature = Signature.encode(r, s, recid)
    copy(signature = signature)
  }

  def address(tag: FallbackAddressTag): String = (prefix, tag.version) match {
    case ("lnbc", 17) => Base58Check.encode(Base58.Prefix.PubkeyAddress, tag.hash)
    case ("lnbc", 18) => Base58Check.encode(Base58.Prefix.ScriptAddress, tag.hash)
    case ("lntb", 17) => Base58Check.encode(Base58.Prefix.PubkeyAddressTestnet, tag.hash)
    case ("lntb", 18) => Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, tag.hash)
    case ("lnbc", 0) => Bech32.encodeWitnessAddress("bc", tag.version, tag.hash)
    case ("lntb", 0) => Bech32.encodeWitnessAddress("tb", tag.version, tag.hash)
  }
}

object PaymentRequest {
  type ByteSeq = Seq[Byte]
  type Int5Seq = Seq[Int5]

  def apply(prefix: String, amount: Option[MilliSatoshi], paymentHash: BinaryData, privateKey: PrivateKey, fallbackAddress: String,
            description: Option[String] = None, expirySeconds: Option[Long] = None, unit: Char = 'm'): PaymentRequest = {

    val requiredTags = PaymentHashTag(paymentHash) :: FallbackAddressTag(fallbackAddress) :: Nil
    val optionalTags = List(description map DescriptionTag, expirySeconds map ExpiryTag).flatten

    PaymentRequest(prefix, amount, unit, timestamp = System.currentTimeMillis / 1000,
      nodeId = privateKey.publicKey, tags = requiredTags ::: optionalTags,
      signature = BinaryData.empty) sign privateKey
  }

  sealed trait Tag {
    def toInt5s: Int5Seq
    def encode(ints: Int5Seq, field: Char): Int5Seq =
      Seq(Bech32 map field, (ints.length / 32).toByte,
        (ints.length % 32).toByte) ++ ints
  }

  object Tag {
    def parse(input: ByteSeq): Tag = {
      val len = input(1) * 32 + input(2)

      input.head match {
        case pTag if pTag == Bech32.map('p') =>
          val hash = Bech32 five2eight input.slice(3, 52 + 3)
          PaymentHashTag(hash)

        case dTag if dTag == Bech32.map('d') =>
          val description = Bech32 five2eight input.slice(3, len + 3)
          val text = new String(description.toArray, "UTF-8")
          DescriptionTag(text)

        case hTag if hTag == Bech32.map('h') =>
          val hash = Bech32 five2eight input.slice(3, len + 3)
          HashTag(hash)

        case fTag if fTag == Bech32.map('f') =>
          val prog = Bech32 five2eight input.slice(4, len - 1 + 4)
          val version = input(3)

          version match {
            case v if v >= 0 && v <= 16 => FallbackAddressTag(version, prog)
            case 17 | 18 => FallbackAddressTag(version, prog)
          }

        case rTag if rTag == Bech32.map('r') =>
          val data = Bech32 five2eight input.slice(3, len + 3)
          val fee = Protocol.uint64(data.drop(33 + 8), ByteOrder.BIG_ENDIAN)
          val cltv = Protocol.uint16(data.drop(33 + 8 + 8), ByteOrder.BIG_ENDIAN)
          RoutingInfoTag(PublicKey(data take 33), data.slice(33, 33 + 8), fee, cltv)

        case xTag if xTag == Bech32.map('x') =>
          require(len == 2, s"Invalid length for expiry tag")
          val expiry = 32  * input(3) + input(4)
          ExpiryTag(expiry)
      }
    }
  }

  case class PaymentHashTag(hash: BinaryData) extends Tag {
    def toInt5s: Int5Seq = encode(Bech32 eight2five hash, 'p')
  }

  case class DescriptionTag(description: String) extends Tag {
    def toInt5s: Int5Seq = encode(Bech32 eight2five description.getBytes("UTF-8"), 'd')
  }

  case class HashTag(hash: BinaryData) extends Tag {
    override def toInt5s: Int5Seq = encode(Bech32 eight2five hash, 'h')
  }

  case class FallbackAddressTag(version: Byte, hash: BinaryData) extends Tag {
    override def toInt5s: Int5Seq = encode(version +: Bech32.eight2five(hash), 'f')
  }

  object FallbackAddressTag {
    def apply(string: String): FallbackAddressTag = {
      // A Bitcoin fallack address MUST always be present
      val try32 = Try apply fromBech32Address(address = string)
      val try58 = Try apply fromBase58Address(address = string)
      try32.orElse(try58).get
    }

    def fromBase58Address(address: String) = Base58Check decode address match {
      case (Base58.Prefix.PubkeyAddressTestnet, hash) => FallbackAddressTag(17, hash)
      case (Base58.Prefix.ScriptAddressTestnet, hash) => FallbackAddressTag(18, hash)
      case (Base58.Prefix.PubkeyAddress, hash) => FallbackAddressTag(17, hash)
      case (Base58.Prefix.ScriptAddress, hash) => FallbackAddressTag(18, hash)
    }

    def fromBech32Address(address: String): FallbackAddressTag = {
      val (prefix, hash) = Bech32 decodeWitnessAddress address
      FallbackAddressTag(prefix, hash)
    }
  }

  case class RoutingInfoTag(pubkey: PublicKey, channelId: BinaryData, fee: Long, cltvExpiryDelta: Int) extends Tag {
    def toInt5s: Int5Seq = encode(Bech32.eight2five(pubkey.toBin ++ channelId ++ feeUint16 ++ cltvExpiryDeltaUint16), 'r')
    val cltvExpiryDeltaUint16 = Protocol.writeUInt16(cltvExpiryDelta, ByteOrder.BIG_ENDIAN)
    val feeUint16 = Protocol.writeUInt64(fee, ByteOrder.BIG_ENDIAN)
  }

  case class ExpiryTag(seconds: Long) extends Tag {
    override def toInt5s: Int5Seq = Seq(Bech32 map 'x', 0.toByte,
      2.toByte, (seconds / 32).toByte, (seconds % 32).toByte)
  }

  object Amount {
    def decode(input: String) = input.lastOption match {
      case Some('m') => (Some(MilliSatoshi apply input.dropRight(1).toLong * 100000000L), 'm')
      case Some('u') => (Some(MilliSatoshi apply input.dropRight(1).toLong * 100000L), 'u')
      case Some('n') => (Some(MilliSatoshi apply input.dropRight(1).toLong * 100L), 'n')
      case Some('p') => (Some(MilliSatoshi apply input.dropRight(1).toLong / 10L), 'p')
      case _ => (None, 'm')
    }

    def encode(amt: Option[MilliSatoshi], unit: Char) = amt match {
      case Some(sum) if unit == 'm' => s"${sum.amount / 100000000L}$unit"
      case Some(sum) if unit == 'u' => s"${sum.amount / 100000L}$unit"
      case Some(sum) if unit == 'n' => s"${sum.amount / 100L}$unit"
      case Some(sum) if unit == 'p' => s"${sum.amount * 10L}$unit"
      case _ => ""
    }
  }

  object Timestamp {
    def decode(data: Int5Seq = Nil): Long = data.take(7).foldLeft(0L) { case (a, b) => a * 32 + b }
    def encode(timestamp: Long, acc: Int5Seq = Nil): Int5Seq = if (acc.length == 7) acc
      else encode(timestamp / 32, (timestamp % 32).toByte +: acc)
  }

  object Signature {
    def decode(signature: BinaryData) = {
      require(signature.length == 65, "Invalid signature length")
      val s = new BigInteger(1, signature.slice(32, 64).toArray)
      val r = new BigInteger(1, signature.take(32).toArray)
      val recid = signature.last
      (r, s, recid)
    }

    def encode(r: BigInteger, s: BigInteger, recid: Byte) = {
      val rEncoded = Crypto fixSize r.toByteArray.dropWhile(0.==)
      val sEncoded = Crypto fixSize s.toByteArray.dropWhile(0.==)
      BinaryData(rEncoded ++ sEncoded :+ recid)
    }
  }

  def read(input: String): PaymentRequest = {
    def loop(tags: Seq[Int5Seq], data: Int5Seq): (Seq[Int5Seq], BinaryData) =

      if (data.length > 104) {
        // 104 is the size of a signature
        val len = 1 + 2 + 32 * data(1) + data(2)
        loop(tags :+ data.take(len), data drop len)
      } else (tags, Bech32 five2eight data)

    val (hrp, data) = Bech32 decode input
    val timestamp = Timestamp decode data

    val (rawtags, signature) = loop(Nil, data drop 7)
    val (r, s, recid) = Signature decode signature
    val tags = rawtags.map(Tag.parse).toList

    val message: BinaryData = hrp.getBytes ++ data.dropRight(104)
    val (pub1, pub2) = Crypto.recoverPublicKey(r -> s, Crypto sha256 message)
    val pub = if (recid % 2 != 0) pub2 else pub1

    val (amountOpt, unit) = Amount.decode(hrp drop 4)
    val pr = PaymentRequest(hrp take 4, amountOpt, unit, timestamp, pub, tags, signature)
    require(Crypto.verifySignature(pr.requestHash, r -> s, pub), "invalid signature")
    pr
  }

  def write(pr: PaymentRequest): String = {
    val hrp = pr.prefix + Amount.encode(pr.amount, pr.unit)
    val data1 = pr.data ++ Bech32.eight2five(pr.signature)
    val checksum: Int5Seq = Bech32.checksum(hrp, data1)
    val body = data1 ++ checksum map Bech32.pam
    hrp + "1" + new String(body.toArray)
  }
}