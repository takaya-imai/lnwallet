package com.lightning.wallet.lncloud

import spray.json._
import spray.json.DefaultJsonProtocol._
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import com.lightning.wallet.ln.crypto.{Packet, SecretsAndPacket}
import com.lightning.wallet.lncloud.LNCloud.{ClearToken, InvoiceAndMemo}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.{hopsCodec, lightningMessageCodec}
import com.lightning.wallet.ln.{IncomingPaymentSpec, Invoice, OutgoingPaymentSpec, PaymentSpec}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.PaymentRoute
import com.lightning.wallet.ln.crypto.Sphinx.BytesAndKey
import com.lightning.wallet.lncloud.RatesSaver.RatesMap
import com.lightning.wallet.ln.wire.LightningMessage
import org.bitcoinj.core.ECKey.CURVE.getCurve
import com.lightning.wallet.ln.Tools.Bytes
import org.spongycastle.math.ec.ECPoint
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.Coin
import scodec.bits.BitVector
import java.math.BigInteger


object ImplicitJsonFormats { me =>
  val json2String = (_: JsValue).convertTo[String]
  def json2BitVec(json: JsValue): Option[BitVector] =
    BitVector fromHex json2String(json)

  implicit object BigIntegerFmt extends JsonFormat[BigInteger] {
    def read(json: JsValue) = new BigInteger(me json2String json)
    def write(internal: BigInteger) = internal.toString.toJson
  }

  implicit object ECPointFmt extends JsonFormat[ECPoint] {
    def read(json: JsValue) = (json2String andThen HEX.decode andThen getCurve.decodePoint)(json)
    def write(internal: ECPoint): JsValue = HEX.encode(internal getEncoded true).toJson
  }

  implicit object LNCloudActFmt extends JsonFormat[LNCloudAct] {
    def read(json: JsValue) = json.asJsObject fields "kind" match {
      case JsString("PingCloudAct") => json.convertTo[PingCloudAct]
      case _ => throw new RuntimeException
    }

    def write(internal: LNCloudAct) = internal match {
      case pingCloudAct: PingCloudAct => pingCloudAct.toJson
    }
  }

  implicit object PaymentSpecFmt extends JsonFormat[PaymentSpec] {
    def read(json: JsValue) = json.asJsObject fields "kind" match {
      case JsString("OutgoingPaymentSpec") => json.convertTo[OutgoingPaymentSpec]
      case JsString("IncomingPaymentSpec") => json.convertTo[IncomingPaymentSpec]
      case _ => throw new RuntimeException
    }

    def write(internal: PaymentSpec) = internal match {
      case outgoingPaymentSpec: OutgoingPaymentSpec => outgoingPaymentSpec.toJson
      case incomingPaymentSpec: IncomingPaymentSpec => incomingPaymentSpec.toJson
    }
  }

  implicit object CoinFmt extends JsonFormat[Coin] {
    def read(json: JsValue) = Coin valueOf json.convertTo[Long]
    def write(coin: Coin) = coin.value.toJson
  }

  implicit object LightningMessageFmt extends JsonFormat[LightningMessage] {
    def read(rawJson: JsValue) = lightningMessageCodec.decode(json2BitVec(rawJson).get).require.value
    def write(message: LightningMessage) = lightningMessageCodec.encode(message).require.toHex.toJson
  }

  implicit object PaymentRouteFmt extends JsonFormat[PaymentRoute] {
    def read(rawJson: JsValue) = hopsCodec.decode(json2BitVec(rawJson).get).require.value
    def write(route: PaymentRoute) = hopsCodec.encode(route).require.toHex.toJson
  }

  implicit val blindParamFmt = jsonFormat[Bytes, BigInteger, BigInteger, BigInteger, BigInteger,
    BlindParam](BlindParam.apply, "point", "a", "b", "c", "bInv")

  implicit val blindMemoFmt = jsonFormat[List[BlindParam], List[BigInteger], String,
    BlindMemo](BlindMemo.apply, "params", "clears", "sesPubKeyHex")

  implicit val pointFmt = jsonFormat[ECPoint, Point](Point.apply, "value")
  implicit val publicKeyFmt = jsonFormat[Point, Boolean, PublicKey](PublicKey.apply, "value", "compressed")
  implicit val MilliSatoshiFmt = jsonFormat[Long, MilliSatoshi](MilliSatoshi.apply, "amount")
  implicit val binaryDataFmt = jsonFormat[Seq[Byte], BinaryData](BinaryData.apply, "data")

  implicit val invoiceFmt = jsonFormat[Option[String], PublicKey, MilliSatoshi, BinaryData,
    Invoice](Invoice.apply, "message", "nodeId", "sum", "paymentHash")

  implicit val packetFmt = jsonFormat[Bytes, Bytes, Bytes, Bytes,
    Packet](Packet.apply, "v", "publicKey", "routingInfo", "hmac")

  implicit val secretsAndPacketFmt = jsonFormat[Vector[BytesAndKey], Packet,
    SecretsAndPacket](SecretsAndPacket.apply, "sharedSecrets", "packet")

  implicit val outgoingPaymentSpecFmt = jsonFormat[Invoice, String, Long,
    Option[BinaryData], Long, Vector[PaymentRoute], SecretsAndPacket, Long, String,
    OutgoingPaymentSpec](OutgoingPaymentSpec.apply, "invoice", "status", "stamp",
    "preimage", "expiry", "routes", "onion", "amountWithFee", "kind")

  implicit val incomingPaymentSpecFmt = jsonFormat[Invoice, String, Long, BinaryData, String,
    IncomingPaymentSpec](IncomingPaymentSpec.apply, "invoice", "status", "stamp", "preimage", "kind")

  implicit val ratesFmt = jsonFormat[RatesMap, Seq[Double], Coin, Coin, Long,
    Rates](Rates.apply, "exchange", "feeHistory", "feeLive", "feeRisky", "stamp")

  implicit val pingLNCLoudAct = jsonFormat[String, String,
    PingCloudAct](PingCloudAct.apply, "data", "kind")

  implicit val lnCloudDataFmt = jsonFormat[Option[InvoiceAndMemo], List[ClearToken], List[LNCloudAct],
    LNCloudData](LNCloudData.apply, "info", "tokens", "acts")

  implicit val lbCloudDataPrivateFmt = jsonFormat[List[LNCloudAct], String,
    LNCloudDataPrivate](LNCloudDataPrivate.apply, "acts", "url")

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")
}
