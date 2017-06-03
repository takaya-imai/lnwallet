package com.lightning.wallet.lncloud

import spray.json._
import spray.json.DefaultJsonProtocol._
import fr.acinq.bitcoin.Crypto.{Point, PublicKey}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Transaction}
import com.lightning.wallet.ln.crypto.{Packet, SecretsAndPacket}
import com.lightning.wallet.lncloud.LNCloud.{ClearToken, InvoiceAndMemo}
import com.lightning.wallet.ln.wire.{LightningMessage, NodeAnnouncement}
import com.lightning.wallet.ln.{IncomingPaymentSpec, Invoice, OutgoingPaymentSpec, PaymentSpec}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.{hopsCodec, lightningMessageCodec, nodeAnnouncementCodec}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.PaymentRoute
import com.lightning.wallet.ln.crypto.Sphinx.BytesAndKey
import com.lightning.wallet.lncloud.RatesSaver.RatesMap
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
    def read(json: JsValue): BigInteger = new BigInteger(me json2String json)
    def write(internal: BigInteger): JsValue = internal.toString.toJson
  }

  implicit object BinaryDataFmt extends JsonFormat[BinaryData] {
    def read(json: JsValue): BinaryData = BinaryData(me json2String json)
    def write(internal: BinaryData): JsValue = internal.toString.toJson
  }

  implicit object TransactionFmt extends JsonFormat[Transaction] {
    def read(json: JsValue): Transaction = Transaction.read(me json2String json)
    def write(internal: Transaction): JsValue = Transaction.write(internal).toString.toJson
  }

  implicit object ECPointFmt extends JsonFormat[ECPoint] {
    def read(json: JsValue) = (json2String andThen HEX.decode andThen getCurve.decodePoint)(json)
    def write(internal: ECPoint): JsValue = HEX.encode(internal getEncoded true).toJson
  }

  implicit object LNCloudActFmt extends JsonFormat[LNCloudAct] {
    def read(json: JsValue) = json.asJsObject fields "kind" match {
      case JsString("CheckCloudAct") => json.convertTo[CheckCloudAct]
      case _ => throw new RuntimeException
    }

    def write(internal: LNCloudAct) = internal match {
      case pingCloudAct: CheckCloudAct => pingCloudAct.toJson
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

  implicit object NodeAnnouncementFmt extends JsonFormat[NodeAnnouncement] {
    def read(rawJson: JsValue) = nodeAnnouncementCodec.decode(json2BitVec(rawJson).get).require.value
    def write(message: NodeAnnouncement) = nodeAnnouncementCodec.encode(message).require.toHex.toJson
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

  implicit val invoiceFmt = jsonFormat[Option[String], PublicKey, MilliSatoshi, BinaryData,
    Invoice](Invoice.apply, "message", "nodeId", "sum", "paymentHash")

  implicit val packetFmt = jsonFormat[Bytes, Bytes, Bytes, Bytes,
    Packet](Packet.apply, "v", "publicKey", "routingInfo", "hmac")

  implicit val secretsAndPacketFmt = jsonFormat[Vector[BytesAndKey], Packet,
    SecretsAndPacket](SecretsAndPacket.apply, "sharedSecrets", "packet")

  implicit val outgoingPaymentSpecFmt = jsonFormat[Invoice, Option[BinaryData], Vector[PaymentRoute], SecretsAndPacket, Long, Long, String,
    OutgoingPaymentSpec](OutgoingPaymentSpec.apply, "invoice", "preimage", "routes", "onion", "amountWithFee", "expiry", "kind")

  implicit val incomingPaymentSpecFmt = jsonFormat[Invoice, BinaryData, String,
    IncomingPaymentSpec](IncomingPaymentSpec.apply, "invoice", "preimage", "kind")

  implicit val ratesFmt = jsonFormat[Seq[Double], RatesMap, Long,
    Rates](Rates.apply, "feeHistory", "exchange", "stamp")

  implicit val pingLNCLoudAct = jsonFormat[BinaryData, String,
    CheckCloudAct](CheckCloudAct.apply, "data", "kind")

  implicit val publicDataFmt = jsonFormat[Option[InvoiceAndMemo], List[ClearToken], List[LNCloudAct],
    PublicData](PublicData.apply, "info", "tokens", "acts")

  implicit val privateDataFmt = jsonFormat[List[LNCloudAct], String,
    PrivateData](PrivateData.apply, "acts", "url")
}