package com.lightning.wallet.lncloud

import spray.json._
import spray.json.DefaultJsonProtocol._
import fr.acinq.bitcoin.Crypto.{Point, PrivateKey, PublicKey, Scalar}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, OutPoint, Satoshi, Transaction, TxOut}
import com.lightning.wallet.ln.crypto.{Packet, SecretsAndPacket, ShaHashesWithIndex}
import com.lightning.wallet.lncloud.LNCloud.{ClearToken, InvoiceAndMemo}
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import com.lightning.wallet.ln.wire.LightningMessageCodecs.PaymentRoute
import com.lightning.wallet.ln.crypto.Sphinx.BytesAndKey
import com.lightning.wallet.lncloud.RatesSaver.RatesMap
import org.bitcoinj.core.ECKey.CURVE.getCurve
import com.lightning.wallet.ln.Tools.{Bytes, LightningMessages}
import org.spongycastle.math.ec.ECPoint
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.Coin
import scodec.bits.BitVector
import java.math.BigInteger

import com.lightning.wallet.ln.Scripts._
import com.lightning.wallet.ln.crypto.ShaChain.Index


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
      case spec: OutgoingPaymentSpec => spec.toJson
      case spec: IncomingPaymentSpec => spec.toJson
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

  implicit object UpdateAddHtlcFmt extends JsonFormat[UpdateAddHtlc] {
    def read(rawJson: JsValue) = updateAddHtlcCodec.decode(json2BitVec(rawJson).get).require.value
    def write(add: UpdateAddHtlc) = updateAddHtlcCodec.encode(add).require.toHex.toJson
  }

  implicit object CommitSigFmt extends JsonFormat[CommitSig] {
    def read(rawJson: JsValue) = commitSigCodec.decode(json2BitVec(rawJson).get).require.value
    def write(sig: CommitSig) = commitSigCodec.encode(sig).require.toHex.toJson
  }

  implicit object ClosingSignedFmt extends JsonFormat[ClosingSigned] {
    def read(rawJson: JsValue) = closingSignedCodec.decode(json2BitVec(rawJson).get).require.value
    def write(close: ClosingSigned) = closingSignedCodec.encode(close).require.toHex.toJson
  }

  implicit object ShutdownFmt extends JsonFormat[Shutdown] {
    def read(rawJson: JsValue) = shutdownCodec.decode(json2BitVec(rawJson).get).require.value
    def write(down: Shutdown) = shutdownCodec.encode(down).require.toHex.toJson
  }

  implicit object FundingLockedFmt extends JsonFormat[FundingLocked] {
    def read(rawJson: JsValue) = fundingLockedCodec.decode(json2BitVec(rawJson).get).require.value
    def write(lock: FundingLocked) = fundingLockedCodec.encode(lock).require.toHex.toJson
  }

  implicit val blindParamFmt = jsonFormat[Bytes, BigInteger, BigInteger, BigInteger, BigInteger,
    BlindParam](BlindParam.apply, "point", "a", "b", "c", "bInv")

  implicit val blindMemoFmt = jsonFormat[List[BlindParam], List[BigInteger], String,
    BlindMemo](BlindMemo.apply, "params", "clears", "sesPubKeyHex")

  implicit val pointFmt = jsonFormat[ECPoint, Point](Point.apply, "value")
  implicit val scalarFmt = jsonFormat[BigInteger, Scalar](Scalar.apply, "value")
  implicit val privateKeyFmt = jsonFormat[Scalar, Boolean, PrivateKey](PrivateKey.apply, "value", "compressed")
  implicit val publicKeyFmt = jsonFormat[Point, Boolean, PublicKey](PublicKey.apply, "value", "compressed")
  implicit val milliSatoshiFmt = jsonFormat[Long, MilliSatoshi](MilliSatoshi.apply, "amount")
  implicit val satoshiFmt = jsonFormat[Long, Satoshi](Satoshi.apply, "amount")

  implicit val invoiceFmt = jsonFormat[Option[String], PublicKey, MilliSatoshi, BinaryData,
    Invoice](Invoice.apply, "message", "nodeId", "sum", "paymentHash")

  implicit val packetFmt = jsonFormat[Bytes, Bytes, Bytes, Bytes,
    Packet](Packet.apply, "v", "publicKey", "routingInfo", "hmac")

  implicit val secretsAndPacketFmt = jsonFormat[Vector[BytesAndKey], Packet,
    SecretsAndPacket](SecretsAndPacket.apply, "sharedSecrets", "packet")

  implicit val outgoingPaymentSpecFmt = jsonFormat[Invoice,
    Option[BinaryData], Vector[PaymentRoute], SecretsAndPacket, Long, Long, String,
    OutgoingPaymentSpec](OutgoingPaymentSpec.apply, "invoice", "preimage", "routes",
    "onion", "amountWithFee", "expiry", "kind")

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

  // Channel data

  implicit val outPointFmt = jsonFormat[BinaryData, Long,
    OutPoint](OutPoint.apply, "hash", "index")

  implicit val txOutFmt = jsonFormat[Satoshi, BinaryData,
    TxOut](TxOut.apply, "amount", "publicKeyScript")

  implicit val inputInfoFmt = jsonFormat[OutPoint, TxOut, BinaryData,
    InputInfo](InputInfo.apply, "outPoint", "txOut", "redeemScript")

  implicit object TransactionWithInputInfoFmt
  extends JsonFormat[TransactionWithInputInfo] {

    def read(json: JsValue) =
      json.asJsObject fields "kind" match {
        case JsString("CommitTx") => json.convertTo[CommitTx]
        case JsString("HtlcSuccessTx") => json.convertTo[HtlcSuccessTx]
        case JsString("HtlcTimeoutTx") => json.convertTo[HtlcTimeoutTx]
        case JsString("ClaimHtlcSuccessTx") => json.convertTo[ClaimHtlcSuccessTx]
        case JsString("ClaimHtlcTimeoutTx") => json.convertTo[ClaimHtlcTimeoutTx]
        case JsString("ClaimP2WPKHOutputTx") => json.convertTo[ClaimP2WPKHOutputTx]
        case JsString("ClaimDelayedOutputTx") => json.convertTo[ClaimDelayedOutputTx]
        case JsString("MainPenaltyTx") => json.convertTo[MainPenaltyTx]
        case JsString("HtlcPenaltyTx") => json.convertTo[HtlcPenaltyTx]
        case JsString("ClosingTx") => json.convertTo[ClosingTx]
        case _ => throw new RuntimeException
      }

    def write(internal: TransactionWithInputInfo) =
      internal match {
        case data: CommitTx => data.toJson
        case data: HtlcSuccessTx => data.toJson
        case data: HtlcTimeoutTx => data.toJson
        case data: ClaimHtlcSuccessTx => data.toJson
        case data: ClaimHtlcTimeoutTx => data.toJson
        case data: ClaimP2WPKHOutputTx => data.toJson
        case data: ClaimDelayedOutputTx => data.toJson
        case data: MainPenaltyTx => data.toJson
        case data: HtlcPenaltyTx => data.toJson
        case data: ClosingTx => data.toJson
      }
  }

  implicit val commitTxFmt = jsonFormat[InputInfo, Transaction, String,
    CommitTx](CommitTx.apply, "input", "tx", "kind")

  implicit val htlcSuccessTxFmt = jsonFormat[InputInfo, Transaction, BinaryData, String,
    HtlcSuccessTx](HtlcSuccessTx.apply, "input", "tx", "paymentHash", "kind")

  implicit val htlcTimeoutTxFmt = jsonFormat[InputInfo, Transaction, String,
    HtlcTimeoutTx](HtlcTimeoutTx.apply, "input", "tx", "kind")

  implicit val claimHtlcSuccessTxFmt = jsonFormat[InputInfo, Transaction, String,
    ClaimHtlcSuccessTx](ClaimHtlcSuccessTx.apply, "input", "tx", "kind")

  implicit val claimHtlcTimeoutTxFmt = jsonFormat[InputInfo, Transaction, String,
    ClaimHtlcTimeoutTx](ClaimHtlcTimeoutTx.apply, "input", "tx", "kind")

  implicit val claimP2WPKHOutputTxFmt = jsonFormat[InputInfo, Transaction, String,
    ClaimP2WPKHOutputTx](ClaimP2WPKHOutputTx.apply, "input", "tx", "kind")

  implicit val claimDelayedOutputTxFmt = jsonFormat[InputInfo, Transaction, String,
    ClaimDelayedOutputTx](ClaimDelayedOutputTx.apply, "input", "tx", "kind")

  implicit val mainPenaltyTxFmt = jsonFormat[InputInfo, Transaction, String,
    MainPenaltyTx](MainPenaltyTx.apply, "input", "tx", "kind")

  implicit val htlcPenaltyTxFmt = jsonFormat[InputInfo, Transaction, String,
    HtlcPenaltyTx](HtlcPenaltyTx.apply, "input", "tx", "kind")

  implicit val closingTxFmt = jsonFormat[InputInfo, Transaction, String,
    ClosingTx](ClosingTx.apply, "input", "tx", "kind")

  implicit val localParamsFmt = jsonFormat[BinaryData, Long, Long, Long,
    Long, Int, Int, PrivateKey, Scalar, PrivateKey, Scalar, BinaryData, BinaryData, Boolean,
    LocalParams](LocalParams.apply, "chainHash", "dustLimitSatoshis", "maxHtlcValueInFlightMsat", "channelReserveSat",
    "htlcMinimumMsat", "toSelfDelay", "maxAcceptedHtlcs", "fundingPrivKey", "revocationSecret", "paymentKey", "delayedPaymentKey",
    "defaultFinalScriptPubKey", "shaSeed", "isFunder")

  implicit val remoteParamsFmt = jsonFormat[Long, Long, Long, Long, Int, Int, PublicKey, Point, Point, Point, BinaryData, BinaryData,
    RemoteParams](RemoteParams.apply, "dustLimitSatoshis", "maxHtlcValueInFlightMsat", "channelReserveSatoshis", "htlcMinimumMsat",
    "toSelfDelay", "maxAcceptedHtlcs", "fundingPubKey", "revocationBasepoint", "paymentBasepoint", "delayedPaymentBasepoint",
    "globalFeatures", "localFeatures")

  implicit val htlcFmt = jsonFormat[Boolean, UpdateAddHtlc,
    Htlc](Htlc.apply, "incoming", "add")

  implicit val commitmentSpecFmt = jsonFormat[Long, Long, Long, Set[Htlc],
    CommitmentSpec](CommitmentSpec.apply, "feeratePerKw", "toLocalMsat", "toRemoteMsat", "htlcs")

  implicit val htlcTxAndSigs = jsonFormat[TransactionWithInputInfo, BinaryData, BinaryData,
    HtlcTxAndSigs](HtlcTxAndSigs.apply, "txinfo", "localSig", "remoteSig")

  implicit val localCommitFmt = jsonFormat[Long, CommitmentSpec, Seq[HtlcTxAndSigs], CommitTx,
    LocalCommit](LocalCommit.apply, "index", "spec", "htlcTxsAndSigs", "commitTx")

  implicit val remoteCommitFmt = jsonFormat[Long, CommitmentSpec, BinaryData, Point,
    RemoteCommit](RemoteCommit.apply, "index", "spec", "txid", "remotePerCommitmentPoint")

  implicit val waitingForRevocationFmt = jsonFormat[RemoteCommit, CommitSig,
    WaitingForRevocation](WaitingForRevocation.apply, "nextRemoteCommit", "sent")

  implicit val changesFmt = jsonFormat[LightningMessages, LightningMessages, LightningMessages,
    Changes](Changes.apply, "proposed", "signed", "acked")

  implicit val shaHashesWithIndexFmt = jsonFormat[Map[Index, Bytes], Option[Long],
    ShaHashesWithIndex](ShaHashesWithIndex.apply, "hashes", "lastIndex")

  implicit val commitmentsFmt = jsonFormat[LocalParams, RemoteParams, LocalCommit, RemoteCommit, Changes, Changes,
    Long, Long, Either[WaitingForRevocation, Point], LightningMessages, InputInfo, ShaHashesWithIndex, BinaryData,
    Commitments](Commitments.apply, "localParams", "remoteParams", "localCommit", "remoteCommit", "localChanges",
    "remoteChanges", "localNextHtlcId", "remoteNextHtlcId", "remoteNextCommitInfo", "unackedMessages",
    "commitInput", "remotePerCommitmentSecrets", "channelId")

  implicit val localCommitPublishedFmt = jsonFormat[Seq[Transaction],
    Seq[Transaction], Seq[Transaction], Seq[Transaction], Seq[Transaction], Transaction,
    LocalCommitPublished](LocalCommitPublished.apply, "claimMainDelayedOutputTx", "htlcSuccessTxs",
    "htlcTimeoutTxs", "claimHtlcSuccessTxs", "claimHtlcTimeoutTxs", "commitTx")

  implicit val remoteCommitPublishedFmt = jsonFormat[Seq[Transaction], Seq[Transaction], Seq[Transaction], Transaction,
    RemoteCommitPublished](RemoteCommitPublished.apply, "claimMainOutputTx", "claimHtlcSuccessTxs", "claimHtlcTimeoutTxs",
    "commitTx")

  implicit val revokedCommitPublishedFmt = jsonFormat[Seq[Transaction],
    Seq[Transaction], Seq[Transaction], Seq[Transaction], Seq[Transaction], Transaction,
    RevokedCommitPublished](RevokedCommitPublished.apply, "claimMainOutputTx", "mainPenaltyTx",
    "claimHtlcTimeoutTxs", "htlcTimeoutTxs", "htlcPenaltyTxs", "commitTx")

  implicit object ChannelDataFmt
    extends JsonFormat[ChannelData] {

    def read(json: JsValue) =
      json.asJsObject fields "kind" match {
        case JsString("NormalData") => json.convertTo[NormalData]
        case JsString("ClosingData") => json.convertTo[ClosingData]
        case JsString("NegotiationsData") => json.convertTo[NegotiationsData]
        case JsString("WaitFundingConfirmedData") => json.convertTo[WaitFundingConfirmedData]
        case _ => throw new RuntimeException
      }

    def write(internal: ChannelData) = internal match {
      case data: WaitFundingConfirmedData => data.toJson
      case data: NegotiationsData => data.toJson
      case data: ClosingData => data.toJson
      case data: NormalData => data.toJson
      case _ => throw new RuntimeException
    }
  }

  implicit val closingDataFmt = jsonFormat[NodeAnnouncement, Commitments, Seq[Transaction],
    Seq[LocalCommitPublished], Seq[RemoteCommitPublished], Seq[RemoteCommitPublished], Seq[RevokedCommitPublished], String,
    ClosingData](ClosingData.apply, "announce", "commitments", "mutualClose", "localCommit", "remoteCommit", "nextRemoteCommit",
    "revokedCommits", "kind")

  implicit val negotiationsDataFmt = jsonFormat[NodeAnnouncement, Commitments, ClosingSigned, Shutdown, Shutdown, String,
    NegotiationsData](NegotiationsData.apply, "announce", "commitments", "localClosingSigned", "localShutdown",
    "remoteShutdown", "kind")

  implicit val normalDataFmt = jsonFormat[NodeAnnouncement, Commitments, Option[Shutdown], Option[Shutdown], String,
    NormalData](NormalData.apply, "announce", "commitments", "localShutdown", "remoteShutdown", "kind")

  implicit val waitFundingConfirmedDataFmt = jsonFormat[NodeAnnouncement,
    Option[FundingLocked], Option[FundingLocked], Transaction, Commitments, String,
    WaitFundingConfirmedData](WaitFundingConfirmedData.apply, "announce", "our",
    "their", "fundingTx", "commitments", "kind")
}