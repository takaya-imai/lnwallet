package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.lncloud.BlindTokens._
import com.lightning.wallet.lncloud.BlindTokensSaver._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.Utils.{app, string2Ops}
import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.{NodeAnnouncements}

import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.wire.LightningMessageCodecs
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.BinaryData
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException

import org.bitcoinj.core.ECKey
import scodec.bits.BitVector


sealed trait BlindProgress
case class UnpaidTokens(memo: BlindMemo, invoice: Invoice) extends BlindProgress
case class PaidTokens(memo: BlindMemo, paymentPreimage: BinaryData) extends BlindProgress

//abstract class BlindTokens(var tokens: Set[ClearToken], baseState: List[String], baseData: BlindProgress)
//extends StateMachine[BlindProgress](baseState, baseData) with ChannelListener with LNCloud { me =>
//
//  def reset = become(null, OPERATIONAL)
//  def extractState = (tokens, state, data)
//
//  def doProcess(change: Any) = (data, change, state.head) match {
//    case (UnpaidTokens(_, invoice), spec: CommitmentSpec, AWAIT_PAYMENT)
//      if !spec.htlcs.exists(invoice.paymentHash == _.add.paymentHash) =>
//      reset
//
//    // Counterparty has fulfilled an HTLC, we now have a payment preimage
//    case (UnpaidTokens(memo, invoice), preimage: BinaryData, AWAIT_PAYMENT)
//      if invoice.paymentHash == fr.acinq.bitcoin.Crypto.sha256(preimage) =>
//      becomeReact(PaidTokens(memo, preimage), AWAIT_TOKENS, Start)
//
//    // Ask server for tokens
//    case (paid: PaidTokens, Start, AWAIT_TOKENS) =>
//      // In case of general server error we just wait until the next app launch
//      val result = getClearTokens(paid).map(plus => tokens ++= plus).map(_ => reset)
//      result.foreach(_ => onTokensAdded, err => if (err.getMessage == "notfound") reset)
//
//    // No matter the state: do it if we have tokens
//    case (_, act: LNCloudAction, _) if tokens.nonEmpty =>
//      val head: ClearToken = tokens.head
//      tokens -= head
//
//      act.run(head).foreach(none, _.getMessage match {
//        // Retry this item if token is invalid, revert otherwise
//        case "tokeninvalid" | "tokenused" => me process act
//        case _ => tokens += head
//      }, println)
//
//    // We have no more clear tokens
//    // and we are not getting new ones
//    case (null, Start, AWAIT_INFO) => reset
//    case (null, _: LNCloudAction, OPERATIONAL) if tokens.isEmpty =>
//      retry(getInfo, pickInc, 2 to 6 by 2) foreach { case (inv, memo) =>
//        become(UnpaidTokens(memo, inv), AWAIT_PAYMENT)
//        me processInvoice inv
//      }
//
//      // Block next cloud actions
//      become(null, AWAIT_INFO)
//
//    case (data1, something, state1) =>
//      // Let know if received an unhandled message in some state
//      println(s"Unhandled $something in BlindTokensListener at $state1 : $data1")
//  }
//
//  def onTokensAdded
//  def processInvoice(invoice: Invoice)
//  override def onFulfilled(preimage: BinaryData) = me process preimage
//  override def onRevokedAndAcked(commitSpec: CommitmentSpec) = me process commitSpec
//
//  // TALKING TO LNCLOUD SERVER
//
//  def augmentInvoice(invoice: Invoice, quantity: Int) = {
//    import com.lightning.wallet.R.string.ln_credits_invoice
//    val text = app getString ln_credits_invoice format quantity
//    invoice.copy(message = Some apply text)
//  }
//
//  def getInfo = call("blindtokens/info", identity) flatMap { raw =>
//    val JsString(pubQ) +: JsString(pubR) +: JsNumber(qty) +: _ = raw
//    val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubR)
//    val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubQ)
//
//    // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
//    val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
//    val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)
//
//    call("blindtokens/buy", result => Invoice parse result.head.convertTo[String],
//      "seskey" -> memo.sesPubKeyHex, "tokens" -> memo.makeBlindTokens.toJson.convertTo[String].hex)
//      .filter(_.sum.amount < 50000000).map(invoice => augmentInvoice(invoice, qty.toInt) -> memo)
//  }
//
//  def getClearTokens(paid: PaidTokens) = call("blindtokens/redeem",
//    blindSigs => for (blind <- blindSigs) yield blind.convertTo[String].bigInteger,
//    "preimage" -> paid.paymentPreimage.toString, "seskey" -> paid.memo.sesPubKeyHex)
//      .map(paid.memo.makeClearSigs).map(paid.memo.pack)
//}

object BlindTokens {
  val AWAIT_PAYMENT = "AwaitPayment"
  val AWAIT_TOKENS = "AwaitTokens"
  val OPERATIONAL = "Operational"
  val AWAIT_INFO = "AwaitInfo"
  val Start = "Start"
}

trait LNCloud { me =>
  def call[T](command: String, trans: Vector[JsValue] => T, params: (String, Object)*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => trans(responses)
      case _ => throw new ProtocolException
    }

  def http(command: String) = HttpRequest.post(s"http://10.0.2.2:9002/v1/$command", true) connectTimeout 10000
  def bitVec2Nodes(vec: BitVector): NodeAnnouncements = LightningMessageCodecs.announcementsCodec.decode(vec).require.value
  def json2BitVec(raw: JsValue): Option[BitVector] = BitVector fromHex raw.convertTo[String]

  type FeeRates = Map[Int, BigDecimal]
  def getFees = to[FeeRates](http("fees").body) mapValues MSat.btcBigDecimal2MilliSatoshi
  def findRoutes(from: BinaryData, to: BinaryData) = call("router/routes",
    _.flatMap(json2BitVec).map(LightningMessageCodecs.hopsCodec.decode(_).require.value),
    "from" -> from.toString, "to" -> to.toString)

  def findNodes(query: String): Obs[NodeAnnouncements] = call("router/nodes/find",
    result => json2BitVec(result.head).map(bitVec2Nodes).get, "query" -> query)

  def listNodes: Obs[NodeAnnouncements] = call("router/nodes/list",
    result => json2BitVec(result.head).map(bitVec2Nodes).get)
}

trait LNCloudAction {
  def run(ct: ClearToken): Obs[Unit]
}