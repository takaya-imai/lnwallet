package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.Connector._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import com.lightning.wallet.ln.Tools.{none, random}
import rx.lang.scala.{Observable => Obs}

import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey


// Shared by both private and public clouds, empty url means "use public"
case class CloudData(info: Option[RequestAndMemo], tokens: List[ClearToken],
                     acts: List[CloudAct], needsSave: Boolean, url: String)

trait Cloud { me: StateMachine[CloudData] =>
  def check: Obs[Any] = Obs just null
  val connector: Connector

  def UPDATE(d1: CloudData): Unit = {
    val d2 = d1.copy(needsSave = true)
    become(d2, state)
  }

  def SAVE: Unit = {
    val d1 = data.copy(needsSave = false)
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

// Users may supply their own cloud
class PrivateCloud(val connector: Connector)
extends StateMachine[CloudData] with Cloud { me =>

  def doProcess(some: Any) = (data, some) match {
    case CloudData(_, _, (action: CloudAct) :: rest, _, _) \ CMDStart =>
      // Private server should use a public key based authorization so we add a signature
      action.run(signedParams(action.requestPayload), me).doOnCompleted(me doProcess CMDStart)
        .foreach(ok => me UPDATE data.copy(acts = rest), Tools.errlog)

    case (_, action: CloudAct) =>
      // We must always record incoming actions
      val actions1 = action :: data.acts take 50
      me UPDATE data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools log s"PrivateCloud: unhandled $some : $data"
  }

  def signedParams(data: BinaryData): Seq[HttpParam] = {
    val signature = Crypto encodeSignature Crypto.sign(Crypto sha256 data, cloudPrivateKey)
    Seq("sig" -> signature.toString, "key" -> cloudPrivateKey.publicKey.toString, body -> data.toString)
  }

  override def check = {
    // Just send random stuff to see if key fits
    val test = signedParams(random getBytes 32)
    connector.call("check", none, test:_*)
  }
}

// Default cloud
class PublicCloud(val connector: Connector, bag: PaymentInfoBag)
extends StateMachine[CloudData] with Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, Nil, _, _, _) \ CMDStart => for {
      request \ blindMemo <- retry(getRequestAndMemo, pickInc, 2 to 3)
      Some(pay) <- retry(app.ChannelManager outPaymentObs request, pickInc, 2 to 3)
    } me doProcess Tuple2(pay, blindMemo)

    // This payment request may arrive in some time after an initialization above,
    // hence we state that it can only be accepted if info == None to avoid race condition
    case CloudData(None, tokens, _, _, _) \ Tuple2(pay: OutgoingPayment, memo: BlindMemo) =>
      for (chan <- app.ChannelManager.alive.headOption) chan process SilentAddHtlc(pay)
      me UPDATE data.copy(info = Some(pay.request, memo), tokens = tokens)

    // No matter what the state is: do auth based cloud call if we have spare tokens
    case CloudData(_, (blindPoint, clearToken, clearSig) :: ts, (action: CloudAct) :: rest, _, _) \ CMDStart =>
      val params = Seq("point" -> blindPoint, "cleartoken" -> clearToken, "clearsig" -> clearSig, body -> action.requestPayload.toString)
      action.run(params, me).doOnCompleted(me doProcess CMDStart).foreach(ok => me UPDATE data.copy(tokens = ts, acts = rest), Tools.errlog)

    // Start a new request while payment is in progress
    case CloudData(Some(request \ memo), _, _, _, _) \ CMDStart =>
      bag getPaymentInfo request.paymentHash getOrElse null match {
        case out: OutgoingPayment if out.actualStatus == SUCCESS => me resolveSuccess memo
        case out: OutgoingPayment if out.actualStatus == FAILURE => resetPaymentData
        case in: IncomingPayment => resetPaymentData
        case null => resetPaymentData
        case _ => // do nothing
      }

    case (_, action: CloudAct) =>
      // We must always record incoming actions
      val actions1 = action :: data.acts take 50
      me UPDATE data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools log s"LNCloudPublic: unhandled $some : $data"
  }

  // ADDING NEW TOKENS

  def resetPaymentData = me UPDATE data.copy(info = None)
  def sumIsAppropriate(req: PaymentRequest): Boolean = req.amount.exists(_.amount < 25000000L)
  def resolveSuccess(memo: BlindMemo) = getClearTokens(memo).doOnCompleted(me doProcess CMDStart)
    .foreach(plus => me UPDATE data.copy(info = None, tokens = plus ::: data.tokens),
      error => if (error.getMessage == "notfound") resetPaymentData)

  // TALKING TO SERVER

  def getRequestAndMemo: Obs[RequestAndMemo] =
    connector.call("blindtokens/info", identity) flatMap { raw =>
      val JsString(pubKeyQ) +: JsString(pubKeyR) +: JsNumber(qty) +: _ = raw
      val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubKeyR)
      val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubKeyQ)

      // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
      val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
      val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)
      connector.call("blindtokens/buy", vec => PaymentRequest read json2String(vec.head), "seskey" -> memo.sesPubKeyHex,
        "tokens" -> memo.makeBlindTokens.toJson.toString.hex).filter(sumIsAppropriate).map(_ -> memo)
    }

  def getClearTokens(memo: BlindMemo) =
    connector.call("blindtokens/redeem", _.map(json2String(_).bigInteger),
      "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

trait CloudAct {
  // These will be presisted in case of failure
  // and require token or key based authentication
  def run(params: Seq[HttpParam], cloud: Cloud): Obs[Unit]
  def requestPayload: BinaryData
}

// This is a basic interface for making cloud calls
// failover invariant will fall back to default in case of failure

class Connector(val url: String) {
  def http(way: String) = post(s"http://$url:9001/v1/$way", true)
  def call[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case err => throw new ProtocolException
    }

  def getRates = call("rates/get", identity)
  def getDatas(key: String) = call("data/get", toVec[BinaryData], "key" -> key)
  def getTxs(txids: String) = call("txs/get", toVec[Transaction], "txids" -> txids)
  def findNodes(ask: String) = call("router/nodes", toVec[AnnounceChansNum], "query" -> ask)
  def findRoutes(from: PublicKey, to: PublicKey) = if (from == to) Obs just Vector(Vector.empty)
    else call("router/routes", toVec[PaymentRoute], "from" -> from.toString, "to" -> to.toString)
}

class FailoverConnector(failover: Connector, url: String) extends Connector(url) {
  override def findNodes(ask: String) = super.findNodes(ask).onErrorResumeNext(_ => failover findNodes ask)
  override def getTxs(txids: String) = super.getTxs(txids).onErrorResumeNext(_ => failover getTxs txids)
  override def getDatas(key: String) = super.getDatas(key).onErrorResumeNext(_ => failover getDatas key)
  override def getRates = super.getRates.onErrorResumeNext(_ => failover.getRates)
}

object Connector {
  type HttpParam = (String, Object)
  type ClearToken = (String, String, String)
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  val CMDStart = "CMDStart"
  val body = "body"
}