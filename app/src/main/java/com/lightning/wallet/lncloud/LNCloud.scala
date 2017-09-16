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


// Persisted data exchange with a maintenance server
abstract class Cloud extends StateMachine[CloudData] {

  def checkIfWorks: Obs[Any] = Obs just null
  protected var isFree: Boolean = true
  var needsToBeSaved: Boolean = false
  val connector: Connector

  def UPDATE(d1: CloudData) = {
    // Each time data is changed
    needsToBeSaved = true
    become(d1, state)
  }

  def SAVE = {
    CloudDataSaver saveObject data
    needsToBeSaved = false
  }
}

case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
class PublicCloud(val connector: Connector, bag: PaymentInfoBag) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, ts, _, _) \ CMDStart if ts.isEmpty => for {
      paymentRequest \ blindMemo <- retry(getRequestAndMemo, pickInc, 2 to 3)
      Some(pay) <- retry(app.ChannelManager outPaymentObs paymentRequest, pickInc, 2 to 3)
    } me doProcess Tuple2(pay, blindMemo)

    // This payment request may arrive in some time after an initialization above,
    // hence we state that it can only be accepted if info == None to avoid race condition
    case CloudData(None, tokens, _, _) \ Tuple2(pay: OutgoingPayment, memo: BlindMemo) =>
      for (chan <- app.ChannelManager.alive.headOption) chan process SilentAddHtlc(pay)
      me UPDATE data.copy(info = Some(pay.request, memo), tokens = tokens)

    // Execute if we are not busy and have available tokens and actions
    case CloudData(_, SetEx(token @ (pt, clearToken, clearSig), _*), SetEx(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> pt, "cleartoken" -> clearToken, "clearsig" -> clearSig, body -> action.requestPayload.toString)
      val callAndRestart = action.run(params, me) doOnTerminate { isFree = true } doAfterTerminate { me doProcess CMDStart }
      callAndRestart.foreach(ok => me UPDATE data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      // 'data' may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" => me UPDATE data.copy(tokens = data.tokens - token)
        case "tokenused" => me UPDATE data.copy(tokens = data.tokens - token)
        case _ => Tools errlog serverError
      }

    // Start a new request while payment is in progress
    case CloudData(Some(request \ memo), _, _, _) \ CMDStart =>
      bag getPaymentInfo request.paymentHash getOrElse null match {
        case info if info.actualStatus == SUCCESS => me resolveSuccess memo
        // This is crucial: payment may fail but we should wait until expiration
        case info if info.actualStatus == FAILURE && info.request.isFresh =>
        case info if info.actualStatus == FAILURE => resetPaymentData
        case info if info.actualStatus == REFUND => resetPaymentData
        case null => resetPaymentData
        case _ =>
      }

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action take 50
      me UPDATE data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  // ADDING NEW TOKENS

  def resetPaymentData = me UPDATE data.copy(info = None)
  def sumIsAppropriate(req: PaymentRequest): Boolean = req.amount.exists(_.amount < 25000000L)
  // Send CMDStart only in case if call was successful as we may enter an infinite loop otherwise
  def resolveSuccess(memo: BlindMemo) = getClearTokens(memo).doOnCompleted(me doProcess CMDStart)
    .foreach(plus => me UPDATE data.copy(info = None, tokens = data.tokens ++ plus),
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

// Users may supply their own cloud with key based authentication
class PrivateCloud(val connector: Connector) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SetEx(action, _*), _) \ CMDStart if isFree =>
      val callAndRestart = action.run(signedParams(action.requestPayload), me)
      callAndRestart doOnTerminate { isFree = true } doAfterTerminate { me doProcess CMDStart }
      callAndRestart.foreach(ok => me UPDATE data.copy(acts = data.acts - action), Tools.errlog)
      isFree = false

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action take 50
      me UPDATE data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  def signedParams(data: BinaryData): Seq[HttpParam] = {
    val signature = Crypto encodeSignature Crypto.sign(Crypto sha256 data, cloudPrivateKey)
    Seq("sig" -> signature.toString, "key" -> cloudPrivateKey.publicKey.toString, body -> data.toString)
  }

  override def checkIfWorks = {
    val test = signedParams(random getBytes 32)
    connector.call("check", none, test:_*)
  }
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