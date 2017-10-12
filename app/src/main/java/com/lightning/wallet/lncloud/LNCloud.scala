package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.softwaremill.quicklens._
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
import scala.util.{Failure, Success}

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

  def checkIfWorks: Obs[Any] = Obs just true
  protected[this] var isFree: Boolean = true
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

case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
class PublicCloud(val connector: Connector, bag: PaymentInfoBag) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, ts, _, _) \ CMDStart if ts.isEmpty => for {
      // Get payment request, then fetch payment routes, then fulfill it
      requestAndMemo @ Tuple2(request, _) <- retry(getRequestAndMemo, pickInc, 3 to 4)
      Some(pay) <- retry(app.ChannelManager outPaymentObsFirst request, pickInc, 3 to 4)
      channel <- app.ChannelManager.alive.headOption

    } if (data.info.isEmpty) {
      // Payment request may arrive in some time after an initialization above,
      // so we state that it can only be accepted if `data.info` is still empty
      me UPDATE data.copy(info = Some apply requestAndMemo)
      channel process SilentAddHtlc(pay)
    }

    // Execute if we are not busy and have available tokens and actions
    case CloudData(_, SetEx(token @ Tuple3(pnt, clear, sig), _*), SetEx(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> pnt, "cleartoken" -> clear, "clearsig" -> sig, BODY -> action.data.toString) ++ action.plus
      val go = connector.tell(params, action.path) doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(ok => me UPDATE data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      // 'data' may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" => me UPDATE data.copy(tokens = data.tokens - token)
        case "tokenused" => me UPDATE data.copy(tokens = data.tokens - token)
        case _ => Tools errlog serverError
      }

    // Start a new request while payment is in progress
    case CloudData(Some(request \ memo), _, _, _) \ CMDStart =>

      bag getPaymentInfo request.paymentHash match {
        case Success(info) if info.actualStatus == SUCCESS => me resolveSuccess memo
        // Important: payment may fail but we wait until expiration before restarting
        case Success(info) if info.actualStatus == FAILURE && info.request.isFresh =>
        case Success(info) if info.actualStatus == FAILURE => resetPaymentData
        case Failure(_) => resetPaymentData
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
    connector.ask("blindtokens/info", identity) flatMap { raw =>
      val JsString(pubKeyQ) +: JsString(pubKeyR) +: JsNumber(qty) +: _ = raw
      val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubKeyR)
      val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubKeyQ)

      // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
      val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
      val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)
      connector.ask("blindtokens/buy", vec => PaymentRequest read json2String(vec.head), "seskey" -> memo.sesPubKeyHex,
        "tokens" -> memo.makeBlindTokens.toJson.toString.hex).filter(sumIsAppropriate).map(_ -> memo)
    }

  def getClearTokens(memo: BlindMemo) =
    connector.ask("blindtokens/redeem", _.map(json2String(_).bigInteger),
      "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

// Users may supply their own cloud with key based authentication
class PrivateCloud(val connector: Connector) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SetEx(action, _*), _) \ CMDStart if isFree =>
      val go = connector.tell(signed(action.data) ++ action.plus, action.path)
      go doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(ok => me UPDATE data.copy(acts = data.acts - action), Tools.errlog)
      isFree = false

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action take 50
      me UPDATE data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  def signed(data: BinaryData): Seq[HttpParam] = {
    val sig = Crypto encodeSignature Crypto.sign(Crypto sha256 data, cloudPrivateKey)
    Seq("sig" -> sig.toString, "key" -> cloudPublicKey.toString, BODY -> data.toString)
  }

  override def checkIfWorks = {
    val params = signed(random getBytes 32)
    connector.ask("check", none, params:_*)
  }
}

class Connector(val url: String) {
  // This is a basic interface for making cloud calls
  // failover invariant below will fall back to default in case of failure
  def http(way: String) = post(s"http://$url:9001/v1/$way", true) connectTimeout 15000

  def tell(params: Seq[HttpParam], path: String) = ask(path, none, params:_*)
  def ask[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case _ => throw new ProtocolException
    }

  def getRates = ask("rates/get", identity)
  def getDatas(key: String) = ask("data/get", toVec[BinaryData], "key" -> key)
  def getTxs(txids: String) = ask("txs/get", toVec[Transaction], "txids" -> txids)
  def findNodes(query: String) = ask("router/nodes", toVec[AnnounceChansNum], "query" -> query)
  def findRoutes(nodes: Set[PublicKey], channels: Set[Long], from: PublicKey, to: PublicKey) =
    ask("router/routes", toVec[PaymentRoute], "from" -> from.toString, "to" -> to.toString,
      "nodes" -> nodes.toJson.toString.hex, "channels" -> channels.toJson.toString.hex)
}

class FailoverConnector(failover: Connector, url: String) extends Connector(url) {
  override def getTxs(txids: String) = super.getTxs(txids).onErrorResumeNext(_ => failover getTxs txids)
  override def getDatas(key: String) = super.getDatas(key).onErrorResumeNext(_ => failover getDatas key)
  override def getRates = super.getRates.onErrorResumeNext(_ => failover.getRates)
}

object Connector {
  type HttpParam = (String, String)
  type ClearToken = (String, String, String)
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  val CMDStart = "CMDStart"
  val BODY = "body"
}