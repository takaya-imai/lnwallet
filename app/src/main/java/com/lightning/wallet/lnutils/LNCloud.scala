package com.lightning.wallet.lnutils

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import com.lightning.wallet.ln.Tools.{none, random}
import rx.lang.scala.{Observable => Obs}
import scala.util.{Failure, Success}

import com.lightning.wallet.ln.PaymentInfo.PublicPaymentRoute
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import com.lightning.wallet.ln.Broadcaster.TxSeq
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey


abstract class Cloud extends StateMachine[CloudData] {
  // Persisted data exchange with a maintenance server
  def checkIfWorks: Obs[Any] = Obs just true
  protected[this] var isFree = true
  val connector: Connector

  def UPDATE(d1: CloudData) = {
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
class PublicCloud(val connector: Connector, bag: PaymentInfoBag) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, ts, _, _) \ CMDStart if ts.isEmpty => for {
      // Get payment request, then fetch payment routes, then fulfill it
      paymentRequestAndMemo @ Tuple2(request, _) <- retry(getRequestAndMemo, pickInc, 3 to 4)
      Some(pay) <- retry(app.ChannelManager.outPaymentObs(Set.empty, Set.empty, request), pickInc, 3 to 4)
      channel <- app.ChannelManager.alive.headOption

    } if (data.info.isEmpty) {
      // Payment request may arrive in some time after an initialization above,
      // so we state that it can only be accepted if `data.info` is still empty
      me UPDATE data.copy(info = Some apply paymentRequestAndMemo)
      channel process SilentAddHtlc(pay)
    }

    // Execute if we are not busy and have available tokens and actions
    case CloudData(_, ##(token @ (point, clear, sig), _*), ##(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> sig, BODY -> action.data.toString) ++ action.plus
      val go = connector.tell(params, action.path) doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(ok => me UPDATE data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      // 'data' may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" => me UPDATE data.copy(tokens = data.tokens - token)
        case "tokenused" => me UPDATE data.copy(tokens = data.tokens - token)
        case _ => Tools errlog serverError
      }

    // Check if payment is fulfilled and get tokens if it is
    case CloudData(Some(request \ memo), _, _, _) \ CMDStart =>

      bag getPaymentInfo request.paymentHash match {
        case Success(info) if info.actualStatus == SUCCESS => me resolveSuccess memo
        // Important: payment may fail but we wait until expiration before restarting
        case Success(info) if info.actualStatus == FAILURE && info.request.isFresh =>
        case Success(info) if info.actualStatus == FAILURE => resetPaymentData
        // Payment has been rejected by channel so it's not found here
        case Failure(_) => resetPaymentData
        // WAITING state, do nothing
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
    case CloudData(_, _, ##(action, _*), _) \ CMDStart if isFree =>
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
  import com.lightning.wallet.ln.wire.LightningMessageCodecs._
  def http(way: String) = post(s"http://$url:9001/v1/$way", true) connectTimeout 15000

  def tell(params: Seq[HttpParam], path: String) = ask(path, none, params:_*)
  def ask[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case _ => throw new ProtocolException
    }

  def getRates = ask("rates/get", identity)
  def findNodes(query: String) = ask("router/nodes", toVec[AnnounceChansNum], "query" -> query)
  def getBackup(key: String) = ask("data/get", chans => toVec[String](chans) map HEX.decode, "key" -> key)
  def getChildTxs(txs: TxSeq) = ask("txs/get", toVec[Transaction], "txids" -> txs.map(_.txid).toJson.toString.hex)

  def findRoutes(noNodes: Set[PublicKey], noChannels: Set[Long], fromNode: PublicKey, toNode: PublicKey) =
    ask("router/routes", toVec[PublicPaymentRoute], "from" -> fromNode.toString, "to" -> toNode.toString,
      "nodes" -> noNodes.map(_.toBin).toJson.toString.hex, "channels" -> noChannels.toJson.toString.hex)
}

class FailoverConnector(failover: Connector, url: String) extends Connector(url) {
  override def getChildTxs(txs: TxSeq) = super.getChildTxs(txs).onErrorResumeNext(_ => failover getChildTxs txs)
  override def getBackup(key: String) = super.getBackup(key).onErrorResumeNext(_ => failover getBackup key)
  override def getRates = super.getRates.onErrorResumeNext(_ => failover.getRates)
}

object Connector {
  type HttpParam = (String, String)
  type ClearToken = (String, String, String)
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  val CMDStart = "CMDStart"
  val BODY = "body"
}