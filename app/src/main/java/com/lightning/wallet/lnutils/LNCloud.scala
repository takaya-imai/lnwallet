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

import com.lightning.wallet.ln.RoutingInfoTag.PaymentRoute
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

  def BECOME(d1: CloudData) = {
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

// Represents a task to be executed, can be persisted
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)
// Stores token request parameters, clear tokens left and tasks to be executed, also url for custom server
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
class PublicCloud(val connector: Connector, bag: PaymentInfoBag) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, tokens, acts, _) \ CMDStart
      // Try to get new tokens either when we have no tokens left
      // or when we have no acts to execute and a few tokens left
      if tokens.isEmpty || acts.isEmpty && tokens.size < 5 =>

      for {
        operationalChannel <- app.ChannelManager.all.find(_.isOperational)
        paymentRequestAndMemo @ (pr, _) <- retry(getRequestAndMemo, pickInc, 3 to 4)
        Some(pay) <- retry(app.ChannelManager outPaymentObs emptyRD(pr), pickInc, 3 to 4)
        // Server response may arrive in some time after an initialization above
      } if (data.info.isEmpty) {
        // Eliminate a race condition: only proceed if info is empty
        me BECOME data.copy(info = Some apply paymentRequestAndMemo)
        operationalChannel process SilentAddHtlc(pay)
      }

    // Execute if we are not busy and have available tokens and actions, don't care amout memo
    case CloudData(_, SET(token @ (point, clear, sig), _*), SET(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> sig, BODY -> action.data.toString) ++ action.plus
      val go = connector.ask(action.path, none, params:_*) doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(_ => me BECOME data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      // data may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" => me BECOME data.copy(tokens = data.tokens - token)
        case "tokenused" => me BECOME data.copy(tokens = data.tokens - token)
        case _ => Tools errlog serverError
      }

    // We don't have acts or tokens but have a memo
    case CloudData(Some(pr \ memo), _, _, _) \ CMDStart =>

      bag getPaymentInfo pr.paymentHash match {
        case Success(pay) if pay.actualStatus == SUCCESS => me resolveSuccess memo
        // These will have long expiration times, could be retried multiple times
        case Success(pay) if pay.actualStatus == FAILURE =>

          if (!pr.isFresh) eraseRequestData else for {
            operationalChannel <- app.ChannelManager.all.find(_.isOperational)
            // Repeatedly retry an old request instead of getting a new one until it expires
            Some(pay) <- retry(app.ChannelManager outPaymentObs emptyRD(pr), pickInc, 3 to 4)
          } operationalChannel process SilentAddHtlc(pay)

        // First attempt has been rejected
        case Failure(_) => eraseRequestData
        // Probably WAITING so do nothing
        case _ =>
      }

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action
      me BECOME data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  // ADDING NEW TOKENS

  def eraseRequestData = me BECOME data.copy(info = None)
  // Send CMDStart only in case if call was successful as we may enter an infinite loop otherwise
  // We only start over if server can't actually find our tokens, otherwise just wait for next try
  def resolveSuccess(memo: BlindMemo) = getClearTokens(memo).doOnCompleted(me doProcess CMDStart)
    .foreach(plus => me BECOME data.copy(info = None, tokens = data.tokens ++ plus),
      error => if (error.getMessage == "notfound") eraseRequestData)

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
        "tokens" -> memo.makeBlindTokens.toJson.toString.hex).filter(_.finalMsat < 20000000L).map(_ -> memo)
    }

  def getClearTokens(memo: BlindMemo) = connector.ask("blindtokens/redeem",
    ts => for (clearToken <- ts) yield json2String(clearToken).bigInteger,
    "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

// Users may supply their own cloud with key based authentication
class PrivateCloud(val connector: Connector) extends Cloud { me =>

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SET(action, _*), _) \ CMDStart if isFree =>
      val go = connector.ask(action.path, none, signed(action.data) ++ action.plus:_*)
      val go1 = go doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go1.foreach(_ => me BECOME data.copy(acts = data.acts - action), Tools.errlog)
      isFree = false

    case (_, action: CloudAct) =>
      val actions1 = data.acts + action
      me BECOME data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
  }

  def signed(data: BinaryData): Seq[HttpParam] = {
    val sig = Crypto encodeSignature Crypto.sign(Crypto sha256 data, cloudPrivateKey)
    Seq("sig" -> sig.toString, "pubkey" -> cloudPublicKey.toString, BODY -> data.toString)
  }

  override def checkIfWorks = {
    val params = signed(random getBytes 32)
    connector.ask("check", none, params:_*)
  }
}

class Connector(val url: String) {
  import com.lightning.wallet.ln.wire.LightningMessageCodecs._
  def http(way: String) = post(s"http://$url:9001/v1/$way", true)
  def ask[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case _ => throw new ProtocolException
    }

  def getRates = ask("rates/get", identity)
  def getBackup(key: String) = ask("data/get", ecryptedData => toVec[String](ecryptedData) map HEX.decode, "key" -> key)
  def getChildTxs(txs: TxSeq) = ask("txs/get", toVec[Transaction], "txids" -> txs.map(_.txid).toJson.toString.hex)

  def findNodes(query: String) = ask("router/nodes", toVec[AnnounceChansNum], "query" -> query)
  def findRoutes(noNodes: Set[PublicKey], noChannels: Set[Long], from: PublicKey, to: PublicKey) =
    ask("router/routes", toVec[PaymentRoute], "nodes" -> noNodes.map(_.toBin).toJson.toString.hex,
      "channels" -> noChannels.toJson.toString.hex, "from" -> from.toString, "to" -> to.toString)
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