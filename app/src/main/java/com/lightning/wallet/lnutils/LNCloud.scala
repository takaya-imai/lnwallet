package com.lightning.wallet.lnutils

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, Crypto, Transaction}
import com.lightning.wallet.ln.Tools.{none, random}
import rx.lang.scala.{Observable => Obs}
import scala.util.{Failure, Success}

import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.ln.RoutingInfoTag.PaymentRouteVec
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import java.math.BigInteger


abstract class Cloud extends StateMachine[CloudData] {
  // Persisted data exchange with a maintenance server
  // Checks if user's server accepts a signature
  def checkIfWorks: Obs[Any] = Obs just true
  protected[this] var isFree = true
  val connector: Connector

  def BECOME(d1: CloudData) = {
    // Save to db on every update
    CloudDataSaver saveObject d1
    become(d1, state)
  }
}

// Stores token request parameters, clear tokens left and tasks to be executed, also url for custom server
case class CloudData(info: Option[RequestAndMemo], tokens: Set[ClearToken], acts: Set[CloudAct], url: String)
// Represents a remote call to be executed in exchange for token or signature, can be locally persisted
case class CloudAct(data: BinaryData, plus: Seq[HttpParam], path: String)
class PublicCloud(bag: PaymentInfoBag) extends Cloud { me =>
  val connector = new Connector("http://10.0.2.2:9002")
  val expectedPrice = 2000000L // 2000 SAT
  val maxPrice = 20000000L // 20000 SAT

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case CloudData(None, clearTokens, _, _) \ CMDStart
      // We are free, have less than five tokens left, have channel which can handle a price
      if isFree && clearTokens.size < 5 && app.ChannelManager.canSend(maxPrice).nonEmpty =>
      isFree = false

      for {
        prAndMemo @ (pr, memo) <- retry(getRequestAndMemo, pickInc, 3 to 4)
        Some(rpi) <- retry(app.ChannelManager withRoutesAndOnionRPIFromPR pr, pickInc, 3 to 4)
        // Server response may arrive in some time after our requesting, must account for that
      } if (data.info.isEmpty && pr.unsafeMsat < maxPrice && memo.clears.size > 20) {
        // If data is empty, price is low enough and number of sigs is sufficient
        me BECOME data.copy(info = Some apply prAndMemo)
        app.ChannelManager.send(rpi, none)
        isFree = true
      }

    // Execute if we are not busy and have available tokens and actions, don't care amout memo
    case CloudData(_, SET(token @ (point, clear, sig), _*), SET(action, _*), _) \ CMDStart if isFree =>
      val params = Seq("point" -> point, "cleartoken" -> clear, "clearsig" -> sig, BODY -> action.data.toString) ++ action.plus
      val go = connector.ask[String](action.path, params:_*) doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go.foreach(_ => me BECOME data.copy(acts = data.acts - action, tokens = data.tokens - token), onError)
      isFree = false

      def onError(err: Throwable) = err.getMessage match {
        case "tokeninvalid" => me BECOME data.copy(tokens = data.tokens - token)
        case "tokenused" => me BECOME data.copy(tokens = data.tokens - token)
        case _ => Tools errlog err
      }

    // We don't have acts or tokens but have a memo
    // which means a payment is waiting so check if it's done
    case CloudData(Some(pr \ memo), _, _, _) \ CMDStart =>

      bag getPaymentInfo pr.paymentHash match {
        case Success(pay) if pay.actualStatus == SUCCESS => me resolveSuccess memo
        case Success(pay) if pay.actualStatus == FAILURE && !pr.isFresh => eraseRequestData
        // Repeatedly retry an existing payment request instead of getting a new one until it expires
        case Success(pay) if pay.actualStatus == FAILURE && app.ChannelManager.canSend(maxPrice).nonEmpty =>
          val go = retry(app.ChannelManager withRoutesAndOnionRPIFromPR pr, pickInc, 3 to 4)
          go.foreach(app.ChannelManager.sendOpt(_, none), none)

        // The very first attempt was rejected
        case Failure(_) => eraseRequestData
        // It's WAITING so do nothing
        case _ =>
      }

    case (_, act: CloudAct) =>
      // Record new action and try to send it
      me BECOME data.copy(acts = data.acts + act)
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
    connector.ask[TokensInfo]("blindtokens/info") flatMap {
      case (signerMasterPubKey, signerSessionPubKey, quantity) =>
        val pubKeyQ = ECKey.fromPublicOnly(HEX decode signerMasterPubKey)
        val pubKeyR = ECKey.fromPublicOnly(HEX decode signerSessionPubKey)

        // Prepare a list of BlindParam and a list of BigInteger clear tokens
        val blinder = new ECBlind(pubKeyQ.getPubKeyPoint, pubKeyR.getPubKeyPoint)
        val memo = BlindMemo(blinder params quantity, blinder tokens quantity, pubKeyR.getPublicKeyAsHex)
        connector.ask[String]("blindtokens/buy", "tokens" -> memo.makeBlindTokens.toJson.toString.hex,
          "seskey" -> memo.sesPubKeyHex).map(PaymentRequest.read).map(pr => pr -> memo)
      }

  def getClearTokens(m: BlindMemo) =
    connector.ask[BigIntegerVec]("blindtokens/redeem",
      "seskey" -> m.sesPubKeyHex).map(m.makeClearSigs).map(m.pack)
}

// Sig-based authentication
class PrivateCloud extends Cloud { me =>
  lazy val connector = new Connector(data.url)

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    // Execute if we are not busy and have available actions
    case CloudData(_, _, SET(action, _*), _) \ CMDStart if isFree =>
      val go = connector.ask[String](action.path, signed(action.data) ++ action.plus:_*)
      val go1 = go doOnTerminate { isFree = true } doOnCompleted { me doProcess CMDStart }
      go1.foreach(_ => me BECOME data.copy(acts = data.acts - action), Tools.errlog)
      isFree = false

    case (_, act: CloudAct) =>
      // Record new action and try to send it
      me BECOME data.copy(acts = data.acts + act)
      me doProcess CMDStart

    case _ =>
  }

  def signed(data: BinaryData) = {
    val sig = Crypto encodeSignature Crypto.sign(Crypto sha256 data, LNParams.cloudPrivateKey)
    Seq("sig" -> sig.toString, "pubkey" -> LNParams.cloudPublicKey.toString, BODY -> data.toString)
  }

  override def checkIfWorks = {
    val params = signed(random getBytes 32)
    connector.ask[String]("check", params:_*)
  }
}

class Connector(val url: String) {
  def http(way: String) = post(s"$url/$way", true)
  def ask[T : JsonFormat](command: String, params: HttpParam*): Obs[T] =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: response +: _) => response.convertTo[T]
      case _ => throw new ProtocolException
    }

  def findNodes(query: String) = ask[AnnounceChansNumVec]("router/nodes", "query" -> query)
  def getBackup(key: BinaryData) = ask[StringVec]("data/get", "key" -> key.toString).map(_ map HEX.decode)
  def getChildTxs(txs: TxSeq) = ask[TxSeq]("txs/get", "txids" -> txs.map(_.txid).toJson.toString.hex)

  def findRoutes(rd: RoutingData, froms: Set[PublicKey], to: PublicKey) =
    ask[PaymentRouteVec]("router/routes", "xn" -> rd.badNodes.map(_.toBin).toJson.toString.hex,
      "xc" -> rd.badChans.toJson.toString.hex, "froms" -> froms.map(_.toBin).toJson.toString.hex,
      "tos" -> Set(to).map(_.toBin).toJson.toString.hex)
}

object Connector {
  type TxSeq = Seq[Transaction]
  type StringVec = Vector[String]
  type BigIntegerVec = Vector[BigInteger]
  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  type RequestAndMemo = (PaymentRequest, BlindMemo)
  type ClearToken = (String, String, String)
  type TokensInfo = (String, String, Int)
  type HttpParam = (String, String)
  val CMDStart = "CMDStart"
  val BODY = "body"
}