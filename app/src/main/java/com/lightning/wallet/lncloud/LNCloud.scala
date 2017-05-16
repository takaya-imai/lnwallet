package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.softwaremill.quicklens._
import com.lightning.wallet.lncloud.LNCloud._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, Crypto}

import collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.wallet.ln.wire.UpdateFulfillHtlc
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import android.webkit.URLUtil
import scala.util.Success


case class LNCloudDataPrivate(acts: List[LNCloudAct], url: String)
abstract class LNCloudPrivate extends StateMachine[LNCloudDataPrivate] with Pathfinder { me =>
  def tryIfWorks(dummy: BinaryData) = lnCloud.call("sigcheck", identity, signed(dummy):_*)

  def signed(data: BinaryData): Seq[HttpParam] = {
    // Users may want to use a key pair instead of blind tokens so this is a helper for that
    val signature = Crypto encodeSignature Crypto.sign(Crypto sha256 data, LNParams.cloudPrivateKey)
    Seq("sig" -> signature.toString, "key" -> LNParams.cloudPrivateKey.publicKey.toString, body -> data.toString)
  }

  def doProcess(some: Any): Unit = (data, some) match {
    case (LNCloudDataPrivate(acts, _), act: LNCloudAct) =>
      me stayWith data.copy(acts = act :: acts take 1000)
      me doProcess CMDStart

    case (LNCloudDataPrivate(act :: ax, _), CMDStart) =>
      act.runPrivate(me).doOnCompleted(me doProcess CMDStart)
        .subscribe(_ => me stayWith data.copy(acts = ax), none)

    case(_, newUrl: String)
      if URLUtil isValidUrl newUrl =>
      me stayWith data.copy(url = newUrl)

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"LNCloudPrivate: unhandled $data : $some")
  }
}

case class LNCloudData(info: Option[InvoiceAndMemo], tokens: List[ClearToken] = Nil, acts: List[LNCloudAct] = Nil)
abstract class LNCloudPublic extends StateMachine[LNCloudData] with Pathfinder with StateMachineListener { me =>
  private def reset = me stayWith data.copy(info = None)
  val bag: PaymentSpecBag

  // LISTENING TO CHANNEL

  channel.listeners += me
  override def onPostProcess = {
    case fulfill: UpdateFulfillHtlc =>
      // This may be our HTLC, check it
      me doProcess CMDStart
  }

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case LNCloudData(None, Nil, _) ~ CMDStart => for {
      Tuple2(invoice, memo) <- retry(getInfo, pickInc, 2 to 3)
      Some(spec) <- retry(makeOutgoingSpec(invoice), pickInc, 2 to 3)
    } me doProcess Tuple2(spec, memo)

    // This payment request may arrive in some time after an initialization above,
    // hence we state that it can only be accepted if info == None to avoid race condition
    case LNCloudData(None, tokens, _) ~ Tuple2(spec: OutgoingPaymentSpec, memo: BlindMemo) =>
      me stayWith data.copy(info = Some(spec.invoice -> memo), tokens = tokens)
      channel doProcess SilentAddHtlc(spec)

    // No matter what the state is: do it if we have acts and tokens
    case LNCloudData(_, (blindPoint, clearToken, clearSignature) :: tx, act :: ax) ~ CMDStart =>
      act.runPublic(Seq("point" -> blindPoint, "cleartoken" -> clearToken, "clearsig" -> clearSignature), me)
        .doOnCompleted(me doProcess CMDStart).foreach(_ => me stayWith data.copy(tokens = tx, acts = ax), resolveError)

      // 'data' may change while request is on so we always copy it
      def resolveError(servError: Throwable) = servError.getMessage match {
        case "tokeninvalid" | "tokenused" => me stayWith data.copy(tokens = tx)
        case _ => me stayWith data
      }

    // Start request while payment is in progress
    // Payment may be finished already so we ask the bag
    case LNCloudData(Some(invoice ~ memo), _, _) ~ CMDStart =>
      bag.getInfoByHash(invoice.paymentHash).map(_.status) match {
        case Success(PaymentSpec.SUCCESS) => me resolveSuccess memo
        case Success(PaymentSpec.FAIL) => reset
        case Success(_) => me stayWith data
        case _ => reset
      }

    case (_, act: LNCloudAct) =>
      // We must always record incoming acts
      val acts1 = act :: data.acts take 1000
      me stayWith data.copy(acts = acts1)
      me doProcess CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"LNCloudPublic: unhandled $data : $some")
  }

  // ADDING NEW TOKENS

  def resolveSuccess(memo: BlindMemo) = {
    val refill: List[ClearToken] => Unit = plus => me stayWith data.copy(info = None, tokens = plus ::: data.tokens)
    getClearTokens(memo).doOnCompleted(me doProcess CMDStart).subscribe(refill, err => if (err.getMessage == "notfound") reset)
  }

  // TALKING TO SERVER

  def augmentInvoice(invoice: Invoice, howMuch: Int) = {
    import com.lightning.wallet.R.string.ln_credits_invoice
    val text = app getString ln_credits_invoice format howMuch
    invoice.modify(_.message) setTo Some(text)
  }

  def getInfo = lnCloud.call("blindtokens/info", identity) flatMap { raw =>
    val JsString(pubKeyQ) +: JsString(pubKeyR) +: JsNumber(qty) +: _ = raw
    val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubKeyR)
    val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubKeyQ)

    // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
    val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
    val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)
    lnCloud.call("blindtokens/buy", json => Invoice parse json2String(json.head), "seskey" -> memo.sesPubKeyHex,
      "tokens" -> memo.makeBlindTokens.toJson.toString.hex).map(augmentInvoice(_, qty.toInt) -> memo)
  }

  def getClearTokens(memo: BlindMemo) =
    lnCloud.call("blindtokens/redeem", _.map(json2String(_).bigInteger),
      "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

// Concrete cloud acts

trait LNCloudAct {
  def runPublic(base: Seq[HttpParam], cloud: LNCloudPublic): Obs[Unit]
  def runPrivate(cloud: LNCloudPrivate): Obs[Unit]
}

// Used for testing purposes
case class PingCloudAct(data: String, kind: String = "PingCloudAct") extends LNCloudAct {
  def runPublic(base: Seq[HttpParam], cloud: LNCloudPublic) = cloud.lnCloud.call("ping", println, body -> data)
  def runPrivate(cloud: LNCloudPrivate): Obs[Unit] = cloud.lnCloud.call("ping", println, body -> data)
}

// Pathfinder needs a channel reference
// to search for outgoing routes

trait Pathfinder {
  val lnCloud: LNCloud
  val channel: StateMachine[ChannelData]

  def makeOutgoingSpec(invoice: Invoice) =
    if (invoice.nodeId == LNParams.extendedNodeKey.publicKey) Obs just None
    else lnCloud.findRoutes(channel.data.announce.nodeId, invoice.nodeId) map {
      PaymentSpec.makeOutgoingSpec(_, invoice, firstExpiry = LNParams.myHtlcExpiry)
    }
}

// This is a basic interface to cloud which does not require a channel
// failover invariant will fall back to default in case of failure

class LNCloud(url: String) {
  def http(way: String) = post(s"$url/v1/$way", true) connectTimeout 6000
  def call[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case err => throw new ProtocolException
    }

  def findNodes(query: String) = call(command = "router/nodes",
    vec => for (json <- vec) yield json.convertTo[AnnounceChansNum],
    "query" -> query)

  def getRawData(key: String) = call("data/get", _.head, "key" -> key)
  def findRoutes(from: BinaryData, to: PublicKey) = call("router/routes",
    vec => for (json <- vec) yield json.convertTo[PaymentRoute],
    "from" -> from.toString, "to" -> to.toString)
}

class FailoverLNCloud(failover: LNCloud, url: String) extends LNCloud(url) {
  override def getRawData(key: String) = super.getRawData(key).onErrorResumeNext(_ => failover getRawData key)
  override def findNodes(query: String) = super.findNodes(query).onErrorResumeNext(_ => failover findNodes query)
  override def findRoutes(from: BinaryData, to: PublicKey) = super.findRoutes(from, to)
    .onErrorResumeNext(_ => failover.findRoutes(from, to) /* failover */)
}

object LNCloud {
  type HttpParam = (String, Object)
  type ClearToken = (String, String, String)
  type InvoiceAndMemo = (Invoice, BlindMemo)

  val body = "body"
  val fromBlacklisted = "fromblacklisted"
  val OPERATIONAL = "Operational"
  val CMDStart = "CMDStart"
}