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
import org.bitcoinj.core.{ECKey, PeerAddress, PeerGroup}

import collection.JavaConverters.mapAsJavaMapConverter
import com.lightning.wallet.ln.wire.UpdateFulfillHtlc
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import com.google.common.net.InetAddresses
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import scala.util.Success


case class PrivateData(acts: List[LNCloudAct], url: String)
case class PublicData(info: Option[InvoiceAndMemo], tokens: List[ClearToken], acts: List[LNCloudAct] = Nil)
// By default a public pathfinder is used but user may provide their own private pathfinder anytime

class PrivatePathfinder(val lnCloud: LNCloud, val channel: Channel)
extends StateMachine[PrivateData] with Pathfinder { me =>

  def doProcess(some: Any) = (data, some) match {
    case (PrivateData(actions, _), act: LNCloudAct) =>
      me stayWith data.copy(acts = act :: actions take 1000)
      me doProcess CMDStart

    case (PrivateData(action :: rest, _), CMDStart) =>
      val signature = Crypto encodeSignature Crypto.sign(Crypto sha256 action.data, LNParams.cloudPrivateKey)
      val params = Seq("sig" -> signature.toString, "key" -> LNParams.cloudPrivateKey.publicKey.toString, body -> action.data.toString)
      action.run(params, lnCloud).doOnCompleted(me doProcess CMDStart).subscribe(_ => me stayWith data.copy(acts = rest), none)

    case _ =>
      // Let know if received an unhandled message in some state
      Tools log s"PrivatePathfinder: unhandled $data : $some"
  }
}

class PublicPathfinder(val bag: PaymentSpecBag, val lnCloud: LNCloud, val channel: Channel)
extends StateMachine[PublicData] with Pathfinder with StateMachineListener { me =>
  private def reset = me stayWith data.copy(info = None)

  // LISTENING TO CHANNEL

  channel.listeners += me
  override def onPostProcess = {
    case fulfill: UpdateFulfillHtlc =>
      // This may be our HTLC, check it
      me doProcess CMDStart
  }

  // STATE MACHINE

  def doProcess(some: Any) = (data, some) match {
    case PublicData(None, Nil, _) ~ CMDStart => for {
      invoice ~ memo <- retry(getInfo, pickInc, times = 2 to 3)
      Some(spec) <- retry(makeOutgoingSpec(invoice), pickInc, 2 to 3)
    } me doProcess Tuple2(spec, memo)

    // This payment request may arrive in some time after an initialization above,
    // hence we state that it can only be accepted if info == None to avoid race condition
    case PublicData(None, tokens, _) ~ Tuple2(spec: OutgoingPaymentSpec, memo: BlindMemo) =>
      me stayWith data.copy(info = Some(spec.invoice, memo), tokens = tokens)
      channel doProcess SilentAddHtlc(spec)

    // No matter what the state is: do it if we have acts and tokens
    case PublicData(_, (blindPoint, clearToken, clearSignature) :: ts, action :: rest) ~ CMDStart =>
      val params = Seq("point" -> blindPoint, "cleartoken" -> clearToken, "clearsig" -> clearSignature, body -> action.data.toString)
      action.run(params, lnCloud).doOnCompleted(me doProcess CMDStart).foreach(_ => me stayWith data.copy(tokens = ts, acts = rest), onError)

      // 'data' may change while request is on so we always copy it
      def onError(serverError: Throwable) = serverError.getMessage match {
        case "tokeninvalid" | "tokenused" => me stayWith data.copy(tokens = ts)
        case _ => me stayWith data
      }

    // Start request while payment is in progress
    // Payment may be finished already so we ask the bag
    case PublicData(Some(invoice ~ memo), _, _) ~ CMDStart =>
      bag.getInfoByHash(invoice.paymentHash).map(_.status) match {
        case Success(PaymentSpec.SUCCESS) => me resolveSuccess memo
        case Success(PaymentSpec.FAIL) => reset
        case Success(_) => me stayWith data
        case _ => reset
      }

    case (_, action: LNCloudAct) =>
      // We must always record incoming actions
      val actions1 = action :: data.acts take 1000
      me stayWith data.copy(acts = actions1)
      me doProcess CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools log s"LNCloudPublic: unhandled $data : $some"
  }

  // ADDING NEW TOKENS

  def resolveSuccess(memo: BlindMemo) = getClearTokens(memo).doOnCompleted(me doProcess CMDStart)
    .subscribe(plus => me stayWith data.copy(info = None, tokens = plus ::: data.tokens),
      serverError => if (serverError.getMessage == "notfound") reset)

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

    lnCloud.call("blindtokens/buy", response => Invoice parse json2String(response.head),
      "seskey" -> memo.sesPubKeyHex, "tokens" -> memo.makeBlindTokens.toJson.toString.hex)
      .filter(_.sum.amount < 25000000L).map(augmentInvoice(_, qty.toInt) -> memo)
  }

  def getClearTokens(memo: BlindMemo) =
    lnCloud.call("blindtokens/redeem", _.map(json2String(_).bigInteger),
      "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

trait Pathfinder {
  val lnCloud: LNCloud
  val channel: Channel

  def makeOutgoingSpec(invoice: Invoice) =
    lnCloud.findRoutes(channel.data.announce.nodeId, invoice.nodeId) map {
      PaymentSpec.makeOutgoingSpec(_, invoice, firstExpiry = LNParams.myHtlcExpiry)
    }
}

// Concrete cloud acts

case class CheckCloudAct(data: BinaryData, kind: String = "CheckCloudAct") extends LNCloudAct {
  def run(params: Seq[HttpParam], cloud: LNCloud): Obs[Unit] = cloud.call("check", println, params:_*)
}

trait LNCloudAct {
  // Sending data to server using either token or sig auth
  def run(params: Seq[HttpParam], cloud: LNCloud): Obs[Unit]
  val data: BinaryData
}

// This is a basic interface to cloud which does not require a channel
// failover invariant will fall back to default in case of failure

trait BitcoinNodeProvider {
  def add(pr: PeerGroup): Unit = none
}

class LNCloud(url: String) extends BitcoinNodeProvider {
  def http(way: String) = post(s"$url:9001/v1/$way", true) connectTimeout 7500
  def call[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case err => throw new ProtocolException
    }

  def getRates = call("rates", _.head)
  def findNodes(aliasQuery: String) = call(command = "router/nodes",
    vector => for (json <- vector) yield json.convertTo[AnnounceChansNum],
    "query" -> aliasQuery)

  def findRoutes(from: BinaryData, to: PublicKey) = call("router/routes",
    vector => for (json <- vector) yield json.convertTo[PaymentRoute],
    "from" -> from.toString, "to" -> to.toString)
}

class FailoverLNCloud(failover: LNCloud, url: String) extends LNCloud(url) {
  override def add(peerGroup: PeerGroup) = peerGroup addAddress new PeerAddress(app.params, InetAddresses forString url, 8333)
  override def findNodes(aliasQuery: String) = super.findNodes(aliasQuery).onErrorResumeNext(_ => failover findNodes aliasQuery)
  override def getRates = super.getRates.onErrorResumeNext(_ => failover.getRates)
}

object LNCloud {
  type HttpParam = (String, Object)
  type ClearToken = (String, String, String)
  type InvoiceAndMemo = (Invoice, BlindMemo)
  val fromBlacklisted = "fromblacklisted"
  val CMDStart = "CMDStart"
  val body = "body"
}