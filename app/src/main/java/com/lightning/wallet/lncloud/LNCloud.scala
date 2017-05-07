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
import scala.util.Success


case class LNCloudDataPrivate(acts: List[LNCloudAct], url: String)
abstract class LNCloudPrivate extends StateMachine[LNCloudDataPrivate] with Pathfinder { me =>
  def tryIfWorks(dummy: BinaryData) = lnCloud.call("sig/check", identity, sign(dummy):_*)
  lazy val prefix = LNParams.extendedCloudPrivateKey.publicKey.toString take 8

  def doProcess(change: Any) = (data, change) match {
    case (LNCloudDataPrivate(acts, url), act: LNCloudAct) =>
      me stayWith data.copy(acts = act :: acts take 1000)
      me process CMDStart

    case (LNCloudDataPrivate(act :: ax, _), CMDStart) =>
      act.runPrivate(me).foreach(_ => me stayWith data.copy(acts = ax),
        none, (/* successfully completed */) => me process CMDStart)

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"LNCloudPrivate: unhandled $data : $change")
  }

  def sign(data: BinaryData) = {
    val signKey = LNParams.extendedCloudPrivateKey.privateKey
    val signature = Crypto encodeSignature Crypto.sign(data, signKey)
    Seq("sig" -> signature.toString, "prefix" -> prefix, body -> data.toString)
  }
}

case class LNCloudData(info: Option[InvoiceAndMemo], tokens: List[ClearToken] = Nil, acts: List[LNCloudAct] = Nil)
abstract class LNCloudPublic extends StateMachine[LNCloudData] with Pathfinder with StateMachineListener { me =>
  private def reset = me stayWith data.copy(info = None)
  val bag: PaymentSpecBag

  // LISTENING TO CHANNEL

  channel.events addListener me
  override def onPostProcess = {
    case fulfill: UpdateFulfillHtlc =>
      // This may be our HTLC, check it
      me process CMDStart
  }

  // STATE MACHINE

  def doProcess(change: Any) = (data, change) match {
    case LNCloudData(None, Nil, _) ~ CMDStart => for {
      Tuple2(invoice, memo) <- retry(getInfo, pickInc, 2 to 3)
      Some(spec) <- retry(makeOutgoingSpec(invoice), pickInc, 2 to 3)
    } me process Tuple2(spec, memo)

    // This payment request may arrive in some time after an initialization above,
    // hence we state that it can only be accepted if info == None to avoid race condition
    case LNCloudData(None, tokens, _) ~ Tuple2(spec: OutgoingPaymentSpec, memo: BlindMemo) =>
      me stayWith data.copy(info = Some(spec.invoice, memo), tokens = tokens)
      channel process SilentAddHtlc(spec)

    // No matter what the state is if we have acts and tokens
    case LNCloudData(_, (blindPoint, clearToken, clearSignature) :: tx, act :: ax) ~ CMDStart =>
      val base = Seq("point" -> blindPoint, "cleartoken" -> clearToken, "clearsig" -> clearSignature)
      act.runPublic(base, me).foreach(_ => me stayWith data.copy(tokens = tx, acts = ax), resolveError,
        (/* successfully completed */) => me process CMDStart)

      // 'data' may change while request is on so we always copy it
      def resolveError(servError: Throwable) = servError.getMessage match {
        case "tokeninvalid" | "tokenused" => me stayWith data.copy(tokens = tx)
        case _ => me stayWith data
      }

    // Start request while payment is in progress,
    // it may be finished already so we ask the bag
    case LNCloudData(Some(invoice ~ memo), _, _) ~ CMDStart =>
      bag.getOutgoingPaymentSpec(hash = invoice.paymentHash) match {
        case Success(spec) if spec.status == PaymentSpec.SUCCESS => me resolve memo
        case Success(spec) if spec.status == PaymentSpec.PERMANENT_FAIL => reset
        case Success(_) => me stayWith data
        case _ => reset
      }

    case (_, act: LNCloudAct) =>
      // We must always record incoming acts
      val acts1 = act :: data.acts take 1000
      me stayWith data.copy(acts = acts1)
      me process CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"LNCloudPublic: unhandled $data : $change")
  }

  // PAYMENT STATE MANAGEMENT

  def resolve(memo: BlindMemo) =
    // Discard info which is irrelevant now and add tokens
    // In case of error we do nothing, but "notfound" means we have to reset
    getClearTokens(memo).subscribe(plus => me stayWith data.copy(info = None, tokens = plus ::: data.tokens),
      err => if (err.getMessage == "notfound") reset, (/* after we got tokens */) => me process CMDStart)

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
    if (invoice.nodeId == LNParams.nodeId) Obs just None
    else lnCloud.findRoutes(channel.data.announce.nodeId, invoice.nodeId) map {
      PaymentSpec.makeOutgoingSpec(_, invoice, firstExpiry = LNParams.myHtlcExpiry)
    }
}

// This is a basic interface to cloud which does not require a channel
// failover invariant will fall back to default in case of failure

class LNCloud(url: String = "10.0.2.2:9002") {
  def http(way: String) = post(s"http://$url/v1/$way", true) connectTimeout 7500
  def call[T](command: String, process: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => process(responses)
      case err => throw new ProtocolException
    }

  def getRawData(key: String) = call("data/get", _.head, "key" -> key)
  def findNodes(query: String): Obs[NodeAnnouncements] = call("router/nodes",
    _.flatMap(json2BitVec).map(nodeAnnouncementCodec.decode(_).require.value),
    "query" -> query)

  // A vector of vectors of hops
  def findRoutes(from: BinaryData, to: PublicKey) = call("router/routes",
    _.flatMap(json2BitVec).map(hopsCodec.decode(_).require.value),
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
  val fromBlacklisted = "fromblacklisted"
  val body = "body"

  val OPERATIONAL = "Operational"
  val CMDStart = "CMDStart"
}