package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.softwaremill.quicklens._
import com.lightning.wallet.lncloud.LNCloud._
import com.lightning.wallet.lncloud.JsonHttpUtils._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.lightning.wallet.ln.Tools.{errLog, wrap, none}
import com.lightning.wallet.ln.wire.{LightningMessageCodecs, UpdateFulfillHtlc}
import com.lightning.wallet.lncloud.ImplicitJsonFormats.{json2BitVec, jsonToString}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.NodeAnnouncements
import com.lightning.wallet.lncloud.DefaultLNCloudSaver.ClearToken
import com.lightning.wallet.lncloud.ImplicitConversions.string2Ops
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest.post
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import scala.util.Success


trait LNCloudAct {
  def runDefault(base: Seq[HttpParam], cloud: DefaultLNCloud): Obs[Unit]
  def runStandalone(cloud: StandaloneCloud): Obs[Unit]
}

// Used mainly for testing purposes
case class PingCloudAct(data: String, kind: String = "PingCloudAct") extends LNCloudAct {
  def runDefault(base: Seq[HttpParam], cloud: DefaultLNCloud) = cloud.send(base :+ ("data", data), "ping")
  def runStandalone(cloud: StandaloneCloud): Obs[Unit] = cloud.send(params = Seq("data" -> data), "ping")
}

case class StandaloneLNCloudData(acts: List[LNCloudAct], url: String)
abstract class StandaloneCloud extends StateMachine[StandaloneLNCloudData] with LNCloud { me =>
  override def http(way: String) = post(s"http://${data.url}/v1/$way", true) connectTimeout 10000
  lazy val prefix = LNParams.extendedCloudPrivateKey.publicKey.toString take 8

  def doProcess(change: Any) = (data, change) match {
    case (StandaloneLNCloudData(acts, url), act: LNCloudAct) =>
      me stayWith StandaloneLNCloudData(act :: acts take 1000, url)
      me process CMDStart

    // Always try to process all the remaining acts
    case (StandaloneLNCloudData(act :: ax, _), CMDStart) =>
      act.runStandalone(me).foreach(_ => me stayWith data.copy(acts = ax),
        errLog, (/* call successfully completed */) => me process CMDStart)

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"StandaloneCloud: unhandled $data : $change")
  }

  def sign(data: BinaryData) = Crypto encodeSignature {
    val signKey = LNParams.extendedCloudPrivateKey.privateKey
    Crypto.sign(data, signKey)
  }

  def tryIfWorks(data: BinaryData) = call(command = "sig/check", identity,
    "sig" -> sign(data).toString, "data" -> data.toString, "prefix" -> prefix)
}

case class MemoAndSpec(memo: BlindMemo, spec: OutgoingPaymentSpec)
case class LNCloudData(info: Option[MemoAndSpec], tokens: List[ClearToken] = Nil, acts: List[LNCloudAct] = Nil)
abstract class DefaultLNCloud extends StateMachine[LNCloudData] with LNCloud with StateMachineListener { me =>

  val bag: PaymentSpecBag
  private def reset = me stayWith data.copy(info = None)
  private def resetStart = wrap(me process CMDStart)(reset)

  // LISTENING TO CHANNEL

  channel.events addListener me
  override def onPostProcess = {
    case fulfill: UpdateFulfillHtlc =>
      // This may be our HTLC, check it
      me process fulfill
  }

  // STATE MACHINE STUFF

  def doProcess(change: Any) = (data, change) match {
    case (LNCloudData(None, Nil, _), CMDStart) => for {
      Tuple2(invoice, memo) <- retry(getInfo, pickInc, 1 to 3)
      Some(spec) <- retry(makeOutgoingSpec(invoice), pickInc, 1 to 3)
    } me process MemoAndSpec(memo, spec)

    case (LNCloudData(None, tokens, acts), mas: MemoAndSpec) =>
      // Eliminating possible race condition by locking a change here
      // We hope HTLC gets accepted but will also catch an error
      me stayWith LNCloudData(info = Some(mas), tokens, acts)
      channel process SilentAddHtlc(mas.spec)

    // Our HTLC has been fulfilled so we can ask for tokens
    case (LNCloudData(Some(info), _, _), fulfill: UpdateFulfillHtlc)
      if fulfill.paymentHash == info.spec.invoice.paymentHash =>
      me resolvePayment info

    // No matter what the state is if we have acts and tokens
    case (LNCloudData(_, (blindPoint, clearToken, clearSignature) :: tx, act :: ax), CMDStart) =>
      val base = Seq("point" -> blindPoint, "cleartoken" -> clearToken, "clearsig" -> clearSignature)
      act.runDefault(base, me).foreach(_ => me stayWith data.copy(tokens = tx, acts = ax),
        resolveError, (/* call successfully completed */) => me process CMDStart)

      // 'data' may change while request is on so we always copy it
      def resolveError(servError: Throwable) = servError.getMessage match {
        case "tokeninvalid" | "tokenused" => me stayWith data.copy(tokens = tx)
        case _ => me stayWith data
      }

    // Start request while payment is in progress,
    // it may be finished already so we ask the bag
    case (LNCloudData(Some(info), _, _), CMDStart) =>
      bag getOutgoingPaymentSpec info.spec.invoice.paymentHash match {
        case Success(spec) if spec.status == PaymentSpec.SUCCESS => me resolvePayment info
        case Success(spec) if spec.status == PaymentSpec.PERMANENT_FAIL => resetStart
        case Success(_) => me stayWith data
        case _ => resetStart
      }

    case (_, act: LNCloudAct) =>
      // We must always record incoming acts
      val acts1 = act :: data.acts take 1000
      me stayWith data.copy(acts = acts1)
      me process CMDStart

    case _ =>
      // Let know if received an unhandled message in some state
      Tools.log(s"DefaultLNCloud: unhandled $data : $change")
  }

  // RESOLVING A WAIT PAYMENT STATE

  private def resolvePayment(info: MemoAndSpec) =
    // In case of error we do nothing in hope it will be resolved later, but "notfound" means we have to reset
    getClearTokens(info.memo).subscribe(plus => me stayWith data.copy(info = None, tokens = plus ::: data.tokens),
      err => if (err.getMessage == "notfound") reset, (/* after we got tokens */) => me process CMDStart)

  // TALKING TO SERVER

  def augmentInvoice(invoice: Invoice, howMuch: Int) = {
    import com.lightning.wallet.R.string.ln_credits_invoice
    val text = app getString ln_credits_invoice format howMuch
    invoice.modify(_.message) setTo Some(text)
  }

  def getInfo = call("blindtokens/info", identity) flatMap { raw =>
    val JsString(pubQ) +: JsString(pubR) +: JsNumber(qty) +: _ = raw
    val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubR)
    val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubQ)

    // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
    val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
    val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)

    call("blindtokens/buy", jsonToString andThen Invoice.parse apply _.head,
      "seskey" -> memo.sesPubKeyHex, "tokens" -> memo.makeBlindTokens.toJson.toString.hex)
        .filter(_.sum.amount < 50000000).map(augmentInvoice(_, qty.toInt) -> memo)
  }

  def getClearTokens(memo: BlindMemo) = call("blindtokens/redeem",
    sigHats => for (blind <- sigHats) yield jsonToString(blind).bigInteger,
    "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

trait LNCloud { me =>
  val channel: StateMachine[ChannelData]
  def makeOutgoingSpec(invoice: Invoice) =
    findRoutes(channel.data.announce.nodeId, invoice.nodeId) map { routes =>
      PaymentSpec.makeOutgoingSpec(routes, invoice, LNParams.myHtlcExpiry)
    }

  def http(way: String) = {
    val address = s"http://10.0.2.2:9002/v1/$way"
    post(address, true) connectTimeout 10000
  }

  def call[T](command: String, trans: Vector[JsValue] => T, params: HttpParam*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => trans(responses)
      case err => throw new ProtocolException
    }

  // We are only interested if a call has been successful, not in it's specifics
  def send(params: Seq[HttpParam], command: String) = call(command, none, params:_*)
  def findNodes(request: String): Obs[NodeAnnouncements] = call(command = "router/nodes/find",
    _.flatMap(json2BitVec).map(LightningMessageCodecs.nodeAnnouncementCodec.decode(_).require.value),
    "query" -> request)

  // A vector of vectors of hops
  def findRoutes(fromNodeId: BinaryData, toNodeId: PublicKey) = call("router/routes",
    _.flatMap(json2BitVec).map(LightningMessageCodecs.hopsCodec.decode(_).require.value),
    "from" -> fromNodeId.toString, "to" -> toNodeId.toString)
}

object LNCloud {
  type HttpParam = (String, Object)
  val OPERATIONAL = "Operational"
  val CMDStart = "CMDStart"
}