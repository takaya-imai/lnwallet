package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.softwaremill.quicklens._
import com.lightning.wallet.lncloud.LNCloud._
import com.lightning.wallet.lncloud.JsonHttpUtils._

import rx.lang.scala.{Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.ln.wire.{LightningMessageCodecs, UpdateFulfillHtlc}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.NodeAnnouncements
import com.lightning.wallet.lncloud.DefaultLNCloudSaver.ClearToken
import com.lightning.wallet.lncloud.ImplicitConversions.string2Ops
import collection.JavaConverters.mapAsJavaMapConverter
import com.github.kevinsawicki.http.HttpRequest
import rx.lang.scala.schedulers.IOScheduler
import fr.acinq.bitcoin.Crypto.PublicKey
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Utils.HEX
import java.net.ProtocolException
import org.bitcoinj.core.ECKey
import scodec.bits.BitVector
import scala.util.Success


trait LNCloudAct {
  def runDefault(ct: ClearToken): Obs[Unit]
  def runStandalone(cloud: StandaloneCloud): Obs[Unit]
}

case class StandaloneLNCloudData(acts: List[LNCloudAct], url: String)
class StandaloneCloud extends StateMachine[StandaloneLNCloudData] with LNCloud { me =>
  lazy val prefix = LNParams.extendedCloudPrivateKey.publicKey.toString take 8

  def doProcess(change: Any) = (data, change) match {
    // We need to store it in case of server side error
    case (StandaloneLNCloudData(acts, url), act: LNCloudAct) =>
      me stayWith StandaloneLNCloudData(act :: acts take 1000, url)
      process(CMDStart)

    // Always try to process all the remaining acts
    case (StandaloneLNCloudData(act :: as, _), CMDStart) =>

      val result = act runStandalone me
      // We don't try again in case of error to avoid recursion
      result.foreach(_ => me stayWith data.copy(acts = as), none)
      result.foreach(_ => me process CMDStart, _ => me stayWith data)

    case _ =>
      // Let know if received an unhandled message in some state
      android.util.Log.d("StandaloneCloud", s"Unhandled $data : $change")
  }

  def sign(data: BinaryData) = Crypto encodeSignature {
    val signKey = LNParams.extendedCloudPrivateKey.privateKey
    Crypto.sign(data, signKey)
  }

  def tryIfWorks(data: BinaryData) = call("sig/check", identity,
    "sig" -> sign(data).toString, "data" -> data.toString, "prefix" -> prefix)

  override def http(command: String) = {
    val address = s"http://${data.url}/v1/$command"
    HttpRequest.post(address, true) connectTimeout 10000
  }
}

case class MemoAndInvoice(memo: BlindMemo, invoice: Invoice)
case class MemoAndSpec(memo: BlindMemo, spec: OutgoingPaymentSpec)
case class LNCloudData(info: Option[MemoAndInvoice], tokens: List[ClearToken] = Nil, acts: List[LNCloudAct] = Nil)
class DefaultLNCloud(bag: PaymentSpecBag) extends StateMachine[LNCloudData] with LNCloud with StateMachineListener { me =>

  private def reset = me stayWith data.copy(info = None)
  private def resetStart = wrap(me process CMDStart)(reset)

  def doProcess(change: Any) = (data, change) match {
    case (LNCloudData(None, Nil, _), CMDStart) => for {
      // Start request with empty tokens asks for a new payment
      Tuple2(invoice, memo) <- retry(getInfo, pickInc, 2 to 4 by 2)
      Some(spec) <- retry(makeOutgoingSpec(invoice), pickInc, 2 to 4 by 2)
    } me process MemoAndSpec(memo, spec)

    case (LNCloudData(None, _, _), mas: MemoAndSpec) =>
      // Eliminating possible race condition by locking a change here
      // We just hope our HTLC gets accepted but will also catch an error
      val memoAndInvoice = MemoAndInvoice(mas.memo, mas.spec.invoice)
      me stayWith data.copy(info = Some apply memoAndInvoice)
      // Sending a silent command ensures no alerts on ui
      LNParams.channel process SilentAddHtlc(mas.spec)

    // Our HTLC has been fulfilled so we can ask for tokens
    case (LNCloudData(Some(info), _, _), fulfill: UpdateFulfillHtlc)
      if fulfill.paymentHash == info.invoice.paymentHash =>
      me resolvePendingPayment info

    // No matter what the state is if we have acts and tokens
    case (LNCloudData(_, token :: tx, act :: ax), CMDStart) =>
      // 'data' may change while request is on so we always copy it
      def resolveError(srvError: Throwable) = srvError.getMessage match {
        case "tokeninvalid" | "tokenused" => me stayWith data.copy(tokens = tx)
        case _ => me stayWith data
      }

      val result = act.runDefault(token)
      // We don't try again in case of an error to avoid recursion
      result.foreach(_ => me stayWith data.copy(tokens = tx, acts = ax), none)
      result.foreach(_ => me process CMDStart, resolveError)

    // Start request while payment is in progress,
    // it may be finished already so we ask the bag
    case (LNCloudData(Some(info), _, _), CMDStart) =>
      bag getOutgoingPaymentSpec info.invoice.paymentHash match {
        case Success(spec) if spec.preimage.isDefined => me resolvePendingPayment info
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
      android.util.Log.d("DefaultLNCloud", s"Unhandled $data : $change")
  }

  // RESOLVING A WAIT PAYMENT STATE

  private def resolvePendingPayment(info: MemoAndInvoice) = {
    // In case of error we do nothing in hope it will be resolved later, but "notfound" means we have to reset
    val onSuccess: List[ClearToken] => Unit = plus => me stayWith data.copy(info = None, tokens = data.tokens ++ plus)
    getClearTokens(info.memo).map(onSuccess).subscribe(_ => me process CMDStart, err => if (err.getMessage == "notfound") reset)
  }

  // LISTENING TO CHANNEL

  override def onPostProcess = {
    case fulfill: UpdateFulfillHtlc =>
      // This may be our HTLC, check it
      me process fulfill
  }

  // TALKING TO SERVER

  def augmentInvoice(invoice: Invoice, quantity: Int) = {
    import com.lightning.wallet.R.string.ln_credits_invoice
    val text = app getString ln_credits_invoice format quantity
    invoice.modify(_.message) setTo Some(text)
  }

  def getInfo = call("blindtokens/info", identity) flatMap { raw =>
    val JsString(pubQ) +: JsString(pubR) +: JsNumber(qty) +: _ = raw
    val signerSessionPubKey = ECKey.fromPublicOnly(HEX decode pubR)
    val signerMasterPubKey = ECKey.fromPublicOnly(HEX decode pubQ)

    // Prepare a list of BlindParam and a list of BigInteger clear tokens for each BlindParam
    val blinder = new ECBlind(signerMasterPubKey.getPubKeyPoint, signerSessionPubKey.getPubKeyPoint)
    val memo = BlindMemo(blinder params qty.toInt, blinder tokens qty.toInt, signerSessionPubKey.getPublicKeyAsHex)

    call("blindtokens/buy", Invoice parse _.head.convertTo[String],
      "seskey" -> memo.sesPubKeyHex, "tokens" -> memo.makeBlindTokens.toJson.convertTo[String].hex)
        .filter(_.sum.amount < 50000000).map(inv => augmentInvoice(inv, qty.toInt) -> memo)
  }

  def getClearTokens(memo: BlindMemo) = call("blindtokens/redeem",
    sigHats => for (blind <- sigHats) yield blind.convertTo[String].bigInteger,
    "seskey" -> memo.sesPubKeyHex).map(memo.makeClearSigs).map(memo.pack)
}

trait LNCloud { me =>
  def http(command: String) = {
    val address = s"http://10.0.2.2:9002/v1/$command"
    HttpRequest.post(address, true) connectTimeout 10000
  }

  def call[T](command: String, trans: Vector[JsValue] => T, params: (String, Object)*) =
    obsOn(http(command).form(params.toMap.asJava).body.parseJson, IOScheduler.apply) map {
      case JsArray(JsString("error") +: JsString(why) +: _) => throw new ProtocolException(why)
      case JsArray(JsString("ok") +: responses) => trans(responses)
      case _ => throw new ProtocolException
    }

  def makeOutgoingSpec(invoice: Invoice) =
    findRoutes(LNParams.channel.data.announce.nodeId, invoice.nodeId) map { routes =>
      PaymentSpec.makeOutgoingSpec(routes, invoice, firstExpiry = LNParams.htlcExpiry)
    }

  def json2BitVec(js: JsValue): Option[BitVector] = BitVector fromHex js.convertTo[String]
  def findNodes(request: String): Obs[NodeAnnouncements] = call(command = "router/nodes/find",
    _.flatMap(json2BitVec).map(LightningMessageCodecs.nodeAnnouncementCodec.decode(_).require.value),
    "query" -> request)

  // A vector of vectors of hops
  def findRoutes(fromNodeId: BinaryData, toNodeId: PublicKey) = call("router/routes",
    _.flatMap(json2BitVec).map(LightningMessageCodecs.hopsCodec.decode(_).require.value),
    "from" -> fromNodeId.toString, "to" -> toNodeId.toString)
}

object LNCloud {
  val OPERATIONAL = "Operational"
  val CMDStart = "CMDStart"
}