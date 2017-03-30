package com.lightning.wallet.helper

import spray.json._
import JsonHttpUtils._
import DefaultJsonProtocol._

import scala.util.{Success, Try}
import rx.lang.scala.{Scheduler, Observable => Obs}
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import com.lightning.wallet.Utils.{nullFail, strDollar, strEuro, strYuan}
import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.ln.Tools.random
import rx.lang.scala.schedulers.IOScheduler
import org.bitcoinj.core.Coin


object JsonHttpUtils {
  def obsOn[T](provider: => T, scheduler: Scheduler): Obs[T] =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  type IntervalPicker = (Throwable, Int) => Duration
  def retry[T](obs: Obs[T], pick: IntervalPicker, times: Range): Obs[T] =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  val get = HttpRequest.get(_: String, true) connectTimeout 10000
  def pickInc(err: Throwable, next: Int): FiniteDuration = next.seconds
  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  def jsonFieldAs[T : JsonFormat](field: String)(raw: String) =
    raw.parseJson.asJsObject.fields(field).convertTo[T]
}

// Fiat rates containers
trait Rate { def now: Double }
case class AskRate(ask: Double) extends Rate { def now = ask }
case class LastRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }

trait RateProvider { val usd, eur, cny: Rate }
case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends RateProvider
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends RateProvider

object FiatRates { me =>
  type RatesMap = Map[String, Rate]
  type BitpayList = List[BitpayRate]
  type PriceMap = Map[String, Double]
  var rates: Try[PriceMap] = nullFail

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")

  // Normalizing incoming json data and converting it to rates map
  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = src.parseJson.asJsObject.fields("data").convertTo[BitpayList].map(rt => rt.code -> rt).toMap

  def pickUrl = random nextInt 3 match {
    case 0 => me toRates bitpayNorm(get("https://bitpay.com/rates").body)
    case 1 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case _ => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
  }

  def go = retry(obsOn(pickUrl, IOScheduler.apply), pickInc, 1 to 4 by 2)
    .repeatWhen(_ delay 15.minute).subscribe(prov => rates = Success apply prov)
}

object Fee {
  var live = Coin valueOf 30000L
  def risky = live div 3

  private def fromCore(raw: String, block: String) =
    Coin parseCoin jsonFieldAs[BigDecimal](block)(raw).abs.toString

  def pickUrl = random nextInt 4 match {
    case 0 => fromCore(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 1 => fromCore(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 2 => fromCore(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body, "9")
    case _ => fromCore(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body, "9")
  }

  def go = retry(obsOn(pickUrl, IOScheduler.apply), pickInc, 1 to 4 by 2)
    .repeatWhen(_ delay 15.minute).subscribe(msat => live = msat)
}

// Tx Insight API formats
case class InsightOutput(spentTxId: Option[String], n: Int)
case class InsightTx(txid: String, vout: List[InsightOutput], confirmations: Int)

object Insight {
  implicit val outFmt = jsonFormat[Option[String], Int, InsightOutput](InsightOutput, "spentTxId", "n")
  implicit val txFmt = jsonFormat[String, List[InsightOutput], Int, InsightTx](InsightTx, "txid", "vout", "confirmations")

  def txInfo(txid: String) = obsOn(random nextInt 5 match {
    //case 0 => get(s"https://localbitcoinschain.com/api/tx/$txid").body
    //case 1 => get(s"https://insight.bitpay.com/api/tx/$txid").body
    case _ => get(s"https://testnet.blockexplorer.com/api/tx/$txid").body
    //case 3 => get(s"https://live.coin.space/api/tx/$txid").body
    //case _ => get(s"https://bitlox.io/api/tx/$txid").body
  }, IOScheduler.apply) map to[InsightTx]

  def rawTx(txid: String) = obsOn(random nextInt 5 match {
    case 1 => get(s"https://testnet-api.smartbit.com.au/v1/blockchain/tx/$txid").body
    case _ => get(s"https://testnet-api.smartbit.com.au/v1/blockchain/tx/$txid").body
  }, IOScheduler.apply) map fr.acinq.bitcoin.Transaction.read

  def retryTxInfo(txid: String) = retry(txInfo(txid), pickInc, 1 to 4 by 2)
  def retryRawTx(txid: String) = retry(rawTx(txid), pickInc, 1 to 4 by 2)
}