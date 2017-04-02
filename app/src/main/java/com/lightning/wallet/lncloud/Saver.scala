package com.lightning.wallet.lncloud

import spray.json._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.ln.JavaSerializer._
import com.lightning.wallet.lncloud.RatesSaver._
import com.lightning.wallet.lncloud.JsonHttpUtils._

import com.lightning.wallet.ln.{Channel, ChannelData, ScheduledTx}
import rx.lang.scala.{Scheduler, Observable => Obs}
import scala.util.{Success, Try}

import com.github.kevinsawicki.http.HttpRequest
import rx.lang.scala.schedulers.IOScheduler
import scala.collection.mutable
import org.bitcoinj.core.Coin
import spray.json.JsonFormat


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

trait Saver {
  type Snapshot
  val KEY: String

  def save(snap: Snapshot): Unit = StorageWrap.put(serialize(snap), KEY)
  def tryGet: Try[Snapshot] = StorageWrap get KEY map deserialize[Snapshot]
}

object BlindTokensSaver extends Saver {
  val initState: Snapshot = (Set.empty, BlindTokens.OPERATIONAL :: Nil, null)
  override type Snapshot = (Set[ClearToken], List[String], BlindProgress)
  type ClearToken = (String, String, String)
  // Blinding point, clear token and sig
  val KEY = "blindTokens"
}

object ChannelSaver extends Saver {
  val initState: Snapshot = (Channel.INITIALIZED :: Nil, null)
  override type Snapshot = (List[String], ChannelData)
  val KEY = "channel"
}

object StandaloneCloudSaver extends Saver {
  def remove = app.db.change(Storage.killSql, KEY)
  override type Snapshot = String
  val KEY = "standaloneCloud"
}

object BroadcasterSaver extends Saver {
  override type Snapshot = mutable.Set[ScheduledTx]
  val KEY = "broadcaster"
}

// Fiat rates containers
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }
case class LastRate(last: Double) extends Rate { def now = last }
case class AskRate(ask: Double) extends Rate { def now = ask }
trait Rate { def now: Double }

case class Rates(exchange: PriceMap, fee: Coin, stamp: Long)
case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends RateProvider
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends RateProvider
trait RateProvider { val usd, eur, cny: Rate }

object RatesSaver extends Saver { me =>
  type PriceMap = Map[String, Double]
  type BitpayList = List[BitpayRate]
  type RatesMap = Map[String, Rate]
  override type Snapshot = Rates
  val KEY = "rates"

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")

  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = src.parseJson.asJsObject.fields("data").convertTo[BitpayList].map(rt => rt.code -> rt).toMap
  def fromCoreFee(raw: String, block: String) = Coin parseCoin jsonFieldAs[BigDecimal](block)(raw).abs.toString

  private def pickFeeRate = retry(obsOn(random nextInt 4 match {
    case 0 => fromCoreFee(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 1 => fromCoreFee(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 2 => fromCoreFee(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body, "9")
    case _ => fromCoreFee(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body, "9")
  }, IOScheduler.apply), pickInc, 1 to 4 by 2).repeatWhen(_ delay 20.minute)

  private def pickExchangeRate = retry(obsOn(random nextInt 3 match {
    case 0 => me toRates bitpayNorm(get("https://bitpay.com/rates").body)
    case 1 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case _ => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
  }, IOScheduler.apply), pickInc, 1 to 4 by 2).repeatWhen(_ delay 20.minute)

  def rates = tryGet match {
    case Success(savedRates) => savedRates
    case _ => Rates(Map.empty, Coin valueOf 60000, 0L)
  }

  def process = {
    val span = 20.minutes.toMillis
    val timeLag = System.currentTimeMillis - rates.stamp
    val delayed = if (span - timeLag < 0) 0L else span - timeLag
    val combined = pickExchangeRate zip pickFeeRate delay delayed.millis

    combined foreach { case (exchange, fee) =>
      val newFee = List(fee, Coin valueOf 60000).sorted.last
      me save Rates(exchange, newFee, System.currentTimeMillis)
    }
  }
}