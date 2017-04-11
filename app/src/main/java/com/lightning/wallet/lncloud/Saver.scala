package com.lightning.wallet.lncloud

import java.io._
import spray.json._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.lncloud.RatesSaver._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.JavaSerializer._

import com.lightning.wallet.ln.{ChannelData, ScheduledTx}
import rx.lang.scala.{Scheduler, Observable => Obs}
import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.helper.Statistics
import rx.lang.scala.schedulers.IOScheduler
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try

object JavaSerializer {
  def serialize(source: Serializable): String = {
    val output: ByteArrayOutputStream = new ByteArrayOutputStream
    new ObjectOutputStream(output) writeObject source
    HEX encode output.toByteArray
  }

  def deserialize[T](hex: String): T = {
    val input = new ByteArrayInputStream(HEX decode hex)
    new ObjectInputStream(input).readObject.asInstanceOf[T]
  }
}

object JsonHttpUtils {
  def obsOn[T](provider: => T, scheduler: Scheduler): Obs[T] =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  def withDelay[T](obs: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0) 0L else adjustedTimeout
    obs.delay(delayLeft.millis)
  }

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
  val KEY: String
  protected def tryGet[T]: Try[T] = StorageWrap get KEY map deserialize[T]
  protected def save(snap: Serializable) = StorageWrap.put(serialize(snap), KEY)
}

object BlindTokensSaver extends Saver {
  // Blinding point, clear token and sig
  type ClearToken = (String, String, String)
  type Snapshot = (Set[ClearToken], List[String], BlindProgress)
  def tryGetObject: Try[Snapshot] = tryGet[Snapshot]
  val KEY = "blindTokens"
}

object ChannelSaver extends Saver {
  type Snapshot = (List[String], ChannelData)
  def tryGetObject: Try[Snapshot] = tryGet[Snapshot]
  val KEY = "channel"
}

object StandaloneCloudSaver extends Saver {
  def remove = app.db.change(Storage.killSql, KEY)
  def saveUrl(url: String) = StorageWrap.put(url, KEY)
  def tryGetUrl: Try[String] = StorageWrap.get(KEY)
  val KEY = "standaloneCloud"
}

object LocalBroadcasterSaver extends Saver {
  type Snapshot = (ScheduledTxs, ScheduledTxs)
  type ScheduledTxs = scala.collection.mutable.Set[ScheduledTx]
  def tryGetObject: Try[Snapshot] = tryGet[Snapshot]
  val KEY = "broadcaster"
}

// Exchange rates and Bitcoin fees
case class Rates(exchange: PriceMap, feeLive: Coin, feeRisky: Coin, stamp: Long)
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }
case class LastRate(last: Double) extends Rate { def now = last }
case class AskRate(ask: Double) extends Rate { def now = ask }
trait Rate { def now: Double }

case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends RateProvider
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends RateProvider
trait RateProvider { val usd, eur, cny: Rate }

object RatesSaver extends Saver { me =>
  type PriceMap = Map[String, Double]
  type BitpayList = List[BitpayRate]
  type RatesMap = Map[String, Rate]
  val KEY = "rates3"

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")

  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = jsonFieldAs[BitpayList]("data")(src).map(bitpay => bitpay.code -> bitpay).toMap

  private def pickExchangeRate = retry(obsOn(random nextInt 3 match {
    case 0 => me toRates bitpayNorm(get("https://bitpay.com/rates").body)
    case 1 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case _ => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
  }, IOScheduler.apply), pickInc, 1 to 4 by 2)

  private def pickFeeRate(orderNum: Int) = obsOn(orderNum match {
    case 0 => Some(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body) map jsonFieldAs[Double]("9")
    case 1 => Some(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body) map jsonFieldAs[Double]("9")
    case 2 => Some(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body) map jsonFieldAs[Double]("9")
    case _ => Some(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body) map jsonFieldAs[Double]("9")
  }, IOScheduler.apply)

  private val period = 60.minutes
  private val defaultFee = Coin valueOf 40000
  private val statistics = new Statistics[Double] { def extract(item: Double) = item }
  var rates = tryGet[Rates] getOrElse Rates(Map.empty, defaultFee, defaultFee div 2, 0L)

  def process = {
    def allFees = for (orderNum <- 0 until 4) yield pickFeeRate(orderNum).onErrorReturn(_ => None)
    def combined = Obs.zip(Obs from allFees).map(_.flatten).zip(pickExchangeRate).repeatWhen(_ delay period)

    withDelay(combined, rates.stamp, period.toMillis) foreach { case (fees, fiatRates) =>
      val fee1 = if (fees.isEmpty) defaultFee else Coin parseCoin statistics.meanWithin(fees, 1)
      rates = Rates(fiatRates, fee1, fee1 div 2, System.currentTimeMillis)
      me save rates
    }
  }
}