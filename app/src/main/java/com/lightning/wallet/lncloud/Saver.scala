package com.lightning.wallet.lncloud

import spray.json._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._

import rx.lang.scala.{Scheduler, Observable => Obs}
import com.lightning.wallet.lncloud.RatesSaver.RatesMap
import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.helper.Statistics
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.LNParams
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scala.util.Try


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
  def tryGet: Try[String] = StorageWrap.get(KEY)
  def save(snap: JsValue) = StorageWrap.put(snap.toString, KEY)
}

object LNCloudPublicSaver extends Saver {
  def tryGetObject = tryGet map to[LNCloudData]
  def saveObject(data: LNCloudData) = save(data.toJson)
  val KEY = "lnCloudPublic1"
}

object LNCloudPrivateSaver extends Saver {
  def tryGetObject = tryGet map to[LNCloudDataPrivate]
  def saveObject(data: LNCloudDataPrivate) = save(data.toJson)
  def remove = LNParams.db.change(Storage.killSql, KEY)
  val KEY = "lnCloudPrivate1"
}

trait Rate { def now: Double }
trait ExchangeRateProvider { val usd, eur, cny: Rate }
case class AskRate(ask: Double) extends Rate { def now = ask }
case class LastRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends ExchangeRateProvider
case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends ExchangeRateProvider
case class Rates(exchange: RatesMap, feeHistory: Seq[Double], feeLive: Coin, feeRisky: Coin, stamp: Long)

object RatesSaver extends Saver { me =>
  type BitpayRatesList = List[BitpayRate]
  type BitpayRatesMap = Map[String, BitpayRate]
  type RatesMap = Map[String, Double]
  val KEY = "rates10"

  def toRates(src: ExchangeRateProvider): RatesMap = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(map: BitpayRatesMap): RatesMap = Map(strDollar -> map("USD").now, strEuro -> map("EUR").now, strYuan -> map("CNY").now)
  def bitpayNormalize(src: BitpayRatesList): RatesMap = toRates(src.map(bitpayRate => bitpayRate.code -> bitpayRate).toMap)

  private def pickExchangeRate = retry(obsOn(random nextInt 3 match {
    case 0 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case 1 => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
    case _ => me bitpayNormalize jsonFieldAs[BitpayRatesList]("data")(get("https://bitpay.com/rates").body)
  }, IOScheduler.apply), pickInc, 1 to 3)

  private def pickFeeRate(orderNum: Int) = obsOn(orderNum match {
    case 0 => jsonFieldAs[Double]("9")(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body)
    case 1 => jsonFieldAs[Double]("9")(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body)
    case 2 => jsonFieldAs[Double]("9")(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body)
    case _ => jsonFieldAs[Double]("9")(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body)
  }, IOScheduler.apply)

  private val defaultFee = 0.0005D
  private val updatePeriod = 30.minutes
  private val statistics = new Statistics[Double] { def extract(item: Double) = item }
  var rates = tryGet map to[Rates] getOrElse updatedRates(Nil, Map.empty).copy(stamp = 0L)

  def updatedRates(fees: Seq[Double], fiat: RatesMap) = {
    val fees1 = if (fees.isEmpty) defaultFee :: Nil else fees take 10
    val feeLive = Coin parseCoin statistics.meanWithin(fees1, stdDevs = 1)
    Rates(fiat, fees1, feeLive, feeLive div 2, System.currentTimeMillis)
  }

  private def startPeriodicDataUpdate(initDelay: Long) = {
    // Periodically update fee and fiat rates, initially delayed to limit network activity
    def attempts = retry(pickFeeRate(random nextInt 4), pickInc, 1 to 3).zip(pickExchangeRate)
    def periodic = withDelay(attempts.repeatWhen(_ delay updatePeriod), initDelay, updatePeriod.toMillis)

    periodic foreach { case (fee1, fiat) =>
      val updatedFees = fee1 +: rates.feeHistory
      rates = updatedRates(updatedFees, fiat)
      save(rates.toJson)
    }
  }

  def process =
    // If fees data is too old we reload all the fees from all the sources once, then proceed normally
    if (System.currentTimeMillis - rates.stamp < 5.days.toMillis) startPeriodicDataUpdate(rates.stamp) else {
      def allFees = for (ordNum <- 0 until 4) yield pickFeeRate(ordNum).map(Option.apply).onErrorReturn(_ => None)
      def attempt = Obs.zip(Obs from allFees).map(_.flatten).zip(pickExchangeRate)
      startPeriodicDataUpdate(System.currentTimeMillis)

      attempt foreach { case (fees, fiat) =>
        rates = updatedRates(fees, fiat)
        save(rates.toJson)
      }
    }
}