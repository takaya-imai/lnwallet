package com.lightning.wallet.lncloud

import java.io._
import spray.json._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.JavaSerializer._

import rx.lang.scala.{Scheduler, Observable => Obs}
import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.helper.Statistics
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.LNParams
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

object DefaultLNCloudSaver extends Saver {
  type ClearToken = (String, String, String)
  def tryGetObject = tryGet[LNCloudData]
  val KEY = "lnCloud"
}

object StandaloneCloudSaver extends Saver {
  def remove = LNParams.db.change(Storage.killSql, KEY)
  def tryGetObject = tryGet[StandaloneLNCloudData]
  val KEY = "standaloneCloud"
}

// Exchange rates and fees
trait Rate { def now: Double }
case class AskRate(ask: Double) extends Rate { def now = ask }
case class LastRate(last: Double) extends Rate { def now = last }
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }

trait RateProvider { val usd, eur, cny: Rate }
case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends RateProvider
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends RateProvider
case class Rates(exchange: Map[String, Double], feeHistory: Seq[Double],
                 feeLive: Coin, feeRisky: Coin, stamp: Long)

object RatesSaver extends Saver { me =>
  type RatesMap = Map[String, Double]
  type BitpayList = List[BitpayRate]
  val KEY = "rates1"

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")
  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)

  def toRates(src: BitpayList) = {
    // Bitpay provides a list so we convert it to a map
    val map = src.map(bitpayRate => bitpayRate.code -> bitpayRate).toMap
    Map(strDollar -> map("USD").now, strEuro -> map("EUR").now, strYuan -> map("CNY").now)
  }

  private def pickExchangeRate = retry(obsOn(random nextInt 3 match {
    case 0 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case 1 => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
    case _ => me toRates jsonFieldAs[BitpayList]("data")(get("https://bitpay.com/rates").body)
  }, IOScheduler.apply), pickInc, 1 to 3)

  private def pickFeeRate(orderNum: Int) = obsOn(orderNum match {
    case 0 => jsonFieldAs[Double]("9")(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body)
    case 1 => jsonFieldAs[Double]("9")(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body)
    case 2 => jsonFieldAs[Double]("9")(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body)
    case _ => jsonFieldAs[Double]("9")(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body)
  }, IOScheduler.apply)

  private val defaultFee = 0.0005D
  private val updatePeriod = 30.minutes
  private val statistics = new Statistics[Double] {def extract(item: Double) = item }
  var rates = tryGet[Rates] getOrElse updatedRates(Nil, Map.empty).copy(stamp = 0L)

  private def updatedRates(fees: Seq[Double], fiat: RatesMap) = {
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
      me save rates
    }
  }

  def process =
    // If fees data is too old we reload all the fees from all the sources once, then proceed normally
    if (System.currentTimeMillis - rates.stamp < 4.days.toMillis) startPeriodicDataUpdate(rates.stamp) else {
      def allFees = for (ordNum <- 0 until 4) yield pickFeeRate(ordNum).map(Option.apply).onErrorReturn(_ => None)
      def attempt = Obs.zip(Obs from allFees).map(_.flatten).zip(pickExchangeRate)
      startPeriodicDataUpdate(System.currentTimeMillis)

      attempt foreach { case (fees, fiat) =>
        rates = updatedRates(fees, fiat)
        me save rates
      }
    }
}