package com.lightning.wallet.lncloud

import spray.json._
import boopickle.Default._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.Tools._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.lncloud.RatesSaver._
import com.lightning.wallet.lncloud.ImplicitVals._
import com.lightning.wallet.lncloud.JsonHttpUtils._

import com.lightning.wallet.ln.{ChannelData, ScheduledTx}
import rx.lang.scala.{Scheduler, Observable => Obs}
import scala.util.{Success, Try}

import com.github.kevinsawicki.http.HttpRequest
import org.bitcoinj.core.ECKey.CURVE.getCurve
import rx.lang.scala.schedulers.IOScheduler
import org.bitcoinj.core.Utils.HEX
import org.bitcoinj.core.Coin
import spray.json.JsonFormat
import scodec.bits.BitVector
import java.nio.ByteBuffer


// Fiat rates containers
case class Rates(exchange: PriceMap, feeRisky: Coin, feeLive: Coin, stamp: Long)
case class BitpayRate(code: String, rate: Double) extends Rate { def now = rate }
case class LastRate(last: Double) extends Rate { def now = last }
case class AskRate(ask: Double) extends Rate { def now = ask }
trait Rate { def now: Double }

case class Blockchain(usd: LastRate, eur: LastRate, cny: LastRate) extends RateProvider
case class Bitaverage(usd: AskRate, eur: AskRate, cny: AskRate) extends RateProvider
trait RateProvider { val usd, eur, cny: Rate }

object ImplicitVals {
  import com.lightning.wallet.ln.wire.LightningMessageCodecs.nodeAnnouncementCodec
  implicit val bigIntegerPickler = transformPickler { s: String => s.bigInteger } (_.toString)
  implicit val ecPointPickler = transformPickler(getCurve.decodePoint)(_ getEncoded true)
  implicit val coinPickler = transformPickler(Coin.valueOf)(_.getValue)

  implicit val announcePickler = transformPickler { raw: Bytes =>
    nodeAnnouncementCodec.decode(BitVector apply raw).require.value
  } (nodeAnnouncementCodec.encode(_).require.toByteArray)

  implicit val askRateFmt = jsonFormat[Double, AskRate](AskRate, "ask")
  implicit val lastRateFmt = jsonFormat[Double, LastRate](LastRate, "last")
  implicit val bitpayRateFmt = jsonFormat[String, Double, BitpayRate](BitpayRate, "code", "rate")
  implicit val bitaverageFmt = jsonFormat[AskRate, AskRate, AskRate, Bitaverage](Bitaverage, "USD", "EUR", "CNY")
  implicit val blockchainFmt = jsonFormat[LastRate, LastRate, LastRate, Blockchain](Blockchain, "USD", "EUR", "CNY")
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
  protected def tryGet: Try[ByteBuffer] = StorageWrap.get(KEY) map HEX.decode map ByteBuffer.wrap
  protected def save(snapshot: Bytes): Unit = StorageWrap.put(HEX encode snapshot, KEY)
}

object BlindTokensSaver extends Saver {
  // Blinding point, clear token and sig
  type ClearToken = (String, String, String)
  type Snapshot = (Set[ClearToken], List[String], BlindProgress)
  def saveObject(snap: Snapshot) = save(Pickle.intoBytes(snap).array)
  def tryGetObject = tryGet map Unpickle[Snapshot].fromBytes
  val KEY = "blindTokens10"
}

object ChannelSaver extends Saver {
  type Snapshot = (List[String], ChannelData)
  def saveObject(snap: Snapshot) = save(Pickle.intoBytes(snap).array)
  def tryGetObject = tryGet map Unpickle[Snapshot].fromBytes
  val KEY = "channel10"
}

object StandaloneCloudSaver extends Saver {
  def remove = app.db.change(Storage.killSql, KEY)
  def saveUrl(url: String) = StorageWrap.put(url, KEY)
  def tryGetUrl: Try[String] = StorageWrap.get(KEY)
  val KEY = "standaloneCloud10"
}

object LocalBroadcasterSaver extends Saver {
  type ScheduledTxs = scala.collection.mutable.Set[ScheduledTx]
  def saveObject(snap: LocalBroadcaster) = save(Pickle.intoBytes(snap).array)
  def tryGetObject = tryGet map Unpickle[LocalBroadcaster].fromBytes
  val KEY = "broadcaster10"
}

object RatesSaver extends Saver { me =>
  private val defFee = Coin valueOf 60000
  private val period = 20.minutes

  type PriceMap = Map[String, Double]
  type BitpayList = List[BitpayRate]
  type RatesMap = Map[String, Rate]
  val KEY = "rates10"

  def toRates(src: RateProvider) = Map(strDollar -> src.usd.now, strEuro -> src.eur.now, strYuan -> src.cny.now)
  def toRates(src: RatesMap) = Map(strDollar -> src("USD").now, strEuro -> src("EUR").now, strYuan -> src("CNY").now)
  def bitpayNorm(src: String) = jsonFieldAs[BitpayList]("data")(src).map(bitpay => bitpay.code -> bitpay).toMap
  def fromCoreFee(src: String, ord: String) = Coin parseCoin jsonFieldAs[BigDecimal](ord)(src).abs.toString

  private def pickFeeRate = retry(obsOn(random nextInt 4 match {
    case 0 => fromCoreFee(get("https://bitlox.io/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 1 => fromCoreFee(get("https://live.coin.space/api/utils/estimatefee?nbBlocks=9").body, "9")
    case 2 => fromCoreFee(get("https://blockexplorer.com/api/utils/estimatefee?nbBlocks=9").body, "9")
    case _ => fromCoreFee(get("https://localbitcoinschain.com/api/utils/estimatefee?nbBlocks=9").body, "9")
  }, IOScheduler.apply), pickInc, 1 to 4 by 2)

  private def pickExchangeRate = retry(obsOn(random nextInt 3 match {
    case 0 => me toRates bitpayNorm(get("https://bitpay.com/rates").body)
    case 1 => me toRates to[Blockchain](get("https://blockchain.info/ticker").body)
    case _ => me toRates to[Bitaverage](get("https://api.bitcoinaverage.com/ticker/global/all").body)
  }, IOScheduler.apply), pickInc, 1 to 4 by 2)

  var rates: Rates =
    tryGet map Unpickle[Rates].fromBytes match {
      case Success(savedRates: Rates) => savedRates
      case _ => Rates(Map.empty, defFee div 2, defFee, 0L)
    }

  private val updateRates = (fee: Coin, exchange: PriceMap) => {
    rates = Rates(exchange, fee div 2, fee, System.currentTimeMillis)
    save(Pickle.intoBytes(rates).array)
  }

  def process = withDelay(pickFeeRate.map(List(_, defFee).sorted.last).zip(pickExchangeRate)
    .repeatWhen(_ delay period), rates.stamp, period.toMillis) foreach updateRates.tupled
}