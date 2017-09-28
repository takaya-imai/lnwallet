package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._

import org.bitcoinj.core.{Coin, Transaction}
import rx.lang.scala.{Scheduler, Observable => Obs}
import com.lightning.wallet.lncloud.RatesSaver.RatesMap
import com.lightning.wallet.helper.Statistics
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  def obsOn[T](provider: => T, scheduler: Scheduler): Obs[T] =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range): Obs[T] =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  def withDelay[T](obs: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0) 0 else adjustedTimeout
    obs.delay(delayLeft.millis)
  }

  type JsValueVec = Vector[JsValue]
  def toVec[T : JsonFormat](raw: JsValueVec): Vector[T] =
    for (jsValue <- raw) yield jsValue.convertTo[T]

  def to[T : JsonFormat](raw: String): T = raw.parseJson.convertTo[T]
  def pickInc(err: Throwable, next: Int) = next.seconds
}

trait Saver {
  val KEY: String
  def tryGet: Try[String] = StorageWrap.get(KEY)
  def save(snap: JsValue) = StorageWrap.put(snap.toString, KEY)
}

object CloudDataSaver extends Saver {
  def empty = CloudData(None, Set.empty, Set.empty, url = "")
  def tryGetObject: TryCloudData = tryGet map to[CloudData]
  def saveObject(data: CloudData) = save(data.toJson)
  type TryCloudData = Try[CloudData]
  val KEY = "cloudData38"
}

object RatesSaver extends Saver {
  type RatesMap = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, RatesMap)
  val KEY = "rates2"

  private val updatePeriod: FiniteDuration = 20.minutes
  var rates = tryGet map to[Rates] getOrElse Rates(Nil, Map.empty, 0)

  def process = {
    def result = LNParams.cloud.connector.getRates map toVec[Result]
    def periodic = retry(result, pickInc, 2 to 6 by 2).repeatWhen(_ delay updatePeriod)
    withDelay(periodic, rates.stamp, updatePeriod.toMillis) foreach { case newFee \ newFiat +: _ =>
      val validFees = for (validFee <- newFee("6") +: rates.feeHistory if validFee > 0) yield validFee
      rates = Rates(validFees take 6, newFiat, System.currentTimeMillis)
      save(rates.toJson)
    }
  }
}

case class Rates(feeHistory: Seq[Double], exchange: RatesMap, stamp: Long) {
  lazy val statistics = new Statistics[Double] { def extract(item: Double) = item }
  lazy val feeLive = if (feeHistory.isEmpty) Transaction.DEFAULT_TX_FEE else cutOutliers
  lazy val feeRisky = feeLive div 2

  private[this] lazy val cutOutliers = Coin parseCoin {
    val filtered = statistics.filterWithin(feeHistory, stdDevs = 1)
    val formatter = new java.text.DecimalFormat("##.00000000")
    formatter.format(statistics mean filtered)
  }
}