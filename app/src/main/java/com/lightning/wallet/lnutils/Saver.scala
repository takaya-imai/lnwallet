package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import rx.lang.scala.{Scheduler, Observable => Obs}
import org.bitcoinj.core.{Coin, Transaction}

import com.lightning.wallet.lnutils.RatesSaver.Fiat2Btc
import com.lightning.wallet.helper.Statistics
import spray.json.JsonFormat
import scala.util.Try


object JsonHttpUtils {
  def initDelay[T](next: Obs[T], startMillis: Long, timeoutMillis: Long) = {
    val adjustedTimeout = startMillis + timeoutMillis - System.currentTimeMillis
    val delayLeft = if (adjustedTimeout < 0) 0 else adjustedTimeout
    Obs.just(null).delay(delayLeft.millis).flatMap(_ => next)
  }

  def obsOn[T](provider: => T, scheduler: Scheduler): Obs[T] =
    Obs.just(null).subscribeOn(scheduler).map(_ => provider)

  def retry[T](obs: Obs[T], pick: (Throwable, Int) => Duration, times: Range): Obs[T] =
    obs.retryWhen(_.zipWith(Obs from times)(pick) flatMap Obs.timer)

  type JsValueVec = Vector[JsValue]
  def toVec[T : JsonFormat](vec: JsValueVec) = for (js <- vec) yield js.convertTo[T]
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
  val KEY = "cloudData"
}

object RatesSaver extends Saver {
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)
  val KEY = "rates"

  private val updatePeriod: FiniteDuration = 20.minutes
  var rates = tryGet map to[Rates] getOrElse Rates(Nil, Map.empty, 0)

  def update = {
    val unsafe = LNParams.cloud.connector.getRates map toVec[Result]
    val periodic = retry(unsafe, pickInc, 2 to 4 by 2).repeatWhen(_ delay updatePeriod)
    initDelay(periodic, rates.stamp, updatePeriod.toMillis) foreach { case newFee \ newFiat +: _ =>
      val validFees = for (validFee <- newFee("4") +: rates.feeHistory if validFee > 0) yield validFee
      rates = Rates(validFees take 4, newFiat, System.currentTimeMillis)
      save(rates.toJson)
    }
  }
}

case class Rates(feeHistory: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  lazy val feeLive = if (feeHistory.isEmpty) Transaction.DEFAULT_TX_FEE else {
    val statistics = new Statistics[Double] { def extract(item: Double) = item }
    val withoutOutliers = statistics.filterWithin(feeHistory, stdDevs = 2)
    btcBigDecimal2MSat(statistics mean withoutOutliers): Coin
  }
}