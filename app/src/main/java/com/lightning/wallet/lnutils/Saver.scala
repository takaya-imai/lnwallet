package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import rx.lang.scala.{Scheduler, Observable => Obs}
import org.bitcoinj.core.Transaction.DEFAULT_TX_FEE
import org.bitcoinj.core.Coin
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

object CloudDataSaver {
  def empty = CloudData(None, Set.empty, Set.empty, url = "")
  def tryGetObject: TryCloudData = StorageWrap get KEY map to[CloudData]
  def saveObject(data: CloudData) = StorageWrap.put(data.toJson.toString, KEY)
  type TryCloudData = Try[CloudData]
  val KEY = "cloudData"
}

object RatesSaver {
  type Fiat2Btc = Map[String, Double]
  type BlockNum2Fee = Map[String, Double]
  type Result = (BlockNum2Fee, Fiat2Btc)
  val KEY = "feerateAndFiat"

  // Either load saved rates data or create a new object from scratch
  var rates = StorageWrap get KEY map to[Rates] getOrElse Rates(Nil, Map.empty, 0)

  def saveObject = {
    val safe = retry(LNParams.cloud.connector.getRates map toVec[Result], pickInc, 3 to 4)
    initDelay(safe, rates.stamp, timeoutMillis = 1200000) foreach { case newFee \ newFiat +: _ =>
      val validFees = for (positive <- newFee("6") +: rates.feeHistory if positive > 0) yield positive
      rates = Rates(validFees take 3, newFiat, System.currentTimeMillis)
      StorageWrap.put(rates.toJson.toString, KEY)
    }
  }
}

import com.lightning.wallet.lnutils.RatesSaver.Fiat2Btc
case class Rates(feeHistory: Seq[Double], exchange: Fiat2Btc, stamp: Long) {
  // Bitcoin Core provides unreliable fees in testnet so just use default here
  // TODO: remove for mainnet

  lazy val feeLive = if (feeHistory.isEmpty) DEFAULT_TX_FEE else {
    val mu: Coin = btcBigDecimal2MSat(feeHistory.sum / feeHistory.size)
    if (mu isLessThan DEFAULT_TX_FEE) DEFAULT_TX_FEE else mu
  }
}