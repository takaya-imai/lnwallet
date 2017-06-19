package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.Exceptions._
import java.text.{DecimalFormat, DecimalFormatSymbols}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi}
import com.lightning.wallet.ln.crypto.RandomGenerator
import language.implicitConversions
import org.bitcoinj.core.Coin
import scodec.bits.BitVector
import java.util.Locale


object ~ {
  // Matching Tuple2 via arrows with much less noise
  def unapply[A, B](tuple: (A, B) /* got a tuple */) = Some(tuple)
}

object Tools {
  type Bytes = Array[Byte]
  type LightningMessages = Vector[LightningMessage]
  val random = new RandomGenerator

  def runAnd[T](result: T)(action: Any): T = result
  def log(message: String): Unit = android.util.Log.d("LN", message)
  def wrap(run: => Unit)(go: => Unit) = try go catch none finally run
  def none: PartialFunction[Any, Unit] = { case _ => }

  def fromShortId(id: Long): (Int, Int, Int) = {
    val blockNumber = id.>>(40).&(0xFFFFFF).toInt
    val txOrd = id.>>(16).&(0xFFFFFF).toInt
    val outOrd = id.&(0xFFFF).toInt
    (blockNumber, txOrd, outOrd)
  }

  def toShortId(blockHeight: Int, txIndex: Int, outputIndex: Int): Long =
    blockHeight.&(0xFFFFFFL).<<(40) | txIndex.&(0xFFFFFFL).<<(16) | outputIndex.&(0xFFFFL)

  def toLongId(hash: BinaryData, fundingOutputIndex: Int): BinaryData = {
    if (fundingOutputIndex >= 65536) throw ChannelException(LONG_ID_INDEX_TOO_BIG)
    if (hash.size != 32) throw ChannelException(why = LONG_ID_HASH_WRONG_SIZE)
    hash.take(30) :+ hash.data(30).^(fundingOutputIndex >> 8).toByte :+
      hash.data(31).^(fundingOutputIndex).toByte
  }
}

object MSat {
  val satFactor = 1000L
  val btcFactor = 100000000000L
  val locale = new Locale("en", "US")
  val baseFiat = new DecimalFormat("#,###,###.##")
  val baseSat = new DecimalFormat("###,###,###.###")
  val symbols = new DecimalFormatSymbols(locale)
  baseFiat setDecimalFormatSymbols symbols
  baseSat setDecimalFormatSymbols symbols

  def withSign(sum: String): String = s"ⓢ\u00A0$sum"
  def btcBigDecimal2MilliSatoshi(btc: BigDecimal): MilliSatoshi = MilliSatoshi(amount = (btc * btcFactor).toLong)
  def satString2MilliSatoshi(sat: String): MilliSatoshi = MilliSatoshi(amount = (BigDecimal(sat) * satFactor).toLong)
  implicit def milliSatoshi2String(msat: MilliSatoshi): String = baseSat format BigDecimal(msat.amount) / satFactor
  implicit def satoshi2String(msat: Satoshi): String = baseSat format msat.amount
  implicit def coin2String(coin: Coin): String = baseSat format coin.value

  implicit def milliSatoshi2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / satFactor)
  implicit def coin2MilliSatoshi(coin: Coin): MilliSatoshi = MilliSatoshi(coin.value * satFactor)
}

object Features {
  val CHANNELS_PUBLIC_BIT = 0
  val INITIAL_ROUTING_SYNC_BIT = 2

  def isSet(features: BinaryData, bitIndex: Int): Boolean = {
    val unreversedBitSet: BitVector = BitVector(features.data).reverse
    bitIndex < unreversedBitSet.size && unreversedBitSet.get(bitIndex)
  }

  def areSupported(features: BinaryData): Boolean =
    !BitVector(features.data).toIndexedSeq.reverse.zipWithIndex.exists {
      case bit ~ idx => bit && idx % 2 == 0 && idx > INITIAL_ROUTING_SYNC_BIT
    }
}

// STATE MACHINE

trait StateMachineListener {
  type Transition = (Any, Any, String, String)
  def onError: PartialFunction[Throwable, Unit] = none
  def onPostProcess: PartialFunction[Any, Unit] = none
  def onBecome: PartialFunction[Transition, Unit] = none
}

abstract class StateMachine[T] { grounding =>
  var listeners = Set.empty[StateMachineListener]
  var state: String = _
  var data: T = _

  protected[this] val events = new StateMachineListener {
    override def onError = { case error => for (lst <- listeners if lst.onError isDefinedAt error) lst onError error }
    override def onBecome = { case trans => for (lst <- listeners if lst.onBecome isDefinedAt trans) lst onBecome trans }
    override def onPostProcess = { case x => for (lst <- listeners if lst.onPostProcess isDefinedAt x) lst onPostProcess x }
  }

  def stayWith(d1: T) = become(d1, state)
  def become(data1: T, state1: String) = {
    // Should be defined before the vars are updated
    val transition = Tuple4(data, data1, state, state1)
    wrap { data = data1 } { state = state1 }
    events onBecome transition
  }

  def doProcess(change: Any)
  def process(anyChange: Any) =
    try grounding synchronized {
      doProcess(change = anyChange)
      events onPostProcess anyChange
    } catch events.onError
}