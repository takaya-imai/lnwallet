package com.lightning.wallet.ln

import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.Exceptions._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import java.text.{DecimalFormat, DecimalFormatSymbols}
import com.lightning.wallet.ln.crypto.RandomGenerator
import language.implicitConversions
import org.bitcoinj.core.Coin
import wire.LightningMessage
import java.util.Locale


object Tools {
  type Bytes = Array[Byte]
  type BinaryDataList = List[BinaryData]
  type LightningMessages = Vector[LightningMessage]
  val random = new RandomGenerator

  def none: PartialFunction[Any, Unit] = { case _ => }
  def runAnd[T](result: T)(action: Any): T = result
  def wrap(run: => Unit)(go: => Unit): Unit =
    try go catch none finally run

  def fromShortId(id: Long): (Int, Int, Int) = {
    val blockNumber = id.>>(40).&(0xFFFFFF).toInt
    val txOrd = id.>>(16).&(0xFFFFFF).toInt
    val outOrd = id.&(0xFFFF).toInt
    (blockNumber, txOrd, outOrd)
  }

  def toShortId(blockHeight: Int, txIndex: Int, outputIndex: Int): Long =
    blockHeight.&(0xFFFFFFL).<<(40) | txIndex.&(0xFFFFFFL).<<(16) | outputIndex.&(0xFFFFL)

  def toLongId(txHash: BinaryData, fundingOutputIndex: Int): BinaryData = {
    if (fundingOutputIndex >= 65536) throw ChannelException(LONG_ID_INDEX_TOO_BIG)
    if (txHash.size != 32) throw ChannelException(LONG_ID_HASH_WRONG_SIZE)

    val longChannelId = txHash.take(30) :+
      txHash.data(30).^(fundingOutputIndex >> 8).toByte :+
      txHash.data(31).^(fundingOutputIndex).toByte

    longChannelId
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

  def btcBigDecimal2MilliSatoshi(btc: BigDecimal): MilliSatoshi = MilliSatoshi(amount = (btc * btcFactor).toLong)
  def satString2MilliSatoshi(sat: String): MilliSatoshi = MilliSatoshi(amount = (BigDecimal(sat) * satFactor).toLong)
  implicit def milliSatoshi2String(msat: MilliSatoshi): String = baseSat.format(BigDecimal(msat.amount) / satFactor)
  implicit def milliSatoshi2Coin(msat: MilliSatoshi): Coin = Coin.valueOf(msat.amount / satFactor)
  implicit def coin2MilliSatoshi(coin: Coin): MilliSatoshi = MilliSatoshi(coin.value * satFactor)
  implicit def coin2String(coin: Coin): String = baseSat format coin.value
  def withSign(sum: String) = s"ⓢ $sum"
}

object Features {
  val CHANNELS_PUBLIC_BIT = 0
  val INITIAL_ROUTING_SYNC_BIT = 2

  private def fromArray(features: BinaryData) =
    java.util.BitSet.valueOf(features.data.reverse.toArray)

  def isSet(features: BinaryData, bitIndex: Int): Boolean = fromArray(features) get bitIndex
  def areSupported(features: BinaryData): Boolean = fromArray(features) match { case bitset =>
    val (accumulator: List[Int], range: Range) = (bitset.nextSetBit(0) :: Nil, 1 until bitset.cardinality)
    val bits = (accumulator /: range) { case (accum, _) => bitset.nextSetBit(accum.head + 1) :: accum }
    !bits.reverse.exists(value => value % 2 == 0 && value > INITIAL_ROUTING_SYNC_BIT)
  }
}

// STATE MACHINE

trait StateMachineListener {
  def onError: PartialFunction[Throwable, Unit]
  def onBecome: Unit
}

class StateMachineListenerProxy extends StateMachineListener {
  private[this] var listeners = Set.empty[StateMachineListener]
  def removeListeners = listeners = Set.empty[StateMachineListener]
  def addListener(listener: StateMachineListener) = listeners += listener
  def onError = { case err => for (lst <- listeners) lst onError err }
  def onBecome: Unit = for (lst <- listeners) lst.onBecome
}

abstract class StateMachine[T](var state: List[String], var data: T) { me =>
  def process(a: Any) = try me synchronized doProcess(a) catch events.onError
  def stayWith(data1: T): Unit = become(data1, state.head)
  def doProcess(change: Any): Unit

  val events = new StateMachineListenerProxy
  def become(data1: T, state1: String): Unit = {
    wrap { data = data1 } { state = state1 :: state take 2 }
    events.onBecome
  }
}