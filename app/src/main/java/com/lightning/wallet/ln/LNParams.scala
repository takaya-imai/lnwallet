package com.lightning.wallet.ln

import com.lightning.wallet.ln.Exceptions._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import org.bitcoinj.wallet.DeterministicSeed
import org.bitcoinj.crypto.DeterministicKey
import org.bitcoinj.core.Sha256Hash


object LNParams {
  private var seed: DeterministicSeed = _
  lazy val extendedPrivateKey: DeterministicKey = Tools.derive(Nil, 46)(seed)
  lazy val extendedCloudPrivateKey: DeterministicKey = Tools.derive(Nil, 92)(seed)
  def getSeedHash: String = Sha256Hash.twiceOf(seed.getSeedBytes).toString
  def setSeed(newSeed: DeterministicSeed): Unit = seed = newSeed
  def hasSeed: Boolean = seed != null

  // Should be constantly updated
  var broadcaster: Broadcaster = _
  var feeRatePerKw = 1000L

  val updateFeeMinDiffRatio = 0.1 // Should update fee
  val updateFeeMaxDiffRatio = 0.3 // Should disconnect

  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(4294967295L)

  // mandatory public channel, sync off
  val localFeatures = BinaryData("01")
  val globalFeatures = BinaryData("")

  val maxHtlcValueInFlightMsat = Long.MaxValue
  val maxReserveToFundingRatio = 0.05 // %
  val reserveToFundingRatio = 0.01 // %
  val htlcMinimumMsat = 100000
  val maxAcceptedHtlcs = 10
  val smartfeeNBlocks = 3
  val minDepth = 2

  val feeProportionalMillionth = 10
  val dustLimitSatoshis = 542
  val feeBaseMsat = 546000

  val expiryDeltaBlocks = 144
  val delayBlocks = 144

  def validateReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Unit = {
    val nope = channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio
    if (nope) throw new RuntimeException(CHANNEL_RESERVE_TOO_HIGH)
  }

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }
}

case class ScheduledTx(tx: BinaryData, atBlock: Int)

trait Broadcaster {
  def schedule(what: ScheduledTx): Unit
  def unschedule(what: ScheduledTx): Unit
  def broadcast(what: ScheduledTx): Unit
  def currentBlockCount: Int
}