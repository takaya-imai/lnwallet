package com.lightning.wallet.ln

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
  var currentBlockCount: Int = 120000
  var feeRatePerKw: Long = 1000

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
}