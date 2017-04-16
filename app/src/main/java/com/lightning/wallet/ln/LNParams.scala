package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{sha256, PrivateKey}


object LNParams {
  var seedHash: String = _
  var extendedPrivateKey: ExtendedPrivateKey = _
  var extendedCloudPrivateKey: ExtendedPrivateKey = _
  var broadcaster: Broadcaster = _

  def hasSeed: Boolean = seedHash != null
  def setup(seed: BinaryData): Unit = generate(seed) match { case master =>
    extendedPrivateKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    extendedCloudPrivateKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil)
    seedHash = Crypto.hash256(seed).toString
  }

  val updateFeeMinDiffRatio = 0.25 // Must update
  val chainHash = Block.TestnetGenesisBlock.blockId
  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(4294967295L)

  // mandatory public channel, sync off
  val localFeatures = BinaryData("01")
  val globalFeatures = BinaryData("")

  val maxReserveToFundingRatio = 0.05 // %
  val reserveToFundingRatio = 0.01 // %
  val htlcMinimumMsat = 1000
  val maxAcceptedHtlcs = 10
  val smartfeeNBlocks = 3
  val minDepth = 2

  val feeProportionalMillionth = 10
  val dustLimitSatoshis = 542
  val feeBaseMsat = 546000

  val expiryDeltaBlocks = 144
  val toSelfDelay = 144

  def deriveParamsPrivateKey(index: Long, n: Long): PrivateKey =
    derivePrivateKey(extendedPrivateKey, index :: n :: Nil).privateKey

  def exceedsReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Boolean =
    channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }

  def makeLocalParams(fundingSat: Long, finalScriptPubKey: BinaryData, keyIndex: Long) =
    LocalParams(chainHash = chainHash, dustLimitSatoshis = dustLimitSatoshis, maxHtlcValueInFlightMsat = Long.MaxValue,
      channelReserveSatoshis = (reserveToFundingRatio * fundingSat).toLong, htlcMinimumMsat, toSelfDelay, maxAcceptedHtlcs,
      fundingPrivKey = deriveParamsPrivateKey(keyIndex, 0L), revocationSecret = deriveParamsPrivateKey(keyIndex, 1L),
      paymentKey = deriveParamsPrivateKey(keyIndex, 2L), delayedPaymentKey = deriveParamsPrivateKey(keyIndex, 3L),
      defaultFinalScriptPubKey = finalScriptPubKey, shaSeed = sha256(deriveParamsPrivateKey(keyIndex, 4L).toBin),
      isFunder = true, globalFeatures, localFeatures)
}

case class ScheduledTx(hex: BinaryData, atBlock: Int)

trait Broadcaster {
  def schedule(what: ScheduledTx): Unit
  def unschedule(what: ScheduledTx): Unit
  def currentFeeRate: Long
  def currentHeight: Int
}