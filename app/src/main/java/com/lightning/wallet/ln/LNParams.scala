package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import com.lightning.wallet.Utils.app


object LNParams {
  lazy val broadcaster: Broadcaster = LocalBroadcaster
  lazy val lnCloud = new LNCloud("http://10.0.2.2:9002")
  var extendedNodeKey: ExtendedPrivateKey = _
  var cloudPrivateKey: PrivateKey = _
  var db: CipherOpenHelper = _

  def isSetUp: Boolean = db != null
  def setup(seed: BinaryData): Unit = generate(seed) match { case master =>
    cloudPrivateKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil).privateKey
    extendedNodeKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    db = new CipherOpenHelper(app, 1, Crypto.hash256(seed).toString)
  }

  val updateFeeMinDiffRatio = 0.25 // Must update
  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(4294967295L)
  val maxReserveToFundingRatio = 0.05 // %
  val reserveToFundingRatio = 0.01 // %
  val untilExpiryBlocks = 6
  val minDepth = 2

  def myHtlcExpiry = broadcaster.currentHeight + untilExpiryBlocks
  def exceedsReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Boolean =
    channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }

  def deriveParamsPrivateKey(index: Long, n: Long): PrivateKey =
    derivePrivateKey(extendedNodeKey, index :: n :: Nil).privateKey

  def makeLocalParams(fundingSat: Long, finalScriptPubKey: BinaryData, keyIndex: Long) = {
    val Seq(funding, revocation, payment, delayed, sha) = for (n <- 0 to 4) yield deriveParamsPrivateKey(keyIndex, n)
    LocalParams(Block.TestnetGenesisBlock.blockId, dustLimitSatoshis = 542, maxHtlcValueInFlightMsat = Long.MaxValue,
      channelReserveSatoshis = (reserveToFundingRatio * fundingSat).toLong, htlcMinimumMsat = 500, toSelfDelay = 144,
      maxAcceptedHtlcs = 10, fundingPrivKey = funding, revocationSecret = revocation, paymentKey = payment,
      delayedPaymentKey = delayed, finalScriptPubKey, shaSeed = sha256(sha.toBin),
      isFunder = true, globalFeatures = "", localFeatures = "03")
  }
}

trait Broadcaster {
  def broadcast(tx: Transaction)
  def currentFeeRate: Long
  def currentHeight: Int

  def broadcastStatus(txs: Seq[Transaction], parents: Map[String, Int], chainHeight: Int): Seq[BroadcastStatus] = {
    val augmented = for (tx <- txs) yield (tx, parents get tx.txIn.head.outPoint.txid.toString, Scripts csvTimeout tx)

    augmented map {
      // If CSV is zero then whether parent tx is present or not is irrelevant, we look as CLTV
      case (tx, _, 0L) if tx.lockTime - chainHeight < 1 => BroadcastStatus(None, publishable = true, tx)
      case (tx, _, 0L) => BroadcastStatus(Some(tx.lockTime - chainHeight), publishable = false, tx)
      // If CSV is not zero but parent tx is not published then we wait for parent
      case (tx, None, _) => BroadcastStatus(None, publishable = false, tx)

      case (tx, Some(parentConfs), csv) =>
        // Tx may have both CLTV and CSV so we need to get the max of them
        val blocksLeft = math.max(csv - parentConfs, tx.lockTime - chainHeight)
        if (blocksLeft < 1) BroadcastStatus(None, publishable = true, tx)
        else BroadcastStatus(Some(blocksLeft), publishable = false, tx)
    }
  }
}