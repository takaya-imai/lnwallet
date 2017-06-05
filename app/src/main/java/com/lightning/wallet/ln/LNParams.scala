package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.DeterministicWallet._
import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import com.lightning.wallet.ln.crypto.Digests
import com.lightning.wallet.ln.MSat.satFactor
import com.lightning.wallet.Utils.app


object LNParams {
  val updateFeeMinDiffRatio = 0.25 // Must update
  val maxReserveToFundingRatio = 0.05 // %
  val reserveToFundingRatio = 0.01 // %
  val untilExpiryBlocks = 6
  val minDepth = 2

  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val maxHtlcValue = MilliSatoshi(100000000L)

  // Public, no initial sync
  val localFeatures = "03"
  val globalFeatures = ""

  lazy val bag: PaymentSpecBag = PaymentSpecWrap
  lazy val broadcaster: Broadcaster = LocalBroadcaster

  var nodePrivateKey: PrivateKey = _
  var cloudPrivateKey: PrivateKey = _
  var extendedNodeKey: ExtendedPrivateKey = _
  var db: CipherOpenHelper = _
  var cloud: LNCloud = _

  def isSetUp: Boolean = db != null
  def setup(seed: BinaryData): Unit = generate(seed) match { case master =>
    cloudPrivateKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil).privateKey
    extendedNodeKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    db = new CipherOpenHelper(app, 1, Crypto.hash256(seed).toString)
    nodePrivateKey = extendedNodeKey.privateKey
    cloud = currentLNCloud
  }

  // LNCLOUD AND PATHFINDER

  def currentLNCloud = PrivateDataSaver.tryGetObject map {
    data => new FailoverLNCloud(new LNCloud("http://10.0.2.2"), data.url)
  } getOrElse new LNCloud("http://10.0.2.2")

  def currentPathfinder(channel: Channel): Pathfinder = PrivateDataSaver.tryGetObject map {
    privateData => new PrivatePathfinder(new FailoverLNCloud(cloud, privateData.url), channel) { data = privateData }
  } getOrElse new PublicPathfinder(bag, cloud, channel) { data = PublicDataSaver.tryGetObject getOrElse PublicDataSaver.empty }

  // FEE RELATED

  def exceedsReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Boolean =
    channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio

  def feerateKB2Kw(feeratePerKB: Long): Long = feeratePerKB / 2
  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }

  // MISC

  def myHtlcExpiry: Int = broadcaster.currentHeight + untilExpiryBlocks
  def derivePreimage(ord: Long): BinaryData = Digests.hmacSha256(nodePrivateKey.toBin, s"Preimage $ord" getBytes "UTF-8")
  def deriveParamsPrivateKey(index: Long, n: Long): PrivateKey = derivePrivateKey(extendedNodeKey, index :: n :: Nil).privateKey

  def makeLocalParams(channelReserveSat: Long, finalScriptPubKey: BinaryData, keyIndex: Long) = {
    val Seq(funding, revocation, payment, delayed, sha) = for (n <- 0 to 4) yield deriveParamsPrivateKey(keyIndex, n)
    LocalParams(chainHash = Block.RegtestGenesisBlock.blockId, dustLimitSatoshis = maxHtlcValue.amount / satFactor,
      maxHtlcValueInFlightMsat = Long.MaxValue, channelReserveSat, htlcMinimumMsat = 500, toSelfDelay = 144,
      maxAcceptedHtlcs = 20, fundingPrivKey = funding, revocationSecret = revocation, paymentKey = payment,
      delayedPaymentKey = delayed, finalScriptPubKey, shaSeed = sha256(sha.toBin), isFunder = true)
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