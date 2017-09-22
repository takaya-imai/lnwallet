package com.lightning.wallet.ln

import fr.acinq.bitcoin._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.DeterministicWallet._
import com.lightning.wallet.lncloud.JsonHttpUtils._

import com.lightning.wallet.ln.Scripts.{TransactionWithInputInfo, csvTimeout}
import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import scala.util.{Failure, Success}

import com.lightning.wallet.lncloud.CloudDataSaver.TryCloudData
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.Utils.app
import fr.acinq.eclair.UInt64


object LNParams { me =>
  val maxReserveToFundingRatio = 0.05 // %
  val updateFeeMinDiffRatio = 0.25 // %
  val reserveToFundingRatio = 0.01 // %
  val localFeatures = "00"
  val globalFeatures = ""
  val minDepth = 2

  val htlcMinimumMsat = 100000L
  val maxHtlcValue = MilliSatoshi(4194304000L)
  val maxChannelCapacity = MilliSatoshi(16777216000L)
  val dustLimit = Satoshi(MIN_NONDUST_OUTPUT.value)
  val chainHash = Block.RegtestGenesisBlock.hash
  lazy val broadcaster = LocalBroadcaster
  lazy val bag = PaymentInfoWrap

  var cloud: StateMachine[CloudData] with Cloud = _
  var extendedNodeKey: ExtendedPrivateKey = _
  var cloudPrivateKey: PrivateKey = _
  var nodePrivateKey: PrivateKey = _
  var db: CipherOpenHelper = _

  def isSetUp: Boolean = db != null
  def setup(seed: BinaryData) = generate(seed) match { case master =>
    val cloudExtendedKey = derivePrivateKey(master, hardened(92) :: hardened(0) :: Nil)
    extendedNodeKey = derivePrivateKey(master, hardened(46) :: hardened(0) :: Nil)
    db = new CipherOpenHelper(app, 1, Crypto.hash256(seed).toString)
    cloud = me getCloud CloudDataSaver.tryGetObject
    cloudPrivateKey = cloudExtendedKey.privateKey
    nodePrivateKey = extendedNodeKey.privateKey
  }

  // CLOUD

  private val con = new Connector("10.0.2.2")
  def getCloud(tryData: TryCloudData) = tryData match {
    case Failure(why) => new PublicCloud(con, bag) { data = CloudDataSaver.empty }
    case Success(saved) if saved.url.isEmpty => new PublicCloud(con, bag) { data = saved }

    case Success(saved) =>
      val failover = new FailoverConnector(con, saved.url)
      new PrivateCloud(failover) { data = saved }
  }

  // FEE RELATED

  def exceedsReserve(channelReserveSatoshis: Long, fundingSatoshis: Long): Boolean =
    channelReserveSatoshis.toDouble / fundingSatoshis > maxReserveToFundingRatio

  def shouldUpdateFee(commitmentFeeratePerKw: Long, networkFeeratePerKw: Long): Boolean = {
    val feeRatio = (networkFeeratePerKw - commitmentFeeratePerKw) / commitmentFeeratePerKw.toDouble
    networkFeeratePerKw > 0 && Math.abs(feeRatio) > updateFeeMinDiffRatio
  }

  // MISC

  def expiry: Int = broadcaster.currentHeight + 6
  def makeLocalParams(reserve: Long, finalScriptPubKey: BinaryData, idx: Long) = {
    val Seq(fund, revoke, pay, delay, sha) = for (n <- 0L to 4L) yield derivePrivateKey(extendedNodeKey, idx :: n :: Nil)
    LocalParams(maxHtlcValueInFlightMsat = UInt64(Long.MaxValue), reserve, toSelfDelay = 144, maxAcceptedHtlcs = 20,
      fundingPrivKey = fund.privateKey, revocationSecret = revoke.privateKey, paymentKey = pay.privateKey,
      delayedPaymentKey = delay.privateKey, finalScriptPubKey, shaSeed = sha256(sha.privateKey.toBin),
      isFunder = true)
  }
}

object AddErrorCodes {
  import com.lightning.wallet.R.string._
  val ERR_AMOUNT_OVERFLOW = err_ln_amount_overflow
  val ERR_REMOTE_FEE_OVERFLOW = err_ln_remote_fee_overflow
  val ERR_REMOTE_AMOUNT_LOW = err_ln_remote_amount_low
  val ERR_TOO_MANY_HTLC = err_ln_too_many
  val ERR_FULFILLED = err_ln_fulfilled
  val ERR_IN_FLIGHT = err_ln_inflight
  val ERR_OFFLINE = err_ln_offline
  val ERR_FAILED = err_ln_general
}

trait Broadcaster extends ChannelListener { me =>
  def getConfs(txid: BinaryData): Option[Int]
  def send(tx: Transaction): String
  def feeRatePerKw: Long
  def currentHeight: Int

  def safeSend(tx: Transaction) = obsOn(me send tx, IOScheduler.apply).onErrorReturn(_.getMessage)
  def cltv(info: TransactionWithInputInfo) = math.max(info.tx.lockTime - currentHeight, 0L)
  def cltvAndCsv(info1: TransactionWithInputInfo, info2: TransactionWithInputInfo) =
    for (left <- me csv info2) yield cltv(info1) + left

  def csv(info: TransactionWithInputInfo) =
    getConfs(info.tx.txIn.head.outPoint.txid)
      .map(parent => csvTimeout(info.tx) - parent)
      .map(delay => if (delay < 0L) 0L else delay)
}