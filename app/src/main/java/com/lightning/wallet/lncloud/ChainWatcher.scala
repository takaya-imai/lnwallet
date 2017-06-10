package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.lncloud.ImplicitConversions._

import org.bitcoinj.core.{Coin, StoredBlock, Transaction}
import fr.acinq.bitcoin.{BinaryData, OutPoint}

import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.Tools.wrap
import com.lightning.wallet.Utils.app
import scala.util.Try


class ReplaceRunnableHolder {
  private[this] var container = Option.empty[Runnable]
  def release: Unit = for (runnable <- container) runnable.run
  def hold(run: Runnable) = wrap { container = Some apply run } { release }
}

object ChainWatcher {
  def watchTxDepthLocal(react: CMDDepth => Unit, watchTxId: String) = {
    val listener = new org.bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(wallet: org.bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxId) react(CMDDepth apply tx.getConfidence.getDepthInBlocks)
    }

    app.kit.wallet addTransactionConfidenceEventListener listener
    anyToRunnable(app.kit.wallet removeTransactionConfidenceEventListener listener)
  }

  def watchChainHeightLocal(react: Int => Unit) = {
    val listener = new org.bitcoinj.core.listeners.NewBestBlockListener {
      def notifyNewBestBlock(bestBlock: StoredBlock) = react(bestBlock.getHeight)
    }

    app.kit.blockChain addNewBestBlockListener listener
    anyToRunnable(app.kit.blockChain removeNewBestBlockListener listener)
  }

  def watchInputUsedLocal(react: CMDFundingSpent => Unit, fundPoint: OutPoint) = {
    val listener = new org.bitcoinj.wallet.listeners.WalletCoinsSentEventListener { me =>
      def onCoinsSent(w: org.bitcoinj.wallet.Wallet, tx: Transaction, prev: Coin, now: Coin) =
        if (tx.txIn.map(_.outPoint) contains fundPoint) react(CMDFundingSpent apply tx)
    }

    app.kit.wallet addCoinsSentEventListener listener
    anyToRunnable(app.kit.wallet removeCoinsSentEventListener listener)
  }

  def registerInputUsedLocal(chan: Channel) = new ReplaceRunnableHolder match { case holder =>
    val commitmentsOpt = Option(chan.data) collect { case data: ChannelData with HasCommitments => data.commitments }
    for (data <- commitmentsOpt) holder hold watchInputUsedLocal(chan.process, data.commitInput.outPoint)
    holder
  }
}


object StorageWrap {
  def put(value: String, key: String) = LNParams.db txWrap {
    LNParams.db.change(sql = StorageTable.newSql, params = value, key)
    LNParams.db.change(sql = StorageTable.updSql, params = value, key)
  }

  def get(key: String): Try[String] = {
    val cursor = LNParams.db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }
}


object PaymentSpecWrap extends PaymentSpecBag { me =>
  import com.lightning.wallet.lncloud.PaymentSpecTable._
  private def byHash(hash: BinaryData) = LNParams.db.select(PaymentSpecTable.selectByHashSql, hash.toString)
  private def byTime = LNParams.db.select(PaymentSpecTable.selectRecentSql, (System.currentTimeMillis - 14.days.toMillis).toString)
  private def toInfo(rcu: RichCursor) = ExtendedPaymentInfo(to[PaymentSpec](rcu string data), rcu string status, rcu long stamp)
  def getInfoByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = RichCursor(me byHash hash) headTry toInfo
  def getRecentInfos: Vector[ExtendedPaymentInfo] = RichCursor(byTime) vec toInfo

  def updatePaymentStatus(hash: BinaryData, status: String): Unit = {
    LNParams.db.change(PaymentSpecTable.updStatusSql, status, hash.toString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def updateOutgoingPaymentSpec(spec: OutgoingPaymentSpec): Unit =
    LNParams.db.change(PaymentSpecTable.updDataSql, spec.toJson.toString,
      spec.invoice.paymentHash.toString)

  def putInfo(info: ExtendedPaymentInfo) = info.spec.invoice.paymentHash.toString match { case hashStr =>
    LNParams.db.change(PaymentSpecTable.newSql, info.spec.toJson.toString, hashStr, info.status, info.stamp.toString)
    LNParams.db.change(PaymentSpecTable.newVirtualSql, s"${info.spec.invoice.message.orNull} $hashStr", hashStr)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }
}