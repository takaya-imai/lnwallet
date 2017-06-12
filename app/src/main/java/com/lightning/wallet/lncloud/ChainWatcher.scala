package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.lncloud.ImplicitConversions._
import org.bitcoinj.core.{Coin, StoredBlock, Transaction}
import org.bitcoinj.core.listeners.NewBestBlockListener
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.TxTracker
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
import scala.util.Try


object ChainWatcher {
  def watchBlockchainLocal(chan: Channel) = new TxTracker with NewBestBlockListener { me =>
    override def coinsSent(tx: Transaction, pb: Coin, nb: Coin) = chan process CMDSomethingSpent(tx)
    def notifyNewBestBlock(bestBlock: StoredBlock) = chan process CMDDepth(bestBlock.getHeight)
    override def txConfirmed(tx: Transaction) = chan process CMDSomethingConfirmed(tx)
    app.kit.wallet addTransactionConfidenceEventListener me
    app.kit.blockChain addNewBestBlockListener me
    app.kit.wallet addCoinsSentEventListener me
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