package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import scala.concurrent.duration._
import org.bitcoinj.wallet.listeners._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.lncloud.ImplicitConversions._

import rx.lang.scala.{Subscription, Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, OutPoint}
import org.bitcoinj.core.{Coin, Transaction}

import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import org.bitcoinj.wallet.Wallet
import scala.util.Try


object ChainWatcher {
  def watchTxDepthLocal(watchTxId: String) = Obs[CMDDepth] { obs =>
    val listener = new org.bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(wallet: org.bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxId) obs onNext CMDDepth(tx.getConfidence.getDepthInBlocks)
    }

    app.kit.wallet addTransactionConfidenceEventListener listener
    Subscription(app.kit.wallet removeTransactionConfidenceEventListener listener)
  }

  def watchInputUsedLocal(fundPoint: OutPoint) = Obs[CMDFundingSpent] { obs =>
    lazy val lst = new WalletCoinsReceivedEventListener with WalletCoinsSentEventListener {
      def onCoinsSent(w: Wallet, tx: Transaction, prev: Coin, now: Coin) = informIfSpendsInput(tx)
      def onCoinsReceived(w: Wallet, tx: Transaction, prev: Coin, now: Coin) = informIfSpendsInput(tx)
    }

    def informIfSpendsInput(tx: fr.acinq.bitcoin.Transaction) = {
      val spendsInput = tx.txIn.map(_.outPoint).contains(fundPoint)
      if (spendsInput) obs onNext CMDFundingSpent(tx)
    }

    app.kit.wallet addCoinsSentEventListener lst
    app.kit.wallet addCoinsReceivedEventListener lst

    Subscription {
      app.kit.wallet removeCoinsSentEventListener lst
      app.kit.wallet removeCoinsReceivedEventListener lst
    }
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
  private def byTime = LNParams.db.select(PaymentSpecTable.selectRecentSql, (System.currentTimeMillis - 7.days.toMillis).toString)
  private def toInfo(rc: RichCursor) = ExtendedPaymentInfo(to[PaymentSpec](rc string data), rc string status, rc long stamp)
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