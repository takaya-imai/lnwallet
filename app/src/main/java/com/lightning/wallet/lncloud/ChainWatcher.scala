package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import collection.JavaConverters._
import org.bitcoinj.wallet.listeners._

import org.bitcoinj.core.{Coin, Transaction}
import rx.lang.scala.{Subscription, Observable => Obs}
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, OutPoint}

import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Crypto.sha256
import org.bitcoinj.wallet.Wallet


object ChainWatcher {
  def watchTxDepthLocal(watchTxId: String) = Obs[Int] { obs =>
    val listener = new org.bitcoinj.core.listeners.TransactionConfidenceEventListener {
      def onTransactionConfidenceChanged(wallet: org.bitcoinj.wallet.Wallet, tx: Transaction) =
        if (tx.getHashAsString == watchTxId) obs onNext tx.getConfidence.getDepthInBlocks
    }

    app.kit.wallet addTransactionConfidenceEventListener listener
    Subscription(app.kit.wallet removeTransactionConfidenceEventListener listener)
  }

  def watchInputUsedLocal(fundPoint: OutPoint) = Obs[Transaction] { obs =>
    val usesInput = (_: Transaction).getInputs.asScala.map(_.getOutpoint) exists { point =>
      point.getIndex == fundPoint.index && point.getHash.toString == fundPoint.hash.toString
    }

    val listener = new WalletCoinsReceivedEventListener with WalletCoinsSentEventListener {
      def onCoinsSent(w: Wallet, tx: Transaction, prev: Coin, now: Coin) = if (usesInput apply tx) obs onNext tx
      def onCoinsReceived(w: Wallet, tx: Transaction, prev: Coin, now: Coin) = if (usesInput apply tx) obs onNext tx
    }

    app.kit.wallet addCoinsSentEventListener listener
    app.kit.wallet addCoinsReceivedEventListener listener

    Subscription {
      app.kit.wallet removeCoinsSentEventListener listener
      app.kit.wallet removeCoinsReceivedEventListener listener
    }
  }
}

object StorageWrap {
  def put(value: String, key: String) = app.db txWrap {
    app.db.change(sql = Storage.newSql, params = value, key)
    app.db.change(sql = Storage.updSql, params = value, key)
  }

  def get(key: String) = {
    val cursor = app.db.select(Storage.selectSql, key)
    RichCursor(cursor).headTry(_ string Storage.value)
  }
}

object PaymentsWrap extends ChannelListener with InvoiceBag {
  import Payments.{table, preimage, hash, status, stamp, sum, message, incoming, nodeId, fee}
  def notifyView = app.getContentResolver.notifyChange(app.db sqlPath table, null)

  def put(wrap: ExtendedInvoice) = app.db txWrap {
    val search = s"${wrap.invoice.message.orNull} ${wrap.invoice.paymentHash.toString}"
    app.db.change(Payments.newVirtualSql, search, wrap.invoice.paymentHash.toString)

    app.db.change(Payments.newSql, wrap.preimage.map(_.toString).orNull, wrap.invoice.paymentHash.toString,
      wrap.status.toString, wrap.stamp.toString, wrap.invoice.sum.amount.toString, wrap.invoice.message.orNull,
      if (wrap.incoming) "1" else "0", wrap.invoice.nodeId.toString, wrap.fee.map(_.toString).orNull)
  }

  def getExtendedInvoice(hash: BinaryData): Option[ExtendedInvoice] = {
    val basicCursor = app.db.select(Payments.selectByHashSql, hash.toString)
    RichCursor(basicCursor).headTry(toExtendedInvoice).toOption
  }

  def putPreimage(preimage: BinaryData) = app.db.change(Payments.updPreimageSql, preimage.toString, sha256(preimage).toString)
  def toExtendedInvoice(rc: RichCursor) = ExtendedInvoice(preimage = Option(rc string preimage), Option(rc long fee),
    Invoice(message = Option(rc string message), rc string nodeId, MilliSatoshi(rc long sum), rc string hash),
    rc long status, rc long incoming equals 1, rc long stamp)
}