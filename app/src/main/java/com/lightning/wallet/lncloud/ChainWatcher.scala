package com.lightning.wallet.lncloud

import android.database.Cursor
import spray.json._
import com.lightning.wallet.ln._

import collection.JavaConverters._
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
      // We are looking for an input which spends exactly our funding tx hash and output index
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
  import PaymentSpecTable.{data, status, stamp, table, searchData}
  private def byHash(hash: BinaryData): Cursor = LNParams.db.select(PaymentSpecTable.selectByHashSql, hash.toString)
  def getIncomingPaymentSpec(hash: BinaryData) = RichCursor(me byHash hash).headTry(_ string data) map to[IncomingPaymentSpec]
  def getOutgoingPaymentSpec(hash: BinaryData) = RichCursor(me byHash hash).headTry(_ string data) map to[OutgoingPaymentSpec]
  def getPaymentStatus(hash: BinaryData) = RichCursor(me byHash hash).headTry(_ string status)

  def updatePaymentStatus(paymentHash: BinaryData, status: String): Unit = {
    LNParams.db.change(PaymentSpecTable.updStatusSql, status, paymentHash.toString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def putPaymentSpec(spec: PaymentSpec, status: String) = LNParams.db txWrap {
    val hash ~ timestamp = spec.invoice.paymentHash.toString -> System.currentTimeMillis.toString
    LNParams.db.change(PaymentSpecTable.newVirtualSql, s"${spec.invoice.message.orNull} $hash", hash)
    LNParams.db.change(PaymentSpecTable.newSql, spec.toJson.toString, hash, status, timestamp)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def updateOutgoingPaymentSpec(spec: OutgoingPaymentSpec): Unit = {
    val hash ~ data = spec.invoice.paymentHash.toString -> spec.toJson.toString
    LNParams.db.change(sql = PaymentSpecTable.updDataSql, params = hash, data)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def addPreimage(spec: OutgoingPaymentSpec)(preimage: BinaryData): Unit =
    me updateOutgoingPaymentSpec spec.copy(preimage = Some apply preimage)
}