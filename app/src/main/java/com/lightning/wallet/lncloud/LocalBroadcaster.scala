package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import collection.JavaConverters._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._

import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Transaction
import scala.util.Try


class LocalBroadcaster extends Broadcaster { me =>
  def broadcast(tx: fr.acinq.bitcoin.Transaction): Unit =
    obsOn(app.kit.peerGroup.broadcastTransaction(tx, 1).broadcast.get, IOScheduler.apply)
      .subscribe(tx => android.util.Log.d("LocalBroadcaster", tx.getHashAsString), none)

  def currentFeeRate: Long = RatesSaver.rates.feeLive.value
  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def parentStatus: Map[String, Int] = app.kit.wallet.getTransactions(false).asScala
    .map(tx => tx.getHashAsString -> tx.getConfidence.getDepthInBlocks).toMap
}