package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import collection.JavaConverters._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Transaction


object LocalBroadcaster extends Broadcaster {
  def broadcast(tx: Transaction): Unit = obsOn(app.kit blockingSend tx,
    IOScheduler.apply).subscribe(Tools log _.toString, _.printStackTrace)

  def currentFeeRate: Long = RatesSaver.rates.feeLive.value
  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def parents: Map[String, Int] = app.kit.wallet.getTransactions(false).asScala
    .map(tx => tx.getHashAsString -> tx.getConfidence.getDepthInBlocks).toMap
}