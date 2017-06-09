package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import collection.JavaConverters._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Transaction


object LocalBroadcaster extends Broadcaster {
  def currentFeeRate: Long = RatesSaver.rates.feeLive.value
  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def send(tx: Transaction): String = app.kit.blockingSend(tx).toString

  def getParentsDepth: ParentTxidToDepth = {
    val txs = app.kit.wallet.getTransactions(false).asScala
    txs.map(tx => tx.getHashAsString -> tx.getConfidence.getDepthInBlocks)
  }.toMap
}