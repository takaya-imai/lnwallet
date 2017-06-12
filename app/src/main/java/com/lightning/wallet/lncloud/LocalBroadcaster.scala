package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import collection.JavaConverters._
import com.lightning.wallet.lncloud.ImplicitConversions._

import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.Transaction


object LocalBroadcaster extends Broadcaster { me =>
  def currentFeeRate: Long = RatesSaver.rates.feeLive.value
  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def send(tx: Transaction): String = app.kit.blockingSend(tx).toString

  def getParentsDepth: ParentTxidToDepth = {
    val txs = app.kit.wallet.getTransactions(false).asScala
    txs.map(tx => tx.getHashAsString -> tx.getConfidence.getDepthInBlocks)
  }.toMap

  override def onBecome = {
    case (_, wait: WaitFundingConfirmedData, _, Channel.WAIT_FUNDING_DONE) =>
      // In a thin wallet we need to watch for transactions which spend external outputs
      app.kit.wallet addWatchedScript wait.commitments.commitInput.txOut.publicKeyScript
      safeSend(wait.fundingTx).foreach(Tools.log, _.printStackTrace)

    case (_, closing: ClosingData, _, Channel.CLOSING) =>
      // Mutual closing and local commit should not be present at once but anyway an order matters here
      val toPublish = closing.mutualClose ++ closing.localCommit.map(_.commitTx) ++ extractTxs(closing)
      Obs.from(toPublish map safeSend).concat.foreach(Tools.log, _.printStackTrace)
  }
}