package com.lightning.wallet.lncloud

import rx.lang.scala.{Observable => Obs}
import org.bitcoinj.core.{Peer, Block, FilteredBlock}
import com.lightning.wallet.ln.{Broadcaster, ScheduledTx}

import com.lightning.wallet.lncloud.LocalBroadcasterSaver.ScheduledTxs
import com.lightning.wallet.lncloud.JsonHttpUtils.obsOn
import com.lightning.wallet.MyPeerDataListener
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.Tools.wrap
import com.lightning.wallet.Utils.app


case class LocalBroadcaster(pending: ScheduledTxs, broadcasted: ScheduledTxs) extends Broadcaster { me =>
  def send(stx: ScheduledTx): Unit = broadcast(stx).subscribe(_ => me unschedule stx, _ => me unschedule stx)
  def resend(stx: ScheduledTx): Unit = broadcast(stx).subscribe(_ => broadcasted -= stx, _ => broadcasted -= stx)

  // This either throws or ends normally or keeps waiting
  def broadcast(stx: ScheduledTx): Obs[org.bitcoinj.core.Transaction] =
    obsOn(new org.bitcoinj.core.Transaction(app.params, stx.hex), IOScheduler.apply)
      .map(tx => app.kit.peerGroup.broadcastTransaction(tx, 1).broadcast.get)

  def schedule(what: ScheduledTx): Unit = wrap(pending += what)(broadcasted -= what)
  def unschedule(what: ScheduledTx): Unit = wrap(pending -= what)(broadcasted += what)
  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def currentFeeRate: Long = RatesSaver.rates.feeLive.value

  app.kit.peerGroup addBlocksDownloadedEventListener new MyPeerDataListener {
    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = if (left < 2) {
      for (stx <- broadcasted if stx.atBlock < currentHeight - blocksPerDay * 7) me resend stx
      for (stx <- pending if stx.atBlock < currentHeight) me send stx
    }
  }
}