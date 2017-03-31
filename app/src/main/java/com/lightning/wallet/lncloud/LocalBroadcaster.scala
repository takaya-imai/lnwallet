package com.lightning.wallet.lncloud

import com.lightning.wallet.helper.JsonHttpUtils._
import com.lightning.wallet.ln.{Broadcaster, ScheduledTx}
import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.Utils.app


class LocalBroadcaster(pending: BroadcasterSaver.Snapshot) extends Broadcaster { me =>
  def broadcastReady: Unit = pending.filter(_.atBlock < currentBlockCount) foreach broadcast
  def currentBlockCount: Int = app.kit.peerGroup.getMostCommonChainHeight
  def unschedule(what: ScheduledTx): Unit = pending -= what
  def schedule(what: ScheduledTx): Unit = pending += what

  def broadcast(what: ScheduledTx): Unit =
    obsOn(new org.bitcoinj.core.Transaction(app.params, what.tx), IOScheduler.apply)
      .map(tx => app.kit.peerGroup.broadcastTransaction(tx, 1).broadcast.get)
      .subscribe(_ => me unschedule what, _ => me unschedule what)
}
