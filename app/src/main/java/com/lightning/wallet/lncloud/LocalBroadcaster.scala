package com.lightning.wallet.lncloud

import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitConversions._

import rx.lang.scala.schedulers.IOScheduler
import com.lightning.wallet.ln.Broadcaster
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app


class LocalBroadcaster extends Broadcaster { me =>
  def broadcast(tx: fr.acinq.bitcoin.Transaction): Unit =
    obsOn(app.kit.peerGroup.broadcastTransaction(tx, 1).broadcast.get, IOScheduler.apply)
      .subscribe(tx => android.util.Log.d("LocalBroadcaster", tx.toString), none)

  def currentHeight: Int = app.kit.peerGroup.getMostCommonChainHeight
  def currentFeeRate: Long = RatesSaver.rates.feeLive.value
}