package com.lightning.wallet.test

import com.lightning.wallet.Utils.app
import com.lightning.wallet.ln.wire.CommitSig
import com.lightning.wallet.ln.{Channel, ChannelListener, LNParams, NormalData}
import com.lightning.wallet.lnutils.{ChannelWrap, CloudAct}
import com.lightning.wallet.lnutils.Connector._
import com.lightning.wallet.ln.Tools.random


class LNCloudSpec {

  def allTests = {
    // This test requires an operational channel
    val chan: Channel = app.ChannelManager.alive.head

    val restartListener = new ChannelListener {
      override def onProcess = {
        case (_, norm: NormalData, _: CommitSig)
          // GUARD: notify and vibrate because HTLC is fulfilled
          if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
          println("payment confirmed, retrying cloud acts")
          LNParams.cloud doProcess CMDStart

      }
    }

    chan.listeners += restartListener
    chan.listeners -= ChannelWrap

    val set: Set[CloudAct] = List.fill(750)(CloudAct(random getBytes 32, Nil, "check")).toSet
    LNParams.cloud.data = LNParams.cloud.data.copy(acts = set)
    LNParams.cloud doProcess CMDStart
  }
}
