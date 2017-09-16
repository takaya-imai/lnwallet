package com.lightning.wallet.test

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.Utils.app
import com.lightning.wallet.ln.wire.CommitSig
import com.lightning.wallet.ln.{Channel, ChannelListener, LNParams, NormalData}
import com.lightning.wallet.lncloud.{Cloud, CloudAct}
import com.lightning.wallet.lncloud.Connector._
import com.lightning.wallet.ln.Tools.random
import fr.acinq.bitcoin.BinaryData


case class CheckCloudAct(requestPayload: BinaryData) extends CloudAct {
  def run(params: Seq[HttpParam], cloud: Cloud) = cloud.connector
    .call("check", res => println(res.head.convertTo[BinaryData] == requestPayload), params:_*)
}

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
    val set: Set[CloudAct] = List.fill(75)(CheckCloudAct(random getBytes 32)).toSet
    LNParams.cloud.data = LNParams.cloud.data.copy(acts = set)
    LNParams.cloud doProcess CMDStart
  }
}
