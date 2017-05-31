package com.lightning.wallet

import com.lightning.wallet.R.string._
import collection.JavaConverters._
import scala.concurrent.duration._
import com.lightning.wallet.lncloud.ImplicitConversions._
import rx.lang.scala.{Subscription, Observable => Obs}
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.{sumIn, app}
import android.widget.Button
import android.os.Bundle
import android.view.View
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.helper.SocketListener
import com.lightning.wallet.lncloud.ChainWatcher._


class LNOpsActivity extends TimerActivity { me =>
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val lnOpsDescription = me clickableTextField findViewById(R.id.lnOpsDescription)
  lazy val lnOpsAction = findViewById(R.id.lnOpsAction).asInstanceOf[Button]
  def goBitcoin(view: View) = me exitTo classOf[BtcActivity]

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_ln_ops)

    app.TransData.value match {
      case kit: ChannelKit => me kitIsPresent kit
      case _ => showNoKitPresentInfo
    }
  }

  private def kitIsPresent(kit: ChannelKit) = {
    val channelOpsListener = new StateMachineListener { self =>
      override def onBecome: PartialFunction[Transition, Unit] = {
        case (_, WaitFundingConfirmedData(_, _, _, fundingTx, commitments), _, WAIT_FUNDING_DONE) =>
          def showOpeningInfoOnUi(depth: CMDDepth) = me runOnUiThread showOpeningInfo(commitments, depth)

          app.kit.wallet addWatchedScript commitments.commitInput.txOut.publicKeyScript
          kit.subscriptions += watchTxDepthLocal(fundingTx.txid.toString).subscribe(kit.chan process _)
          kit.subscriptions += watchTxDepthLocal(fundingTx.txid.toString).subscribe(showOpeningInfoOnUi _)
          kit.subscriptions += watchInputUsedLocal(commitments.commitInput.outPoint).subscribe(kit.chan process _)
          kit.socket.listeners += kit.restartSocketListener
          <(app.kit blockingSend fundingTx, none)(none)
      }
    }

    val start = (null, kit.chan.data, null, kit.chan.state)
    kit.chan.listeners += channelOpsListener
    channelOpsListener onBecome start
  }

  private def showOpeningInfo(c: Commitments, depth: CMDDepth) = {
    val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
    val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, depth.depth)
    val humanCanReceive = app.plurOrZero(txsConfs, 6)

    lnOpsAction setText ln_forse_close
    lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
    lnOpsDescription setText getString(ln_ops_chan_opening).format(humanAmount,
      humanCanSend, humanCanReceive, humanCurrentState)
  }

  private def warnAboutUnilateralClosing: Unit = {
    val warn = me getString ln_ops_chan_unilateral_warn
    mkForm(me negBld dialog_ok, null, warn)
  }

  private def showNoKitPresentInfo: Unit = {
    def goStartChannel = me goTo classOf[LNStartActivity]
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}
