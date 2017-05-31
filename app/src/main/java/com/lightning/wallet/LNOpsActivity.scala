package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ChainWatcher._
import com.lightning.wallet.lncloud.ImplicitConversions._

import com.lightning.wallet.Utils.{app, sumIn}
import com.lightning.wallet.ln.Tools.none
import fr.acinq.bitcoin.MilliSatoshi
import android.widget.Button
import android.os.Bundle
import android.view.View


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
          kit.subscripts += watchInputUsedLocal(commitments.commitInput.outPoint).subscribe(kit.chan process _)
          kit.subscripts += watchTxDepthLocal(fundingTx.txid.toString).subscribe(showOpeningInfoOnUi _)
          kit.subscripts += watchTxDepthLocal(fundingTx.txid.toString).subscribe(kit.chan process _)
          app.kit.wallet addWatchedScript commitments.commitInput.txOut.publicKeyScript
          <(app.kit blockingSend fundingTx, none)(none)
          showOpeningInfoOnUi(CMDDepth apply 0)

        case (_, norm: NormalData, _, NORMAL)
          // Someone has sent a closing signature so we initiaite shutdown
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread showNegotiationsInfo(norm.commitments)

        case (_, negotiations: NegotiationsData, _, NEGOTIATIONS) =>
          me runOnUiThread showNegotiationsInfo(negotiations.commitments)

        // Both sides have locked a funding
        case (_, _, WAIT_FUNDING_DONE, NORMAL) =>
          kit.subscripts.foreach(_.unsubscribe)
          me exitTo classOf[LNActivity]
          kit.chan.listeners -= self
      }
    }

    val start = (null, kit.chan.data, null, kit.chan.state)
    kit.socket.listeners += kit.restartSocketListener
    kit.chan.listeners += channelOpsListener
    channelOpsListener onBecome start
  }

  private def showOpeningInfo(c: Commitments, depth: CMDDepth) = {
    val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
    val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, depth.depth)
    val humanCanReceive = app.plurOrZero(txsConfs, 6)

    lnOpsAction setText ln_force_close
    lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
    lnOpsDescription setText getString(ln_ops_chan_opening).format(humanAmount,
      humanCanSend, humanCanReceive, humanCurrentState)
  }

  private def warnAboutUnilateralClosing: Unit = {
    val warn = me getString ln_ops_chan_unilateral_warn
    mkForm(me negBld dialog_ok, null, warn)
  }

  private def showNegotiationsInfo(c: Commitments): Unit = {
    val humanAmount: String = sumIn format withSign(MilliSatoshi apply c.localCommit.spec.toLocalMsat)
    lnOpsDescription setText getString(ln_ops_chan_bilateral_negotiations).format(humanAmount)
    lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
    lnOpsAction setText ln_force_close
  }

  private def showNoKitPresentInfo: Unit = {
    def goStartChannel = me goTo classOf[LNStartActivity]
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}
