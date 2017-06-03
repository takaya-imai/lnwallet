package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ChainWatcher._
import com.lightning.wallet.lncloud.ImplicitConversions._
import fr.acinq.bitcoin.{MilliSatoshi, Satoshi, Transaction}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.Utils.{app, sumIn}
import android.widget.Button
import android.os.Bundle
import android.view.View
import com.lightning.wallet.lncloud.LocalBroadcaster


class LNOpsActivity extends TimerActivity { me =>
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val blocksLeft = getResources getStringArray R.array.ln_ops_chan_unilateral_blocks_left
  lazy val lnOpsDescription = me clickableTextField findViewById(R.id.lnOpsDescription)
  lazy val lnOpsAction = findViewById(R.id.lnOpsAction).asInstanceOf[Button]
  def goBitcoin(view: View) = me exitTo classOf[BtcActivity]
  def goStartChannel = me goTo classOf[LNStartActivity]

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_ln_ops)
  }

  override def onResume =
    wrap(super.onResume) {
      app.TransData.value match {
        case kit: ChannelKit => me kitIsPresent kit
        case _ => showNoKitPresentInfo
      }
    }

  private def kitIsPresent(kit: ChannelKit) = {
    val channelOpsListener = new StateMachineListener { self =>
      override def onBecome: PartialFunction[Transition, Unit] = {
        // null indicates this is a startup call and makes this branch idempotent
        case (_, WaitFundingConfirmedData(_, _, _, fundingTx, commitments), null, WAIT_FUNDING_DONE) =>
          def showOpeningInfoOnUi(depth: CMDDepth) = me runOnUiThread showOpeningInfo(commitments, depth)
          kit.subscripts += watchInputUsedLocal(commitments.commitInput.outPoint).subscribe(kit.chan process _)
          kit.subscripts += watchTxDepthLocal(fundingTx.txid.toString).subscribe(showOpeningInfoOnUi _)
          kit.subscripts += watchTxDepthLocal(fundingTx.txid.toString).subscribe(kit.chan process _)
          app.kit.wallet addWatchedScript commitments.commitInput.txOut.publicKeyScript
          showOpeningInfoOnUi(CMDDepth apply 0)
          LocalBroadcaster broadcast fundingTx

        case (_, norm: NormalData, _, NORMAL)
          // Someone has sent a closing signature so we initiaite a shutdown
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

  private def prettyTxAmount(tx: Transaction) =
    sumIn format withSign(tx.txOut.head.amount)

  private def showOpeningInfo(c: Commitments, cmd: CMDDepth) = {
    val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
    val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)
    val humanCanReceive = app.plurOrZero(txsConfs, 6)

    lnOpsAction setText ln_force_close
    lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
    lnOpsDescription setText getString(ln_ops_chan_opening).format(humanAmount,
      humanCanSend, humanCanReceive, humanCurrentState).html
  }

  private def warnAboutUnilateralClosing: Unit = {
    val warn = me getString ln_ops_chan_unilateral_warn
    mkForm(me negBld dialog_ok, null, warn.html)
  }

  private def showNegotiationsInfo(c: Commitments): Unit = {
    val humanAmount = sumIn format withSign(MilliSatoshi apply c.localCommit.spec.toLocalMsat)
    lnOpsDescription setText getString(ln_ops_chan_bilateral_negotiations).format(humanAmount).html
    lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
    lnOpsAction setText ln_force_close
  }

  private def showMutualClosingInfo(close: Transaction, cmd: CMDDepth) = {
    val humanCanFinalize: String = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)

    lnOpsAction setText ln_ops_start
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText getString(ln_ops_chan_bilateral_closing)
      .format(me prettyTxAmount close, humanCanFinalize, humanCurrentState).html
  }

  private val showForcedClosingInfo: Seq[BroadcastStatus] => Unit = bss => {
    val totalDelayedAmount = Satoshi(bss.map(_.tx.txOut.head.amount.toLong).sum)
    val humanAmount = sumIn format withSign(totalDelayedAmount)

    lnOpsAction setText ln_ops_start
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText getString(ln_ops_chan_unilateral_closing)
      .format(humanAmount, bss map statusView mkString "<br>").html
  }

  private def statusView(status: BroadcastStatus) = status match {
    case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
    case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
  }

  private def showNoKitPresentInfo: Unit = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}
