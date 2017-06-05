package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ChainWatcher._
import com.lightning.wallet.lncloud.ImplicitConversions._

import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.Utils.{app, sumIn}

import com.lightning.wallet.lncloud.LocalBroadcaster
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import android.widget.Button
import android.os.Bundle
import android.view.View


class LNOpsActivity extends TimerActivity { me =>
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val blocksLeft = getResources getStringArray R.array.ln_ops_chan_unilateral_blocks_left
  lazy val lnOpsDescription = me clickableTextField findViewById(R.id.lnOpsDescription)
  lazy val lnOpsAction = findViewById(R.id.lnOpsAction).asInstanceOf[Button]
  def goBitcoin(view: View) = me exitTo classOf[BtcActivity]
  def goStartChannel = me exitTo classOf[LNStartActivity]

  // May change destroy action throughout an activity lifecycle
  private[this] var whenDestroy = anyToRunnable(super.onDestroy)
  override def onDestroy: Unit = whenDestroy.run

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
          // whenDestroy will be called after this
          me exitTo classOf[LNActivity]

        // We are no more interested in communications
        case (_, _, _, Channel.CLOSING | Channel.FINISHED) =>
          kit.socket.listeners -= kit.restartSocketListener
      }
    }

    whenDestroy = anyToRunnable {
      kit.socket.listeners -= kit.restartSocketListener
      kit.chan.listeners -= channelOpsListener
      kit.subscripts.foreach(_.unsubscribe)
      super.onDestroy
    }

    val start = (null, kit.chan.data, null, kit.chan.state)
    kit.socket.listeners += kit.restartSocketListener
    kit.chan.listeners += channelOpsListener
    channelOpsListener onBecome start

    // UI which needs a channel access

    def showOpeningInfo(c: Commitments, cmd: CMDDepth) = {
      val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
      val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
      val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)
      val humanCanReceive = app.plurOrZero(txsConfs, 6)

      lnOpsAction setText ln_force_close
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsDescription setText getString(ln_ops_chan_opening).format(humanAmount,
        humanCanSend, humanCanReceive, humanCurrentState).html
    }

    def showNegotiationsInfo(c: Commitments) = {
      val humanAmount = sumIn format withSign(MilliSatoshi apply c.localCommit.spec.toLocalMsat)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_negotiations).format(humanAmount).html
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsAction setText ln_force_close
    }

    def warnAboutUnilateralClosing =
      mkForm(mkChoiceDialog(ok = Future { kit.chan process CMDShutdown }, none,
        ln_force_close, dialog_cancel), null, getString(ln_ops_chan_unilateral_warn).html)
  }

  // UI which does not need a channel access

  private def prettyTxAmount(tx: Transaction) =
    sumIn format withSign(tx.txOut.head.amount)

  private def showMutualClosingInfo(close: Transaction, cmd: CMDDepth) = {
    val humanCanFinalize: String = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)

    lnOpsAction setText ln_ops_start
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText getString(ln_ops_chan_bilateral_closing)
      .format(me prettyTxAmount close, humanCanFinalize, humanCurrentState).html
  }

  private def showForcedClosingInfo(bss: BroadcastStatus*) = {
    def statusView(status: BroadcastStatus): String = status match {
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
    }

    val schedule = bss map statusView mkString "<br>"
    val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
    lnOpsDescription setText unilateralClosing.format(schedule).html
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsAction setText ln_ops_start
  }

  private def showNoKitPresentInfo = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}