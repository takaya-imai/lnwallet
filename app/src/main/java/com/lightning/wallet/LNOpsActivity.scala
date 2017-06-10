package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ChainWatcher._
import com.lightning.wallet.lncloud.ImplicitConversions._

import com.lightning.wallet.Utils.{app, sumIn}
import com.lightning.wallet.ln.Tools.{none, wrap}
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}

import com.lightning.wallet.lncloud.ReplaceRunnableHolder
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

  private def getDepth(tx: Transaction) = {
    val txOpt = Option(app.kit.wallet getTransaction tx.hash)
    val depthOpt = txOpt.map(_.getConfidence.getDepthInBlocks)
    CMDDepth(depthOpt getOrElse 0)
  }

  private def kitIsPresent(kit: ChannelKit) = {
    val interfaceHolder = new ReplaceRunnableHolder
    val inputUsedHolder = registerInputUsedLocal(kit.chan)

    // UI which needs a channel access

    def manageOpeningInfo(c: Commitments)(cmd: CMDDepth) = {
      val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
      val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
      val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)
      val humanCanReceive = app.plurOrZero(txsConfs, 6)
      me runOnUiThread updateInterface
      kit.chan process cmd

      def updateInterface = {
        lnOpsAction setText ln_force_close
        lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
        lnOpsDescription setText getString(ln_ops_chan_opening).format(humanAmount,
          humanCanSend, humanCanReceive, humanCurrentState).html
      }
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

    // UI updating listener

    val chanViewListener = new StateMachineListener {
      override def onBecome: PartialFunction[Transition, Unit] = {
        // null indicates this is a startup call so this case is idempotent
        case (null, WaitFundingConfirmedData(_, _, _, tx, commitments), null, WAIT_FUNDING_DONE) =>
          interfaceHolder hold watchTxDepthLocal(manageOpeningInfo(commitments), tx.txid.toString)
          manageOpeningInfo(commitments)(me getDepth tx)

        case (_, norm: NormalData, _, NORMAL)
          // GUARD: mutual shutdown has been initiated
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread showNegotiationsInfo(norm.commitments)

        // Both sides have sent a funding locked
        case (_, norm: NormalData, _, NORMAL) =>
          println("NORMAL STATE REACHED")
          //me exitTo classOf[LNActivity]

        // Closing tx fee negotiations are in process
        case (_, negs: NegotiationsData, _, NEGOTIATIONS) =>
          me runOnUiThread showNegotiationsInfo(negs.commitments)

        // Mutual closing is in progress as only a mutual close tx is available
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil), _, CLOSING) =>
          interfaceHolder hold watchTxDepthLocal(manageMutualClosing(tx), tx.txid.toString)
          manageMutualClosing(tx)(me getDepth tx)

        // Mutual closing but we have no transactions so just drop it
        case (_, ClosingData(_, _, Nil, Nil, Nil, Nil, Nil), _, CLOSING) =>
          me runOnUiThread showNoKitPresentInfo

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          interfaceHolder hold watchChainHeightLocal(_ => me manageForcedClosing close)
          me manageForcedClosing close
      }
    }

    whenDestroy = anyToRunnable {
      kit.socket.listeners -= kit.reconnectSockListener
      kit.chan.listeners -= LNParams.broadcaster
      kit.chan.listeners -= chanViewListener
      interfaceHolder.release
      inputUsedHolder.release
      super.onDestroy
    }

    kit.socket.listeners += kit.reconnectSockListener
    kit.chan.listeners += LNParams.broadcaster
    kit.chan.listeners += chanViewListener
    kit.chan.initBecome
  }

  // UI which does not need a channel access

  private def prettyTxAmount(tx: Transaction) =
    sumIn format withSign(tx.txOut.head.amount)

  private def manageMutualClosing(close: Transaction)(cmd: CMDDepth) = {
    val humanCanFinalize: String = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, cmd.depth)
    me runOnUiThread updateInterface

    def updateInterface = {
      lnOpsAction setText ln_ops_start
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_closing)
        .format(me prettyTxAmount close, humanCanFinalize, humanCurrentState).html
    }
  }

  private def manageForcedClosing(data: ClosingData) = {
    val bss = LNParams.broadcaster convertToBroadcastStatus data
    me runOnUiThread updateInterface

    def castStatusView(status: BroadcastStatus): String = status match {
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    }

    def updateInterface = {
      val schedule = bss map castStatusView mkString "<br>"
      val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
      lnOpsDescription setText unilateralClosing.format(schedule).html
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsAction setText ln_ops_start
    }
  }

  // Offer to create a new channel

  private def showNoKitPresentInfo: Unit = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}