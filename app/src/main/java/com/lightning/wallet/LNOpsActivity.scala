package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
import com.lightning.wallet.ln.Tools.{none, wrap}
import com.lightning.wallet.Utils.{app, sumIn}

import concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.lncloud.ActiveKit
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
        case kit: ActiveKit => me showActive kit
        case _ => showNoKitPresentInfo
      }
    }

  private def getConfirmations(tx: Transaction) = {
    val txOpt = Option(app.kit.wallet getTransaction tx.txid)
    txOpt.map(_.getConfidence.getDepthInBlocks) getOrElse 0
  }

  private def showActive(kit: ActiveKit) = {
    def manageOpeningInfo(c: Commitments, confs: Int) = {
      val humanAmount = sumIn format withSign(c.commitInput.txOut.amount)
      val humanCanSend = app.plurOrZero(txsConfs, LNParams.minDepth)
      val humanCurrentState = app.plurOrZero(txsConfs, confs)
      val humanCanReceive = app.plurOrZero(txsConfs, 6)
      me runOnUiThread updateInterface

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
        // A new channel has been created with funding transaction broadcasted so now we wait
        case (_, WaitFundingConfirmedData(_, _, _, tx, commitments, _), _, WAIT_FUNDING_DONE) =>
          manageOpeningInfo(commitments, me getConfirmations tx)

        case (_, norm: NormalData, _, NORMAL)
          // GUARD: mutual shutdown has been initiated
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread showNegotiationsInfo(norm.commitments)

        // Both sides have sent a funding locked so we're all good
        case (_, norm: NormalData, WAIT_FUNDING_DONE, NORMAL) =>
          me exitTo classOf[LNActivity]

        // Closing tx fee negotiations are in process
        case (_, negs: NegotiationsData, _, NEGOTIATIONS) =>
          me runOnUiThread showNegotiationsInfo(negs.commitments)

        // Mutual closing is in progress as only a mutual close tx is available
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil, _), _, CLOSING) =>
          me manageMutualClosing tx

        // Mutual closing but we have no transactions so just drop it
        case (_, ClosingData(_, _, Nil, Nil, Nil, Nil, Nil, _), _, CLOSING) =>
          me runOnUiThread showNoKitPresentInfo

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs, _), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          me manageForcedClosing close
      }

      override def onError = {
        case chanRelated: Throwable =>
          kit.chan process CMDShutdown
      }
    }

    whenDestroy = anyToRunnable {
      kit.socket.listeners -= kit.reconnectSockListener
      kit.chan.listeners -= chanViewListener
      super.onDestroy
    }

    kit.chan.listeners += chanViewListener
    kit.socket.listeners += kit.reconnectSockListener
    kit.chan.init(kit.chan.data, kit.chan.state)
  }

  // UI which does not need a channel access

  private def manageMutualClosing(close: Transaction) = {
    val humanCanFinalize: String = app.plurOrZero(txsConfs, LNParams.minDepth)
    val humanCurrentState = app.plurOrZero(txsConfs, me getConfirmations close)
    me runOnUiThread updateInterface

    def updateInterface = {
      lnOpsAction setText ln_ops_start
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_closing)
        .format(humanCanFinalize, humanCurrentState).html
    }
  }

  private def manageForcedClosing(data: ClosingData) = {
    val bss = LNParams.broadcaster convertToBroadcastStatus data
    me runOnUiThread updateInterface

    def updateInterface = {
      val schedule = bss map statusView mkString "<br>"
      val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
      lnOpsDescription setText unilateralClosing.format(schedule).html
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsAction setText ln_ops_start
    }

    def statusView(status: BroadcastStatus): String = status match {
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    }

    def prettyTxAmount(tx: Transaction) =
      sumIn format withSign(tx.txOut.head.amount)
  }

  // Offer to create a new channel

  private def showNoKitPresentInfo: Unit = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}