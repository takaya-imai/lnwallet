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

  private def rebroadcast(tx: Transaction) = {
    val txOpt = Option(app.kit.wallet getTransaction tx.hash)
    val depthOpt = txOpt.map(_.getConfidence.getDepthInBlocks)
    LNParams.broadcaster safeSend tx foreach Tools.log
    CMDDepth(depthOpt getOrElse 0)
  }

  private def kitIsPresent(kit: ChannelKit) = {
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


    val interfaceHolder = new ReplaceRunnableHolder
    val inputUsedHolder = registerInputUsedLocal(kit)

    val chanViewListener = new StateMachineListener {
      override def onBecome: PartialFunction[Transition, Unit] = {
        // null indicates this is a startup call and makes this case idempotent
        case (null, WaitFundingConfirmedData(_, _, _, tx, commitments), null, WAIT_FUNDING_DONE) =>
          interfaceHolder add watchTxDepthLocal(manageOpeningInfo(commitments), tx.txid.toString)
          app.kit.wallet addWatchedScript commitments.commitInput.txOut.publicKeyScript
          manageOpeningInfo(commitments)(me rebroadcast tx)

        case (_, norm: NormalData, _, NORMAL)
          // GUARD: mutual shutdown has been initiated
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread showNegotiationsInfo(norm.commitments)

        // Both sides have sent a funding locked
        case (_, norm: NormalData, _, NORMAL) =>
          me exitTo classOf[LNActivity]

        // Closing tx fee negotiations are in process
        case (_, negs: NegotiationsData, _, NEGOTIATIONS) =>
          me runOnUiThread showNegotiationsInfo(negs.commitments)

        // Mutual closing is in progress as only a mutual close tx is available
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil), _, CLOSING) =>
          interfaceHolder add watchTxDepthLocal(showMutualClosingInfo(tx), tx.txid.toString)
          showMutualClosingInfo(tx)(me rebroadcast tx)

        // Mutual closing but we have no transactions so just drop it
        case (_, ClosingData(_, _, Nil, Nil, Nil, Nil, Nil), _, CLOSING) =>
          me runOnUiThread showNoKitPresentInfo

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          interfaceHolder add watchChainHeight(_ => me manageForcedClosing close)
          me startForcedClosing close
      }
    }

    whenDestroy = anyToRunnable {
      kit.socket.listeners -= kit.reconnectSockListener
      kit.chan.listeners -= chanViewListener
      interfaceHolder.release
      inputUsedHolder.release
      super.onDestroy
    }

    kit.chan.listeners += chanViewListener
    kit.socket.listeners += kit.reconnectSockListener
    chanViewListener onBecome kit.chan.initTransition
  }

  // UI which does not need a channel access

  private def prettyTxAmount(tx: Transaction) =
    sumIn format withSign(tx.txOut.head.amount)

  private def showMutualClosingInfo(close: Transaction)(cmd: CMDDepth) = {
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

  // FORCED CLOSING VIEW AND LOGIC

  private def startForcedClosing(close: ClosingData) = {
    val txs = for (lc <- close.localCommit) yield lc.commitTx
    // First we try to spend a local commit tx, then all the 2nd tier transactions
    val sender = LNParams.broadcaster.sendAtOnce(txs:_*).map(_ mkString "\n\n")
    sender.doOnTerminate(me manageForcedClosing close) foreach Tools.log
    me manageForcedClosing close
  }

  private def manageForcedClosing(data: ClosingData) = {
    val secondTierTxs = LNParams.broadcaster extractTxs data
    val parentDepthMap = LNParams.broadcaster.getParentsDepth
    // Broadcast all the suitable transactions and update user interface accordingly
    val bss = LNParams.broadcaster.convertToBroadcastStatus(secondTierTxs, parentDepthMap)
    val txsToSend = bss collect { case castStatus if castStatus.publishable => castStatus.tx }
    LNParams.broadcaster.sendAtOnce(txsToSend:_*).map(_ mkString "\n\n") foreach none
    me runOnUiThread updateInterface

    def updateInterface = {
      val schedule = bss map castStatusView mkString "<br>"
      val unilateralClosing = getString(ln_ops_chan_unilateral_closing)
      lnOpsDescription setText unilateralClosing.format(schedule).html
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsAction setText ln_ops_start
    }

    def castStatusView(status: BroadcastStatus): String = status match {
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    }
  }

  // OFFER TO CREATE A NEW CHANNEL

  private def showNoKitPresentInfo: Unit = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}