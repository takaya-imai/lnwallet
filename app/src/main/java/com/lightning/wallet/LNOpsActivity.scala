package com.lightning.wallet

import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ChainWatcher._
import com.lightning.wallet.lncloud.ImplicitConversions._

import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.Utils.{app, sumIn}
import com.lightning.wallet.ln.Tools.{none, wrap}
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
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

  private def broadcastAndDepth(tx: Transaction) = {
    val txOpt = Option(app.kit.wallet getTransaction tx.hash)
    val depthOpt = txOpt.map(_.getConfidence.getDepthInBlocks)
    LNParams.broadcaster broadcast tx
    CMDDepth(depthOpt getOrElse 0)
  }

  private def kitIsPresent(kit: ChannelKit) = {
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

    // CHANNEL LISTENER

    val chanViewListener = new StateMachineListener { self =>
      override def onBecome: PartialFunction[Transition, Unit] = {
        // null indicates this is a startup call and makes this case idempotent
        case (null, WaitFundingConfirmedData(_, _, _, tx, commitments), null, WAIT_FUNDING_DONE) =>
          def updateInterface(depth: CMDDepth) = me runOnUiThread showOpeningInfo(commitments, depth)
          kit.subscripts += watchTxDepthLocal(tx.txid.toString).subscribe(kit.chan process _)
          kit.subscripts += watchTxDepthLocal(tx.txid.toString).subscribe(updateInterface _)
          app.kit.wallet addWatchedScript commitments.commitInput.txOut.publicKeyScript
          updateInterface(me broadcastAndDepth tx)

        case (_, norm: NormalData, _, NORMAL)
          // GUARD: mutual shutdown has been initiated
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread showNegotiationsInfo(norm.commitments)

        // Both sides have sent a funding locked
        case (_, norm: NormalData, _, NORMAL) =>
          me exitTo classOf[LNActivity]

        // Closing tx fee negotiations are in process
        case (_, negotiations: NegotiationsData, _, NEGOTIATIONS) =>
          me runOnUiThread showNegotiationsInfo(negotiations.commitments)

        // Mutual closing is in progress as only a mutual close tx is available
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil), _, CLOSING) =>
          def updateInterface(depth: CMDDepth) = me runOnUiThread showMutualClosingInfo(tx, depth)
          kit.subscripts += watchTxDepthLocal(tx.txid.toString).subscribe(updateInterface _)
          kit.socket.listeners -= kit.restartSocketListener
          updateInterface(me broadcastAndDepth tx)

        // Old closing data already contains unilateral transactions so do nothing here
        case (ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs), _, CLOSING, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          Tools log "Yet another channel violating transaction has been caught"

        // Peer has initiated a unilateral channel closing of some kind
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          kit.subscripts += Obs.interval(20.seconds).subscribe(_ => me manageForcedClosing close)
          kit.socket.listeners -= kit.restartSocketListener
      }
    }

    whenDestroy = anyToRunnable {
      for (subscript <- kit.subscripts) subscript.unsubscribe
      kit.socket.listeners -= kit.restartSocketListener
      kit.chan.listeners -= chanViewListener
      super.onDestroy
    }

    registerWatchInputUsedLocal(kit)
    kit.chan.listeners += chanViewListener
    kit.socket.listeners += kit.restartSocketListener
    chanViewListener onBecome kit.chan.initTransition
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

  private def manageForcedClosing(data: ClosingData) = {
    val parentDepthMap = LNParams.broadcaster.getParentsDepth
    val chainHeight = LNParams.broadcaster.currentHeight
    val txs = ClosingData extractTxs data

    val bss = LNParams.broadcaster.broadcastStatus(txs, parentDepthMap, chainHeight)
    for (BroadcastStatus(_, true, tx) <- bss) LNParams.broadcaster broadcast tx
    me runOnUiThread showForcedClosingInfo(bss:_*)
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