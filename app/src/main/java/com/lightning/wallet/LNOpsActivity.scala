package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
import com.lightning.wallet.Utils.{app, sumIn}
import com.lightning.wallet.ln.Tools.none
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
  override def onDestroy = whenDestroy.run

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_ln_ops)
    app.ChannelManager.all.headOption match {
      case Some(channel) => manageFirst(channel)
      case None => manageNoActiveChannel
    }
  }

  private def prettyTxAmount(tx: Transaction) =
    sumIn format withSign(tx.txOut.head.amount)

  private def manageFirst(chan: Channel) = {
    def manageOpening(c: Commitments, open: Transaction) = {
      val channelCapacity = sumIn format withSign(c.commitInput.txOut.amount)
      val currentState = app.plurOrZero(txsConfs, LNParams.broadcaster.getConfirmations(open.txid).getOrElse(0).toLong)
      val confirmationThreshold = app.plurOrZero(number = math.max(c.remoteParams.minimumDepth, LNParams.minDepth), opts = txsConfs)
      lnOpsDescription setText getString(ln_ops_chan_opening).format(channelCapacity, confirmationThreshold, currentState).html
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsAction setText ln_force_close
    }

    def manageNegotiations(c: Commitments) = {
      val remainder = sumIn format withSign(MilliSatoshi apply c.localCommit.spec.toLocalMsat)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_negotiations).format(remainder).html
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsAction setText ln_force_close
    }

    def warnAboutUnilateralClosing =
      mkForm(mkChoiceDialog(chan async CMDShutdown, none, ln_force_close,
        dialog_cancel), null, getString(ln_ops_chan_unilateral_warn).html)

    val chanOpsListener = new ChannelListener {
      def reloadOnBecome: Unit = onBecome(chan, chan.data, null, chan.state)
      override def onProcess = { case (_, _, _: CMDHeight) => reloadOnBecome }
      override def onError = { case _ => chan process CMDShutdown }

      override def onBecome = {
        // A new channel has been created with funding transaction broadcasted
        case (_, WaitFundingDoneData(_, _, _, tx, commitments, _), _, _) =>
          me runOnUiThread manageOpening(commitments, tx)

        // Both sides have sent a funding locked
        case (_, norm: NormalData, _, NORMAL) =>
          me exitTo classOf[LNActivity]

        // Mutual shutdown initiated
        case (_, norm: NormalData, _, _)
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread manageNegotiations(norm.commitments)

        // Closing tx fee negotiations
        case (_, negs: NegotiationsData, _, _) =>
          me runOnUiThread manageNegotiations(negs.commitments)

        // Mutual closing is in progress because only a mutual close tx is available here
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil, _, _), _, CLOSING) =>
          me runOnUiThread manageMutualClosing(tx)

        // Mutual closing but we have no transactions at all so just drop it
        case (_, ClosingData(_, _, Nil, Nil, Nil, Nil, Nil, _, _), _, CLOSING) =>
          me runOnUiThread manageNoActiveChannel

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs, _, _), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          me runOnUiThread manageForcedClosing(close)
      }
    }

    whenDestroy = anyToRunnable {
      chan.listeners -= chanOpsListener
      super.onDestroy
    }

    chan.listeners += chanOpsListener
    chanOpsListener.reloadOnBecome
  }

  // UI which does not need a channel access

  private def manageMutualClosing(close: Transaction) = {
    val finalizeThreshold = app.plurOrZero(txsConfs, LNParams.minDepth)
    val currentState = app.plurOrZero(txsConfs, LNParams.broadcaster.getConfirmations(close.txid).getOrElse(0).toLong)
    lnOpsDescription setText getString(ln_ops_chan_bilateral_closing).format(finalizeThreshold, currentState).html
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsAction setText ln_ops_start
  }

  private def manageForcedClosing(data: ClosingData) = {
    def statusView(status: BroadcastStatus): String = status match {
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    }

    val txs = LNParams.broadcaster extractTxs data
    val schedule = LNParams.broadcaster convertToBroadcastStatus txs map statusView mkString "<br>"
    lnOpsDescription setText getString(ln_ops_chan_unilateral_closing).format(schedule).html
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsAction setText ln_ops_start
  }

  // Offer to create a new channel
  private def manageNoActiveChannel = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}