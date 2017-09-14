package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._

import com.lightning.wallet.Utils.{app, sumIn, denom}
import fr.acinq.bitcoin.{MilliSatoshi, Transaction}
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
    sumIn.format(denom withSign tx.txOut.head.amount)

  private def manageFirst(chan: Channel) = {
    def manageOpening(c: Commitments, open: Transaction) = {
      val channelCapacity = sumIn.format(denom withSign c.commitInput.txOut.amount)
      val currentState = app.plurOrZero(txsConfs, LNParams.broadcaster.getConfirmations(open.txid).getOrElse(0).toLong)
      val confirmationThreshold = app.plurOrZero(number = math.max(c.remoteParams.minimumDepth, LNParams.minDepth), opts = txsConfs)
      lnOpsDescription setText getString(ln_ops_chan_opening).format(channelCapacity, confirmationThreshold, currentState).html
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsAction setText ln_force_close
    }

    def manageNegotiations(c: Commitments) = {
      val withSign = denom withSign MilliSatoshi(c.localCommit.spec.toLocalMsat)
      val description = getString(ln_ops_chan_bilateral_negotiations).format(sumIn format withSign)
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsDescription setText description.html
      lnOpsAction setText ln_force_close
    }

    def warnAboutUnilateralClosing =
      mkForm(mkChoiceDialog(chan process CMDShutdown, none, ln_force_close,
        dialog_cancel), null, getString(ln_ops_chan_unilateral_warn).html)

    val chanOpsListener = new ChannelListener {
      // Updates UI accordingly to changes in channel

      override def onBecome = {
        // A new channel has been created with funding transaction broadcasted
        case (_, WaitFundingDoneData(_, _, _, tx, commitments), _, _) =>
          me runOnUiThread manageOpening(commitments, tx)

        // Mutual shutdown initiated
        case (_, norm: NormalData, _, _) if norm.isShutDown =>
          me runOnUiThread manageNegotiations(norm.commitments)

        // Normal is managed by main activity
        case (_, norm: NormalData, _, _) =>
          me exitTo classOf[LNActivity]

        // Closing tx fee negotiations started
        case (_, negs: NegotiationsData, _, _) =>
          me runOnUiThread manageNegotiations(negs.commitments)

        // Mutual closing is in progress because only a mutual close tx is available here
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil, _), _, CLOSING) =>
          me runOnUiThread manageMutualClosing(tx)

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, remoteNextTxs, revokedTxs, _), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || remoteNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          me runOnUiThread manageForcedClosing(close)

        case _ =>
          // Mutual closing without txs, recovery mode
          // and any other possibly unaccounted state
          me runOnUiThread manageNoActiveChannel
      }

      override def onProcess = {
        case (_, _, _: CMDBestHeight) =>
          // Need to update UI on each block
          reloadOnBecome(chan)
      }

      override def onError = {
        case error: Throwable =>
          chan process CMDShutdown
          Tools errlog error
      }
    }

    whenDestroy = anyToRunnable {
      chan.listeners -= chanOpsListener
      super.onDestroy
    }

    chan.listeners += chanOpsListener
    chanOpsListener reloadOnBecome chan
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
    def statusView(status: BroadcastStatus) = status match {
      case BroadcastStatus(None, false, tx) => getString(ln_ops_chan_unilateral_status_wait) format prettyTxAmount(tx)
      case BroadcastStatus(Some(blocks), false, tx) => prettyTxAmount(tx) + " " + app.plurOrZero(blocksLeft, blocks)
      case BroadcastStatus(_, true, tx) => getString(ln_ops_chan_unilateral_status_done) format prettyTxAmount(tx)
    }

    lnOpsAction setText ln_ops_start
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText getString(ln_ops_chan_unilateral_closing)
      .format(data.bss map statusView mkString "<br>").html
  }

  // Offer to create a new channel
  private def manageNoActiveChannel = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}