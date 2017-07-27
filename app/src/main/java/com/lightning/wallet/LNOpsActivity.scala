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
  }

  private def getConfirmations(tx: Transaction) = {
    val txOpt = Option(app.kit.wallet getTransaction tx.txid)
    txOpt.map(_.getConfidence.getDepthInBlocks) getOrElse 0
  }

  private def manageActive(chan: Channel) = {
    def manageOpeningInfo(c: Commitments, confs: Int) = {
      val threshold = math.max(c.remoteParams.minimumDepth, LNParams.minDepth)
      val channelCapacity = sumIn format withSign(c.commitInput.txOut.amount)
      val currentState = app.plurOrZero(txsConfs, confs)
      me runOnUiThread updateInterface

      def updateInterface = {
        lnOpsAction setText ln_force_close
        lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
        lnOpsDescription setText getString(ln_ops_chan_opening).format(channelCapacity,
          app.plurOrZero(txsConfs, threshold), currentState).html
      }
    }

    def manageNegotiationsInfo(c: Commitments) = {
      val remainder = sumIn format withSign(MilliSatoshi apply c.localCommit.spec.toLocalMsat)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_negotiations).format(remainder).html
      lnOpsAction setOnClickListener onButtonTap(warnAboutUnilateralClosing)
      lnOpsAction setText ln_force_close
    }

    def warnAboutUnilateralClosing =
      mkForm(mkChoiceDialog(chan async CMDShutdown, none, ln_force_close,
        dialog_cancel), null, getString(ln_ops_chan_unilateral_warn).html)

    val chanOpsListener = new ChannelListener {
      // We monitor internal channel state here to
      // interact with user and control the program flow

      override def onBecome = {
        // A new channel has been created with funding transaction broadcasted
        case (_, WaitFundingDoneData(_, _, _, tx, commitments, _), _, _) =>
          manageOpeningInfo(commitments, me getConfirmations tx)

        // Both sides have sent a funding locked so we're all good
        case (_, norm: NormalData, WAIT_FUNDING_DONE | SYNC, NORMAL) =>
          me exitTo classOf[LNActivity]

        case (_, norm: NormalData, _, _)
          // GUARD: mutual shutdown has been initiated
          if norm.localShutdown.isDefined || norm.remoteShutdown.isDefined =>
          me runOnUiThread manageNegotiationsInfo(norm.commitments)

        // Closing tx fee negotiations
        case (_, negs: NegotiationsData, _, _) =>
          me runOnUiThread manageNegotiationsInfo(negs.commitments)

        // Mutual closing is in progress because only a mutual close tx is available here
        case (_, ClosingData(_, commitments, tx :: _, Nil, Nil, Nil, Nil, _), _, CLOSING) =>
          me manageMutualClosing tx

        // Mutual closing but we have no transactions at all so just drop it
        case (_, ClosingData(_, _, Nil, Nil, Nil, Nil, Nil, _), _, CLOSING) =>
          me runOnUiThread manageNoActiveChannelInfo

        // Someone has initiated a unilateral channel closing
        case (_, close @ ClosingData(_, _, _, localTxs, remoteTxs, localNextTxs, revokedTxs, _), _, CLOSING)
          if localTxs.nonEmpty || remoteTxs.nonEmpty || localNextTxs.nonEmpty || revokedTxs.nonEmpty =>
          me manageForcedClosing close
      }

      override def onError = {
        case channelRelated: Throwable =>
          Tools log s"Channel $channelRelated"
          chan process CMDShutdown
      }

      override def onProcess = {
        case (_, data, _: CMDHeight) =>
          Tools log s"Calling onBecome method"
          onBecome(chan, data, null, chan.state)
      }
    }

    whenDestroy = anyToRunnable {
      chan.listeners -= chanOpsListener
      super.onDestroy
    }

    // Get it going
    chan.listeners += chanOpsListener
    chan process CMDHeight(0)
  }

  // UI which does not need a channel access

  private def manageMutualClosing(close: Transaction) = {
    val finalizeThreshold = app.plurOrZero(txsConfs, LNParams.minDepth)
    val currentState = app.plurOrZero(txsConfs, me getConfirmations close)
    me runOnUiThread updateInterface

    def updateInterface = {
      lnOpsAction setText ln_ops_start
      lnOpsAction setOnClickListener onButtonTap(goStartChannel)
      lnOpsDescription setText getString(ln_ops_chan_bilateral_closing)
        .format(finalizeThreshold, currentState).html
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

  private def manageNoActiveChannelInfo = {
    lnOpsAction setOnClickListener onButtonTap(goStartChannel)
    lnOpsDescription setText ln_ops_chan_none
    lnOpsAction setText ln_ops_start
  }
}