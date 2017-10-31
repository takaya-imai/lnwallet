package com.lightning.wallet.lnutils

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType._
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Sha256Hash

import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw: Long = RatesSaver.rates.feeLive.value / 2
  def send(tx: Transaction): String = app.kit.blockingSend(tx).toString

  def txStatus(txid: BinaryData) = {
    val wrapped = Sha256Hash wrap txid.toArray
    Option(app.kit.wallet getTransaction wrapped) map { tx =>
      val isTxDead = tx.getConfidence.getConfidenceType == DEAD
      tx.getConfidence.getDepthInBlocks -> isTxDead
    } getOrElse 0 -> false
  }

  def currentHeight: Int =
    math.max(app.kit.wallet.getLastBlockSeenHeight,
      app.kit.peerGroup.getMostCommonChainHeight)

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, WAIT_FUNDING_DONE) =>
      safeSend(wait.fundingTx).foreach(Tools.log, Tools.errlog)
      app.kit watchFunding wait.commitments

    case (_, refund: RefundingData, _, REFUNDING) =>
      app.kit watchFunding refund.commitments

    case (chan, norm: NormalData, SYNC, NORMAL) =>
      // Check for fee changes once on channel becoming online
      val currentFee = norm.commitments.localCommit.spec.feeratePerKw
      val shouldUpdate = LNParams.shouldUpdateFee(currentFee, feeRatePerKw)
      if (shouldUpdate) chan.sendFeeUpdate(norm, feeRatePerKw)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      val tier12Publishable = for (ts <- close.tier12States if ts.isPublishable) yield ts.txn
      val all = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      Obs.from(all map safeSend).concat.foreach(Tools.log, Tools.errlog)
  }
}