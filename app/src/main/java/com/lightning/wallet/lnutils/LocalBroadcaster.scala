package com.lightning.wallet.lnutils

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lnutils.ImplicitConversions._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Sha256Hash

import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw: Long = RatesSaver.rates.feeLive.value / 2
  def send(tx: Transaction): String = app.kit.blockingSend(tx).toString
  def txStatus(txid: BinaryData) = txStatus(Sha256Hash wrap txid.toArray)

  // Confirmations and confidence
  def txStatus(txid: Sha256Hash) = for {
    tx <- Option(app.kit.wallet getTransaction txid)
    dead = tx.getConfidence.getConfidenceType == DEAD
    depth = tx.getConfidence.getDepthInBlocks
  } yield depth.toLong -> dead

  def currentHeight: Int =
    math.max(app.kit.wallet.getLastBlockSeenHeight,
      app.kit.peerGroup.getMostCommonChainHeight)

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, WAIT_FUNDING_DONE) =>
      safeSend(wait.fundingTx).foreach(Tools.log, Tools.errlog)
      app.kit watchFunding wait.commitments

    case (_, refund: RefundingData, _, REFUNDING) =>
      app.kit watchFunding refund.commitments

    case (chan, _, SYNC, NORMAL) =>
      // Will be sent once on app lauch
      chan process CMDFeerate(feeRatePerKw)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      val tier12Publishable = close.tier12States.filter(_.isPublishable).flatMap(_.txs)
      val all = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ tier12Publishable
      Obs.from(all map safeSend).concat.foreach(Tools.log, Tools.errlog)
  }
}