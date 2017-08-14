package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.ImplicitConversions._
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.Utils.app


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw = RatesSaver.rates.feeLive.value / 2
  def currentHeight = app.kit.peerGroup.getMostCommonChainHeight
  def send(tx: Transaction) = app.kit.blockingSend(tx).toString

  def getConfirmations(txid: BinaryData) =
    Option(app.kit.wallet getTransaction txid)
      .map(_.getConfidence.getDepthInBlocks)

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, WAIT_FUNDING_DONE) =>
      // In a thin wallet we need to watch for transactions which spend external outputs
      app.kit.wallet.addWatchedScript(wait.commitments.commitInput.txOut.publicKeyScript)
      safeSend(wait.fundingTx).foreach(Tools.log, _.printStackTrace)

    case (chan, _, SYNC, NORMAL) =>
      // Will be sent once on app lauch
      chan process CMDFeerate(feeRatePerKw)

    case (_, close: ClosingData, _, CLOSING)
      // This is an uncooperative close with HTLCs in-flight so we need to ask
      // server if any delayed HTLC has been spent to extract payment preimages
      if close.nextRemoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) ||
        close.remoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) ||
        close.localCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) =>

      LNParams.cloud.getTxs(close.commitments.commitInput.outPoint.txid.toString)
        .foreach(_ flatMap Helpers.extractPreimages foreach LNParams.bag.updatePreimage,
          _.printStackTrace)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      // Each time we get a command we send out commitment spending txs
      val toPublish = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ extractTxs(close)
      Obs.from(toPublish map safeSend).concat.foreach(Tools.log, _.printStackTrace)
  }

  override def onError = {
    case chanRelated: Throwable =>
      chanRelated.printStackTrace
  }
}