package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._
import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.wallet.Utils.app

import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw = RatesSaver.rates.feeLive.value / 2
  def send(tx: Transaction) = app.kit.blockingSend(tx).toString

  def currentHeight =
    math.max(app.kit.wallet.getLastBlockSeenHeight,
      app.kit.peerGroup.getMostCommonChainHeight)

  def getConfirmations(txid: BinaryData) =
    Option(app.kit.wallet getTransaction txid)
      .map(_.getConfidence.getDepthInBlocks)

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, WAIT_FUNDING_DONE) =>
      val script = wait.commitments.commitInput.txOut.publicKeyScript
      val watchList = List(script: org.bitcoinj.script.Script).asJava
      safeSend(wait.fundingTx).foreach(Tools.log, Tools.errlog)
      app.kit.wallet.addWatchedScripts(watchList)

    case (chan, _, SYNC, NORMAL) =>
      // Will be sent once on app lauch
      chan process CMDFeerate(feeRatePerKw)

    case (_, close: ClosingData, _, CLOSING)
      // This is an uncooperative close with HTLCs in-flight so we need to ask
      // server if any delayed HTLC has been spent to extract payment preimages
      if close.nextRemoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) ||
        close.remoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) ||
        close.localCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) =>

      // We ask server if any HTLC has been pulled to obtain a preimage
      LNParams.cloud.getTxs(commit = close.commitments.commitInput.outPoint.txid.toString)
        .foreach(_ flatMap Helpers.extractPreimages foreach LNParams.bag.updatePreimage, Tools.errlog)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      // Each time we get spent/height command we send out all possible commitment spending txs
      val toPublish = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ extractTxs(close)
      Obs.from(toPublish map safeSend).concat.foreach(Tools.log, Tools.errlog)
  }
}