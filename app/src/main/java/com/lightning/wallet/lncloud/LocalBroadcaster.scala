package com.lightning.wallet.lncloud

import spray.json._
import DefaultJsonProtocol._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._
import collection.JavaConverters.seqAsJavaListConverter
import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.Utils.app


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw = RatesSaver.rates.feeLive.value / 2
  def send(tx: Transaction) = app.kit.blockingSend(tx).toString

  def currentHeight =
    math.max(app.kit.wallet.getLastBlockSeenHeight,
      app.kit.peerGroup.getMostCommonChainHeight)

  def getConfirmations(txid: BinaryData) =
    Option(app.kit.wallet getTransaction txid)
      .map(_.getConfidence.getDepthInBlocks)

  def watchScript(cs: Commitments) = {
    val script = cs.commitInput.txOut.publicKeyScript
    val watch = List(script: org.bitcoinj.script.Script)
    app.kit.wallet addWatchedScripts watch.asJava
  }

  override def onBecome = {
    case (_, wait: WaitFundingDoneData, _, WAIT_FUNDING_DONE) =>
      safeSend(wait.fundingTx).foreach(Tools.log, Tools.errlog)
      watchScript(wait.commitments)

    case (_, recovery: RefundingData, _, REFUNDING) =>
      // Will be called multiple times but it's fine
      watchScript(recovery.commitments)

    case (chan, _, SYNC, NORMAL) =>
      // Will be sent once on app lauch
      chan process CMDFeerate(feeRatePerKw)

    case (_, close: ClosingData, _, CLOSING)
      // This is an uncooperative close with HTLCs in-flight so we need to ask
      // server if any HTLC output has been spent to extract payment preimages
      if close.nextRemoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) ||
        close.remoteCommit.exists(_.claimHtlcTimeoutTxs.nonEmpty) =>

      val remoteCommitTxid = close.commitments.remoteCommit.txid.toString
      val remoteNextCommitTxid = close.commitments.remoteNextCommitInfo.left.toSeq.map(_.nextRemoteCommit.txid.toString)
      LNParams.cloud.connector.getTxs(txids = (remoteNextCommitTxid :+ remoteCommitTxid).toJson.toString.hex)
        .foreach(_ flatMap Helpers.extractPreimages foreach LNParams.bag.updatePreimage, Tools.errlog)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      // Each time we get spent/height command we send out all possible commitment spending txs
      val toPublish = close.mutualClose ++ close.localCommit.map(_.commitTx) ++ close.txs
      Obs.from(toPublish map safeSend).concat.foreach(Tools.log, Tools.errlog)
  }
}