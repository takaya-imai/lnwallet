package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import spray.json.DefaultJsonProtocol._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import com.lightning.wallet.ln.Helpers.extractPreimages
import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Sha256Hash

import fr.acinq.bitcoin.{BinaryData, Transaction}
import rx.lang.scala.{Observable => Obs}


object LocalBroadcaster extends Broadcaster { me =>
  def feeRatePerKw: Long = RatesSaver.rates.feeLive.value / 2
  def send(tx: Transaction): String = app.kit.blockingSend(tx).toString

  // Confirmations and confidence type
  def txStatus(txid: Sha256Hash) = for {
    tx <- Option(app.kit.wallet getTransaction txid)
    isDead = tx.getConfidence.getConfidenceType == DEAD
    depth = tx.getConfidence.getDepthInBlocks
  } yield depth.toLong -> isDead

  def txStatus(txid: BinaryData): Option[DepthAndAlive] =
    txStatus(Sha256Hash wrap txid.toArray)

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

    case (_, close: ClosingData, NORMAL | SYNC | NEGOTIATIONS, CLOSING)
      // This is an uncooperative close with outgoing HTLCs in-flight so we need to ask
      // a server if any such HTLC output has been spent to extract payment preimages
      if close.nextRemoteCommit.exists(_.claimHtlcTimeout.nonEmpty) ||
        close.remoteCommit.exists(_.claimHtlcTimeout.nonEmpty) ||
        close.localCommit.exists(_.claimHtlcTimeout.nonEmpty) =>

      val txids: String = close.allCommits.map(_.txid).toJson.toString.hex
      app.kit.watchScripts(close.allCommits.flatMap(_.txOut).map(_.publicKeyScript) map bitcoinLibScript2bitcoinjScript)
      LNParams.cloud.connector.getTxs(txids).foreach(_ flatMap extractPreimages foreach LNParams.bag.updatePreimage, Tools.errlog)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: Command) =>
      // Each time we get spent/height command we send out all possible spending txs
      Obs.from(close.allTransactions map safeSend).concat.foreach(Tools.log, Tools.errlog)
  }
}