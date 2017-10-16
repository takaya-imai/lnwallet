package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}

import com.lightning.wallet.lncloud.Connector.CMDStart
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.helper.AES
import com.lightning.wallet.Utils.app
import com.lightning.wallet.Vibr
import net.sqlcipher.Cursor


object StorageWrap {
  def put(value: String, key: String) = db txWrap {
    db.change(StorageTable.newSql, params = key, value)
    db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String) = {
    val cursor = db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }
}

object ChannelWrap extends ChannelListener {
  def doPut(chanId: String, data: String) = db txWrap {
    db.change(ChannelTable.newSql, params = chanId, data)
    db.change(ChannelTable.updSql, params = data, chanId)
  }

  def put(data: HasCommitments) = {
    val chanId = data.commitments.channelId
    doPut(chanId.toString, data.toJson.toString)
  }

  def get = {
    val rc = RichCursor(db select ChannelTable.selectAllSql)
    rc.vec(_ string ChannelTable.data) map to[HasCommitments]
  }

  override def onProcess = {
    case (_, close: EndingData, _: CMDBestHeight) if close.isOutdated =>
      db.change(ChannelTable.killSql, close.commitments.channelId.toString)

    case (_, _: NormalData, cmd: PlainAddHtlc) =>
      // Vibrate to show payment is in progress
      Vibr vibrate Vibr.processed

    case (_, _: NormalData, _: NormalData) =>
      // This happens when channel with null data gets NormalData on app start,
      // we re-send CMDStart just in case if we have any pending cloud actions
      cloud doProcess CMDStart

    case (_, norm: NormalData, _: CommitSig)
      // GUARD: this may be a storage token HTLC
      if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
      // We should remind cloud to maybe send a scheduled data
      Vibr vibrate Vibr.confirmed
      cloud doProcess CMDStart
  }

  override def onBecome = {
    case (_, norm: NormalData, WAIT_FUNDING_DONE, NORMAL) =>
      // Once a new channel becomes NORMAL we save it's state on a cloud
      val staticChannelState = RefundingData(norm.announce, norm.commitments)
      val packed = AES.encode(staticChannelState.toJson.toString, cloudPrivateId)
      cloud doProcess CloudAct(packed, Seq("key" -> cloudPublicId.toString), "data/put")
  }
}

import com.lightning.wallet.lncloud.PaymentInfoTable._
object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  // Incoming and outgoing payments are discerned by presence of routing info
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath table, null)
  def byQuery(query: String): Cursor = db.select(searchSql, s"$query*")
  def recentPayments: Cursor = db select selectRecentSql

  def toPaymentInfo(rc: RichCursor) =
    Option(rc string routing) map to[RoutingData] match {
      case Some(routes) => OutgoingPayment(routes, rc string preimage,
        to[PaymentRequest](rc string request), rc string chanId, rc int status)

      case None => IncomingPayment(MilliSatoshi(rc long received), rc string preimage,
        to[PaymentRequest](rc string request), rc string chanId, rc int status)
    }

  def putPaymentInfo(info: PaymentInfo) = db txWrap {
    val paymentHashString = info.request.paymentHash.toString
    val received = info match { case in: IncomingPayment => in.received.amount.toString case _ => null }
    val routing = info match { case out: OutgoingPayment => out.routing.toJson.toString case _ => null }
    db.change(newVirtualSql, s"${info.request.description} $paymentHashString", paymentHashString)
    db.change(newSql, paymentHashString, info.request.toJson.toString, info.status.toString,
      info.chanId.toString, info.preimage.toString, received, routing)
  }

  def getPaymentInfo(hash: BinaryData) = {
    val cursor = db.select(selectByHashSql, hash.toString)
    RichCursor apply cursor headTry toPaymentInfo
  }

  def updateFailWaiting = db.change(updFailWaitingSql)
  def updateStatus(status: Int, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
  def updateReceived(add: UpdateAddHtlc) = db.change(updReceivedSql, add.amountMsat.toString, add.paymentHash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)

  override def onError = {
    case _ \ AddException(cmd: CMDAddHtlc, _) =>
      // Useless most of the time but needed for retry failures
      // CMDAddHtlc should still be used or channel may be closed
      updateStatus(FAILURE, cmd.out.request.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _, add: UpdateAddHtlc) =>
      // Payment request may not contain an amount
      // Or an actual incoming amount paid may differ
      // We need to record exactly how much was paid
      me updateReceived add

    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage right away
      me updatePreimage fulfill

    case (_, _, cmd: CMDAddHtlc) =>
      // Channel has accepted this payment, now we have to save it
      // Using REPLACE instead of INSERT in SQL to update duplicates
      me putPaymentInfo cmd.out

    case (chan, norm: NormalData, _: CommitSig) =>
      // On each incoming CommitSig we have HTLC updates
      // which have to be reflected on the UI

      db txWrap {
        // First we update status for committed, fulfilled and failed HTLCs
        for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
        for (Tuple2(htlc, _: UpdateFailMalformedHtlc) <- norm.commitments.localCommit.spec.failed)
          updateStatus(FAILURE, htlc.add.paymentHash)
      }

      for {
        // Then we try to re-send failed payments
        Tuple2(htlc, updateFailHtlc: UpdateFailHtlc) <- norm.commitments.localCommit.spec.failed
        outgoing @ OutgoingPayment(routing, _, pr, _, _) <- getPaymentInfo(htlc.add.paymentHash)
        routing1 = cutRoutes(updateFailHtlc, routing, pr.nodeId)
      } buildPayment(routing1, pr, chan) match {

        case Some(outgoing1) =>
          // Accepted: routing info updates in onProcess
          // Not accepted: status changes to FAILURE in onError
          chan process PlainAddHtlc(outgoing1)

        case None =>
          // We may have updated bad nodes or bad channels in routing1
          // Should save them anyway in case of user initiated payment retry
          me putPaymentInfo outgoing.copy(routing = routing1, status = FAILURE)
      }

      uiNotify
  }
}