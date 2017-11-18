package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.Connector.CMDStart
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.helper.AES
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.MilliSatoshi
import fr.acinq.bitcoin.BinaryData
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

    case (_, norm: NormalData, _: CommitSig)
      // We should remind cloud to maybe send a scheduled data
      if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
      cloud doProcess CMDStart
  }

  override def onBecome = {
    case (_, norm: NormalData, WAIT_FUNDING_DONE, NORMAL) =>
      // Once a new channel becomes NORMAL we save it's state on a cloud
      val staticChannelState = RefundingData(norm.announce, norm.commitments)
      val packed = AES.encode(staticChannelState.toJson.toString, cloudSecret)
      cloud doProcess CloudAct(packed, Seq("key" -> cloudId.toString), "data/put")
  }
}

import com.lightning.wallet.lnutils.PaymentInfoTable._
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

  def upsertPaymentInfo(info: PaymentInfo) = db txWrap {
    val paymentHashString = info.request.paymentHash.toString
    val received = info match { case in: IncomingPayment => in.received.amount.toString case _ => null }
    val routing = info match { case out: OutgoingPayment => out.routing.toJson.toString case _ => null }
    db.change(newVirtualSql, s"${info.request.description} $paymentHashString", paymentHashString)
    db.change(newSql, paymentHashString, info.request.toJson.toString, info.status.toString,
      info.chanId.toString, info.preimage.toString, received, routing)
  }

  def updateStatus(status: Int, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
  def updateReceived(add: UpdateAddHtlc) = db.change(updReceivedSql, add.amountMsat.toString, add.paymentHash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(selectByHashSql, hash.toString) headTry toPaymentInfo

  def retry(chan: Channel, hash: BinaryData, fail: UpdateFailHtlc) = for {
    outgoing @ OutgoingPayment(routing, _, pr, _, _) <- getPaymentInfo(hash)
    routing1 = cutRoutes(fail, routing, pr.nodeId)
  } buildPayment(routing1, pr, chan) match {

    case Some(outgoing1) =>
      // Accepted: routing info will be updated in onProcess
      // Not accepted: status will change to FAILURE in onError
      chan process PlainAddHtlc(outgoing1)

    case None =>
      // uiNotify will be called below a bit later
      // We may have updated bad nodes or bad channels in routing1
      // Should save them anyway in case of user initiated payment retry
      me upsertPaymentInfo outgoing.copy(routing = routing1, status = FAILURE)
  }

  override def onError = {
    case (_, ex: CMDException) =>
      // Useless most of the time but needed for retry add failures
      // CMDException should still be used or channel may be closed
      updateStatus(FAILURE, ex.cmd.out.request.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, add: UpdateAddHtlc) =>
      // Actual incoming amount paid may be different
      // We need to record exactly how much was paid
      me updateReceived add

    case (_, _: NormalData, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage right away
      me updatePreimage fulfill

    case (_, _: NormalData, cmd: CMDAddHtlc) =>
      // Channel has accepted this payment, now we have to save it
      // Using REPLACE instead of INSERT in SQL to update duplicates
      me upsertPaymentInfo cmd.out
      uiNotify

    case (chan, norm: NormalData, _: CommitSig) =>
      // fulfilled: update as SUCCESS in a database
      // failed: try to re-send or update as FAILURE
      // htlcs: no need to update

      db txWrap {
        for (htlc <- norm.commitments.localCommit.spec.fulfilled)
          updateStatus(SUCCESS, htlc.add.paymentHash)

        for (htlc \ fail <- norm.commitments.localCommit.spec.failed) fail match {
          case _: UpdateFailMalformedHtlc => updateStatus(FAILURE, htlc.add.paymentHash)
          case fail: UpdateFailHtlc => retry(chan, htlc.add.paymentHash, fail)
        }
      }

      uiNotify
  }

  override def onBecome = {
    case (_, _, SYNC | NORMAL | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      db change updFailWaitingSql
  }
}