package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}

import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.LNParams.db
import com.lightning.wallet.Utils.app
import net.sqlcipher.Cursor
import scala.util.Try


object StorageWrap {
  def put(value: String, key: String) = db txWrap {
    db.change(StorageTable.newSql, params = key, value)
    db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String): Try[String] = {
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
  }
}

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  // Incoming and outgoing payments are discerned by presence of routing info
  // Incoming payments have null instead of routing info in a database

  import com.lightning.wallet.lncloud.PaymentInfoTable._
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

  def updateStatus(pre: Int, post: Int) = db.change(updStatusStatusSql, post.toString, pre.toString)
  def updateStatus(status: Int, hash: BinaryData) = db.change(updStatusHashSql, status.toString, hash.toString)
  def updateReceived(add: UpdateAddHtlc) = db.change(updReceivedSql, add.amountMsat.toString, add.paymentHash.toString)
  def updateRouting(out: OutgoingPayment) = db.change(updRoutingSql, out.routing.toJson.toString, out.request.paymentHash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(selectByHashSql, hash.toString) headTry toPaymentInfo

  override def onProcess = {
    case (_, _, add: UpdateAddHtlc) =>
      // Payment request may not contain an amount
      // or an actual amount paid may differ so
      // we need to record how much was paid
      me updateReceived add

    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage right away
      me updatePreimage fulfill
      uiNotify

    case (_, _, retry: RetryAddHtlc) =>
      // Update outgoing payment routing data
      // Fee is not shown so no need for UI changes
      me updateRouting retry.out

    case (_, _, cmd: CMDAddHtlc) =>
      // Try to record a new outgoing payment
      // fails if payment hash is already in db
      me putPaymentInfo cmd.out
      uiNotify

    // We need to update states for all active HTLCs
    case (chan, norm: NormalData, _: CommitSig) =>

      LNParams.db txWrap {
        // First we update status for failed, fulfilled and in-flight HTLCs
        for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(WAITING, htlc.add.paymentHash)
        for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
        for (htlc \ _ <- norm.commitments.localCommit.spec.failed) updateStatus(FAILURE, htlc.add.paymentHash)
        uiNotify
      }

      for {
        (htlc, fail: UpdateFailHtlc) <- norm.commitments.localCommit.spec.failed
        // UpdateFailMalformedHtlc this is a corner case, it can only happen when the *first* node cannot parse the onion
        // (if this happens higher up in the route, the error would be wrapped in an UpdateFailHtlc and handled above)
        // we now retry all outgoing UpdateFailHtlc which have routes left and UpdateFailMalformedHtlc is left failed
        out @ OutgoingPayment(_, _, request, _, _) <- getPaymentInfo(htlc.add.paymentHash)
        out1 <- app.ChannelManager.outPaymentOpt(cutRoutes(fail, out), request, chan)
      } chan process RetryAddHtlc(out1)
  }

  override def onBecome = {
    case (_, some: HasCommitments, NORMAL, SYNC | NEGOTIATIONS) =>
      // At worst will be marked as FAILURE and then as WAITING
      updateStatus(TEMP, FAILURE)
      uiNotify

    case (_, some: HasCommitments, NORMAL | SYNC | NEGOTIATIONS, CLOSING) =>
      // At worst WAITING will be REFUND and then SUCCESS if we get a preimage
      updateStatus(WAITING, REFUND)
      updateStatus(TEMP, FAILURE)
  }
}