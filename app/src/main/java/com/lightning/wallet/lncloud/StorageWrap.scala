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
  def put(data: HasCommitments) = doPut(data.commitments.channelId.toString, data.toJson.toString)
  def get = RichCursor(db select ChannelTable.selectAllSql).vec(_ string ChannelTable.data) map to[HasCommitments]

  def doPut(chanId: String, data: String) = db txWrap {
    db.change(ChannelTable.newSql, params = chanId, data)
    db.change(ChannelTable.updSql, params = data, chanId)
  }

  override def onProcess = {
    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      db.change(ChannelTable.killSql, close.commitments.channelId.toString)
  }
}

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  // Incoming and outgoing payments are discerned by a presence of routing info
  // Incoming payments have null instead of routing info in a database

  import com.lightning.wallet.lncloud.PaymentInfoTable._
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath table, null)
  def byQuery(query: String): Cursor = db.select(searchSql, s"$query*")
  def recentPayments: Cursor = db select selectRecentSql

  def toPaymentInfo(rc: RichCursor) = {
    val delta = MilliSatoshi(rc long received)
    val pr = to[PaymentRequest](rc string request)

    Option(rc string routing) map to[RoutingData] match {
      case Some(rs) => OutgoingPayment(rs, rc string preimage, pr, delta, rc string chanId, rc int status)
      case _ => IncomingPayment(rc string preimage, pr, delta, rc string chanId, rc int status)
    }
  }

  def putPaymentInfo(info: PaymentInfo) = db txWrap {
    val paymentHashString = info.request.paymentHash.toString
    // OutgoingPayment delta is negative amount, IncomingPayment is zero, to be updated later
    val delta = info match { case out: OutgoingPayment => -out.request.finalSum.amount case _ => 0L }
    val routing = info match { case out: OutgoingPayment => out.routing.toJson.toString case _ => null }
    db.change(newVirtualSql, s"${info.request.description} $paymentHashString", paymentHashString)
    db.change(newSql, paymentHashString, info.request.toJson.toString, info.status.toString,
      info.chanId.toString, info.preimage.toString, delta.toString, routing)
  }

  def updateStatus(status: Int, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
  def updateReceived(add: UpdateAddHtlc) = db.change(updReceivedSql, add.amountMsat.toString, add.paymentHash.toString)
  def updateRouting(out: OutgoingPayment) = db.change(updRoutingSql, out.routing.toJson.toString, out.request.paymentHash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(selectByHashSql, hash.toString) headTry toPaymentInfo
  def failPending(status: Int, chanId: BinaryData) = db.change(failPendingSql, status.toString, chanId.toString)

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
    case (chan, norm: NormalData, sig: CommitSig) =>

      LNParams.db txWrap {
        // First we update status for failed, fulfilled and in-flight HTLCs
        for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(WAITING, htlc.add.paymentHash)
        for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
        for (htlc ~ _ <- norm.commitments.localCommit.spec.failed) updateStatus(FAILURE, htlc.add.paymentHash)
        uiNotify
      }

      for {
        // Then retry failed payments with routes left
        htlc ~ fail <- norm.commitments.localCommit.spec.failed
        out @ OutgoingPayment(_, _, request, _, _, _) <- getPaymentInfo(htlc.add.paymentHash)
        out1 <- app.ChannelManager.outPaymentOpt(cutRoutes(fail, out), request, chan)
      } chan process RetryAddHtlc(out1)

    case (_, close: ClosingData, _: Command) if close.isOutdated =>
      // This channel is outdated, fail all the unfinished HTLCs
      failPending(WAITING, close.commitments.channelId)
  }

  override def onBecome = {
    case (_, some: HasCommitments, NORMAL | SYNC, SYNC | NEGOTIATIONS | CLOSING) =>
      // At worst these will be marked as FAILURE and then as WAITING on their CommitSig
      failPending(TEMP, some.commitments.channelId)
      uiNotify
  }
}