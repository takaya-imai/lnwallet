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
  def put(data: HasCommitments): Unit = db txWrap {
    val chanIdString = data.commitments.channelId.toString
    val content = data.toJson.toString

    db.change(ChannelTable.newSql, params = chanIdString, content)
    db.change(ChannelTable.updSql, params = content, chanIdString)
  }

  def get: Vector[HasCommitments] =
    RichCursor(db select ChannelTable.selectAllSql)
      .vec(_ string ChannelTable.data) map to[HasCommitments]

  override def onProcess = {
    case (_, close: ClosingData, _: Command) if close.isOutdated =>
      // This channel is outdated, remove it so we don't have it on next launch
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
    val actual = MilliSatoshi(rc long received)
    val pr = to[PaymentRequest](rc string request)
    Option(rc string routing) map to[RoutingData] match {
      case Some(rs) => OutgoingPayment(rs, rc string preimage, pr, actual, rc string chanId, rc long status)
      case _ => IncomingPayment(rc string preimage, pr, actual, rc string chanId, rc long status)
    }
  }

  def putPaymentInfo(info: PaymentInfo) = db txWrap {
    val paymentHashString = info.request.paymentHash.toString
    // OutgoingPayment has received set to negative amount, IncomingPayment is zero, to be updated later
    val received1 = info match { case o: OutgoingPayment => -o.request.amount.get.amount case _ => 0L }
    val routing = info match { case o: OutgoingPayment => o.routing.toJson.toString case _ => null }
    db.change(newVirtualSql, s"${info.request.description} $paymentHashString", paymentHashString)
    db.change(newSql, paymentHashString, info.request.toJson.toString, info.status.toString,
      info.chanId.toString, info.preimage.toString, received1.toString, routing)
  }

  def updateStatus(status: Long, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
  def updateReceived(add: UpdateAddHtlc) = db.change(updReceivedSql, add.amountMsat.toString, add.paymentHash.toString)
  def updateRouting(out: OutgoingPayment) = db.change(updRoutingSql, out.routing.toJson.toString, out.request.paymentHash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(selectByHashSql, hash.toString) headTry toPaymentInfo
  def failPending(status: Long, chanId: BinaryData) = db.change(failPendingSql, status.toString, chanId.toString)

  def retry(channel: Channel, fail: UpdateFailHtlc, outgoingPayment: OutgoingPayment) =
    channel.outPaymentOpt(cutRoutes(fail, outgoingPayment), outgoingPayment.request) match {
      case Some(updatedOutgoingPayment) => channel process RetryAddHtlc(updatedOutgoingPayment)
      case None => updateStatus(FAILURE, outgoingPayment.request.paymentHash)
    }

  override def onProcess = {
    case (_, _, retry: RetryAddHtlc) =>
      // Update outgoing payment routing data
      // Fee is not shown so no need for UI changes
      me updateRouting retry.out

    case (_, _, cmd: CMDAddHtlc) =>
      // Try to record a new outgoing payment
      // fails if payment hash is already in db
      LNParams.db txWrap putPaymentInfo(cmd.out)
      uiNotify

    case (_, _, add: UpdateAddHtlc) =>
      // Payment request may not contain an amount
      // or an actual amount paid may differ so
      // we need to record how much was paid
      me updateReceived add

    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage right away
      me updatePreimage fulfill
      uiNotify

    case (chan, norm: NormalData, sig: CommitSig) => LNParams.db txWrap {
      for (htlc ~ fail <- norm.commitments.localCommit.spec.failed) getPaymentInfo(htlc.add.paymentHash) foreach {
        case outgoingPayment: OutgoingPayment if outgoingPayment.request.isFresh => retry(chan, fail, outgoingPayment)
        case _ => updateStatus(FAILURE, htlc.add.paymentHash)
      }

      for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
      for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(WAITING, htlc.add.paymentHash)
      uiNotify
    }

    case (_, close: ClosingData, _: Command) if close.isOutdated =>
      // This channel is outdated, fail all unfinished HTLCs
      failPending(WAITING, close.commitments.channelId)
      uiNotify
  }

  override def onError = {
    case ExtendedException(cmd: RetryAddHtlc) =>
      updateStatus(FAILURE, cmd.out.request.paymentHash)
      uiNotify
  }

  override def onBecome = {
    case (_, norm: NormalData, _, SYNC | CLOSING) =>
      // At worst these will be marked as FAILURE and
      // then as WAITING once their CommitSig arrives
      failPending(TEMP, norm.commitments.channelId)
      uiNotify
  }
}