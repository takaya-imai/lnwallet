package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._

import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.ln.LNParams.db
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
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

object ChannelWrap {
  import com.lightning.wallet.lncloud.ChannelTable._
  def remove(channelId: BinaryData) = db.change(killSql, channelId.toString)
  def get = RichCursor(db select selectAllSql).vec(_ string data) map to[HasCommitments]

  def put(data: HasCommitments): Unit = db txWrap {
    val chanIdString = data.commitments.channelId.toString
    val content = data.toJson.toString

    db.change(newSql, chanIdString, content)
    db.change(updSql, content, chanIdString)
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
    val pr = to[PaymentRequest](rc string request)
    Option(rc string routing) map to[RoutingData] match {
      case Some(rs) => OutgoingPayment(rs, rc string preimage, pr, rc string chanId, rc long status)
      case None => IncomingPayment(rc string preimage, pr, rc string chanId, rc long status)
    }
  }

  def putPaymentInfo(info: PaymentInfo) = db txWrap {
    val hashString = info.request.paymentHash.toString
    val requestString = info.request.toJson.toString

    info match {
      case out: OutgoingPayment =>
        db.change(newSql, hashString, requestString, info.status.toString,
          info.chanId.toString, info.preimage.toString, out.routing.toJson.toString)

      case in: IncomingPayment =>
        db.change(newSql, hashString, requestString, info.status.toString,
          info.chanId.toString, info.preimage.toString, null)
    }

    val searchKeys = s"${info.request.description} $hashString"
    db.change(newVirtualSql, searchKeys, hashString)
    uiNotify
  }

  def updateStatus(status: Long, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
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
      // Update existing payment routing
      me updateRouting retry.out
      uiNotify

    case (_, _, cmd: CMDAddHtlc) =>
      // Record new outgoing payment
      me putPaymentInfo cmd.out
      uiNotify

    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage right away
      me updatePreimage fulfill

    case (chan, norm: NormalData, sig: CommitSig) => LNParams.db txWrap {
      for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(WAITING, htlc.add.paymentHash)
      for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
      for (htlc ~ fail <- norm.commitments.localCommit.spec.failed) getPaymentInfo(htlc.add.paymentHash) foreach {
        case outgoingPayment: OutgoingPayment if outgoingPayment.request.isFresh => retry(chan, fail, outgoingPayment)
        case _ => updateStatus(FAILURE, htlc.add.paymentHash)
      }

      uiNotify
    }
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