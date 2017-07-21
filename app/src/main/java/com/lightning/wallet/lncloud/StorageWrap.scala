package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.ln.wire.{CommitSig, UpdateFulfillHtlc}

import com.lightning.wallet.helper.RichCursor
import fr.acinq.eclair.payment.PaymentRequest
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
  def remove(chanId: BinaryData) = db.change(killSql, chanId.toString)
  def get = RichCursor(db select selectAllSql).vec(_ string data) map to[HasCommitments]

  def put(data: HasCommitments): Unit = db txWrap {
    val chanIdString = data.commitments.channelId.toString
    val content = data.toJson.toString

    db.change(newSql, chanIdString, content)
    db.change(updSql, content, chanIdString)
  }
}

object PaymentInfoWrap
extends PaymentInfoBag
with ChannelListener { me =>

  import com.lightning.wallet.lncloud.PaymentInfoTable._

  def recentPayments: Cursor = db select selectRecentSql
  def byQuery(query: String): Cursor = db.select(searchSql, s"$query*")
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath table, null)

  def toPaymentInfo(rc: RichCursor) = Option(rc string routing) map to[RoutingData] match {
    case Some(routes) => OutgoingPayment(routes, rc string preimage, to[PaymentRequest](rc string request), rc string chanId, rc long status)
    case None => IncomingPayment(rc string preimage, to[PaymentRequest](rc string request), rc string chanId, rc long status)
  }

  def putPaymentInfo(info: PaymentInfo) = db txWrap {
    val hashString = info.request.paymentHash.toString
    val requestString = info.request.toJson.toString
    val preimageString = info.preimage.toString
    val statusString = info.status.toString
    val chanIdString = info.chanId.toString

    info match {
      case out: OutgoingPayment =>
        db.change(newSql, hashString, requestString, statusString,
          chanIdString, preimageString, out.routing.toJson.toString)

      case in: IncomingPayment =>
        db.change(newSql, hashString, requestString,
          statusString, chanIdString, preimageString, null)
    }

    val searchKeys = s"${info.request.description} $hashString"
    db.change(newVirtualSql, searchKeys, hashString)
    uiNotify
  }

  def updateStatus(status: Long, hash: BinaryData) = db.change(updStatusSql, status.toString, hash.toString)
  def updateRouting(routing: RoutingData, hash: BinaryData) = db.change(updRoutingSql, routing.toJson.toString, hash.toString)
  def updatePreimage(upd: UpdateFulfillHtlc) = db.change(updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(selectByHashSql, hash.toString) headTry toPaymentInfo
  def failPending(status: Long, chanId: BinaryData) = db.change(failPendingSql, status.toString, chanId.toString)

  override def onProcess = {
    case (_, _, fulfill: UpdateFulfillHtlc) =>
      // We need to save it right away
      me updatePreimage fulfill

    case (_, norm: NormalData, sig: CommitSig) => LNParams.db txWrap {
      // We have finalized payments so should update UI accordingly, failed will be dealt with elsewhere
      for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(SUCCESS, htlc.add.paymentHash)
      for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(WAITING, htlc.add.paymentHash)
      uiNotify
    }
  }

  override def onBecome = {
    case (_, norm: NormalData, _, SYNC | CLOSING) =>
      failPending(TEMP, norm.commitments.channelId)
  }
}