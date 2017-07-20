package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.softwaremill.quicklens._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.PaymentSpec._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import com.lightning.wallet.ln.wire.{CommitSig, UpdateFulfillHtlc}
import fr.acinq.bitcoin.BinaryData
import net.sqlcipher.Cursor

import scala.util.Try


object StorageWrap {
  def put(value: String, key: String) = LNParams.db txWrap {
    LNParams.db.change(StorageTable.newSql, params = key, value)
    LNParams.db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String): Try[String] = {
    val cursor = LNParams.db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }
}

object ChannelWrap {
  import com.lightning.wallet.lncloud.ChannelTable._
  def get = RichCursor(LNParams.db select selectAllSql) vec {
    shiftedCursor => to[HasCommitments](shiftedCursor string data)
  }

  def remove(chanId: BinaryData) =
    LNParams.db.change(killSql, chanId.toString)

  def put(data: HasCommitments) = LNParams.db txWrap {
    val chanIdString = data.commitments.channelId.toString
    val content = data.toJson.toString

    LNParams.db.change(newSql, chanIdString, content)
    LNParams.db.change(updSql, content, chanIdString)
  }
}

object PaymentSpecWrap extends PaymentSpecBag with ChannelListener { me =>
  import com.lightning.wallet.lncloud.PaymentSpecTable._

  def recentPayments: Cursor = LNParams.db select selectRecentSql
  def byQuery(query: String): Cursor = LNParams.db.select(searchSql, s"$query*")
  def uiNotify = app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  def toInfo(rc: RichCursor) = ExtendedPaymentInfo(spec = to[PaymentSpec](rc string data),
    status = rc long status, chanId = rc string chanId)

  def getDataByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = {
    val cursor = LNParams.db.select(selectByHashSql, params = hash.toString)
    RichCursor(cursor) headTry toInfo
  }

  def putData(info: ExtendedPaymentInfo): Unit = LNParams.db txWrap {
    val data ~ hashString = (info.spec.toJson.toString, info.spec.request.paymentHash.toString)
    LNParams.db.change(newVirtualSql, s"${info.spec.request.description} $hashString", hashString)
    LNParams.db.change(newSql, hashString, info.status.toString, info.chanId.toString, data)
    uiNotify
  }

  def updateStatus(hash: BinaryData, status: Long) = LNParams.db.change(updStatusSql, status.toString, hash.toString)
  def failStatuses(chanId: BinaryData, where: Long) = LNParams.db.change(failStatusSql, chanId.toString, where.toString)
  def updateData(spec: OutgoingPaymentSpec) = LNParams.db.change(updDataSql, spec.toJson.toString, spec.request.paymentHash.toString)

  def setPreimage(fulfill: UpdateFulfillHtlc) =
    me getDataByHash fulfill.paymentHash map {

  override def onProcess = {
    case (_, fulfill: UpdateFulfillHtlc) =>
      me getDataByHash fulfill.paymentHash map {
        case ex @ ExtendedPaymentInfo(spec: OutgoingPaymentSpec, _, _) =>
          val spec1 = spec.modify(_.preimage) setTo Some(fulfill.paymentPreimage)
          me putData ex.copy(spec = spec1)

        case otherwise =>
          // No such payment or incoming spec etc
          Tools log s"Found $otherwise for $fulfill"
      }

    case (norm: NormalData, sig: CommitSig) => LNParams.db txWrap {
      // After each incoming commit sig we have finalized payments, should update UI accordingly
      for (htlc <- norm.commitments.localCommit.spec.fulfilled) updateStatus(htlc.add.paymentHash, SUCCESS)
      for (htlc <- norm.commitments.localCommit.spec.failed) updateStatus(htlc.add.paymentHash, FAILURE)
      for (htlc <- norm.commitments.localCommit.spec.htlcs) updateStatus(htlc.add.paymentHash, WAITING)
      uiNotify
    }
  }

  override def onBecome = {
    case (_, norm: NormalData, _, SYNC | CLOSING) =>
      failStatuses(norm.commitments.channelId, where = TEMP)
  }
}