package com.lightning.wallet.lncloud

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.BinaryData
import net.sqlcipher.Cursor
import scala.util.Try


object StorageWrap {
  def put(value: String, key: String) = LNParams.db txWrap {
    LNParams.db.change(sql = StorageTable.newSql, params = key, value)
    LNParams.db.change(sql = StorageTable.updSql, params = value, key)
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

object PaymentSpecWrap extends PaymentSpecBag { me =>
  import com.lightning.wallet.lncloud.PaymentSpecTable._

  def recentPayments: Cursor = LNParams.db select selectRecentSql
  def byQuery(query: String): Cursor = LNParams.db.select(searchSql, s"$query*")
  def byHash(hash: BinaryData): Cursor = LNParams.db.select(selectByHashSql, hash.toString)
  def getDataByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = RichCursor(me byHash hash) headTry toInfo
  def uiNotify = app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)

  def putData(info: ExtendedPaymentInfo) = LNParams.db txWrap {
    val data ~ hashString = (info.spec.toJson.toString, info.spec.request.paymentHash.toString)
    LNParams.db.change(newSql, hashString, info.progress.toString, info.status.toString, info.chanId.toString, data)
    LNParams.db.change(newVirtualSql, params = s"${info.spec.request.description} $hashString", hashString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def toInfo(rc: RichCursor) = ExtendedPaymentInfo(to[PaymentSpec](rc string data), rc long progress, rc long status, rc string channel)
  def updateProgress(hash: BinaryData, progress: Long) = LNParams.db.change(updProgressSql, progress.toString, hash.toString)
  def updateStatus(hash: BinaryData, status: Long) = LNParams.db.change(updStatusSql, status.toString, hash.toString)

  def updateData(spec: OutgoingPaymentSpec) = {
    val hashString = spec.request.paymentHash.toString
    LNParams.db.change(updDataSql, spec.toJson.toString, hashString)
    uiNotify
  }
}