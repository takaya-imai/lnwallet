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
    LNParams.db.change(sql = StorageTable.newSql, params = value, key)
    LNParams.db.change(sql = StorageTable.updSql, params = value, key)
  }

  def get(key: String): Try[String] = {
    val cursor = LNParams.db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }
}

object PaymentSpecWrap extends PaymentSpecBag { me =>
  import com.lightning.wallet.lncloud.PaymentSpecTable._
  def byQuery(query: String): Cursor = LNParams.db.select(selectVirtualSql, s"$query*")
  def byHash(hash: BinaryData): Cursor = LNParams.db.select(selectByHashSql, hash.toString)
  def byTime(millis: Long): Cursor = LNParams.db.select(selectRecentSql, (System.currentTimeMillis - millis).toString)
  def toInfo(rcu: RichCursor) = ExtendedPaymentInfo(to[PaymentSpec](rcu string data), rcu long status, rcu long stamp)
  def getInfoByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = RichCursor(me byHash hash) headTry toInfo

  def updatePaymentStatus(hash: BinaryData, status: Long): Unit = {
    LNParams.db.change(sql = updStatusSql, status.toString, hash.toString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def updateOutgoingPaymentSpec(spec: OutgoingPaymentSpec): Unit =
    LNParams.db.change(updDataSql, spec.toJson.toString,
      spec.request.paymentHash.toString)

  def putInfo(info: ExtendedPaymentInfo) = info.spec.request.paymentHash.toString match { case hashStr =>
    LNParams.db.change(newSql, info.spec.toJson.toString, hashStr, info.status.toString, info.stamp.toString)
    LNParams.db.change(newVirtualSql, s"${info.spec.request.descriptionOpt.orNull} $hashStr", hashStr)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }
}