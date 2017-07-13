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

  def byStatus: Cursor = LNParams.db select selectRecentSql
  def byQuery(query: String): Cursor = LNParams.db.select(searchSql, s"$query*")
  def byHash(hash: BinaryData): Cursor = LNParams.db.select(selectByHashSql, hash.toString)
  def toInfo(rcu: RichCursor) = ExtendedPaymentInfo(to[PaymentSpec](rcu string data), rcu long status)
  def getInfoByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = RichCursor(me byHash hash) headTry toInfo

  def updatePaymentStatus(hash: BinaryData, status: Long) = {
    LNParams.db.change(updStatusSql, status.toString, hash.toString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def updateInfo(spec: OutgoingPaymentSpec) = {
    val hashString = spec.request.paymentHash.toString
    LNParams.db.change(updDataSql, spec.toJson.toString, hashString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }

  def putInfo(info: ExtendedPaymentInfo) = {
    val hashString = info.spec.request.paymentHash.toString
    LNParams.db.change(newSql, info.spec.toJson.toString, hashString, info.status.toString)
    LNParams.db.change(newVirtualSql, s"${info.spec.request.description} $hashString", hashString)
    app.getContentResolver.notifyChange(LNParams.db sqlPath table, null)
  }
}