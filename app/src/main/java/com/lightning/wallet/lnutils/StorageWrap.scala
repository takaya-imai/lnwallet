package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.Announcements._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import com.lightning.wallet.lnutils.Connector.CMDStart
import com.lightning.wallet.lnutils.JsonHttpUtils.to
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.Utils.app
import scala.collection.mutable
import net.sqlcipher.Cursor


object StorageWrap extends ChannelListener {
  def put(value: String, key: String) = db txWrap {
    db.change(StorageTable.newSql, params = key, value)
    db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String) = {
    val cursor = db.select(StorageTable.selectSql, key)
    RichCursor(cursor).headTry(_ string StorageTable.value)
  }

  private def ourUpdate(norm: NormalData, upd: ChannelUpdate) = {
    val (_, _, outputIndex) = Tools.fromShortId(upd.shortChannelId)
    val idxMatch = norm.commitments.commitInput.outPoint.index == outputIndex
    idxMatch && isEnabled(upd.flags) && checkSig(upd, norm.announce.nodeId)
  }

  override def onProcess = {
    case (chan, norm: NormalData, upd: ChannelUpdate) if ourUpdate(norm, upd) =>
      // Store update in a database and process no further updates afterwards
      put(upd.toJson.toString, norm.commitments.channelId.toString)
      chan.listeners -= StorageWrap
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
    case (_, close: ClosingData, _: CMDBestHeight) if close.isOutdated =>
      // Mutual tx has enough confirmations or hard timeout has passed out
      db.change(ChannelTable.killSql, close.commitments.channelId)

    case (_, norm: NormalData, _: CommitSig)
      // GUARD: this may be a storage token HTLC
      if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
      // We should remind cloud to maybe send a scheduled data
      cloud doProcess CMDStart
  }

  override def onBecome = {
    case (_, NormalData(_, _, None, None), WAIT_FUNDING_DONE | SYNC, NORMAL) =>
      // We may need to send an LN payment in -> NORMAL unless it is sutting down
      cloud doProcess CMDStart
  }
}

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener {
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentTable.selectSql, hash) headTry toPaymentInfo
  def updateRouting(status: Int, rpi: RuntimePaymentInfo) = db.change(PaymentTable.updRoutingSql, status, rpi.rd.toJson, rpi.pr.paymentHash)
  def updOkIncoming(m: UpdateAddHtlc) = db.change(PaymentTable.updOkIncomingSql, m.amountMsat, System.currentTimeMillis, m.paymentHash)
  def updOkOutgoing(m: UpdateFulfillHtlc) = db.change(PaymentTable.updOkOutgoingSql, m.paymentPreimage, m.paymentHash)
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentTable.table, null)
  def byQuery(query: String) = db.select(PaymentTable.searchSql, s"$query*")
  def byRecent = db select PaymentTable.selectRecentSql

  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rawRd = rc string PaymentTable.rd, rawPr = rc string PaymentTable.pr,
    preimage = rc string PaymentTable.preimage, incoming = rc int PaymentTable.incoming, finalSum = rc long PaymentTable.sum,
    status = rc int PaymentTable.status, stamp = rc long PaymentTable.stamp, text = rc string PaymentTable.text)

  // Records a number of retry attempts for a given outgoing payment hash
  val serverCallAttempts = mutable.Map.empty[BinaryData, Int] withDefaultValue 0

  override def onError = {
    case (_, exc: CMDException) =>
      // Is needed for retry failures
      // Also prevents a channel shutdown
      updateRouting(FAILURE, exc.cmd.rpi)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, cmd: CMDAddHtlc) =>
      // This may either be a new payment or an old payment retry
      // so either an insert or update will be executed successfully

      updateRouting(WAITING, cmd.rpi)
      db.change(PaymentTable.newVirtualSql, cmd.rpi.text, cmd.rpi.pr.paymentHash)
      db.change(PaymentTable.newSql, cmd.rpi.pr.paymentHash, NOIMAGE, 0, cmd.rpi.finalSum,
        WAITING, System.currentTimeMillis, cmd.rpi.text, cmd.rpi.pr.toJson, cmd.rpi.rd.toJson)

      uiNotify

    case (chan, norm: NormalData, _: CommitSig) =>
      for (Htlc(true, add) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkIncoming(add)
      for (Htlc(false, _) \ fulfill <- norm.commitments.localCommit.spec.fulfilled) updOkOutgoing(fulfill)

      for (htlc \ some <- norm.commitments.localCommit.spec.failed) some match {
        // This is a special case since only our peer can fail an outgoing payment like this, just fail it
        case _: UpdateFailMalformedHtlc => db.change(PaymentTable.updStatusSql, FAILURE, htlc.add.paymentHash)

        case fail: UpdateFailHtlc => for {
          // Cut routes, add failed nodes and channels
          paymentInfo <- getPaymentInfo(htlc.add.paymentHash)
          reducedPaymentInfo = cutRoutes(fail)(paymentInfo.runtime)
          // Try to use the rest of available local routes
        } completeRPI(reducedPaymentInfo) match {

          case Some(updatedPaymentInfo) =>
            // There are still routes left and we have just used one
            // if accepted: routing info will be upserted in onProcess
            // if not accepted: status will change to FAILURE in onError
            chan process PlainAddHtlc(updatedPaymentInfo)

          case None =>
            // No more spare routes left
            updateRouting(FAILURE, reducedPaymentInfo)
            // Ask server for new routes if all conditions are met
            val canNotProceed = reducedPaymentInfo canNotProceed chan.data.announce.nodeId
            val nope = canNotProceed || serverCallAttempts(htlc.add.paymentHash) > 8

            // Reset in case of manual retry later
            if (nope) serverCallAttempts(htlc.add.paymentHash) = 0
            else app.ChannelManager.getOutPaymentObs(reducedPaymentInfo)
              .doOnSubscribe(serverCallAttempts(htlc.add.paymentHash) += 1)
              .foreach(_ map PlainAddHtlc foreach chan.process, Tools.errlog)
        }
      }

      uiNotify
  }

  override def onBecome = {
    case (_, _, SYNC | NORMAL | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      db change PaymentTable.updFailWaitingSql
  }
}