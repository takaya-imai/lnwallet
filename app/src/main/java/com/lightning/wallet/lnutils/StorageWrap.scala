package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.Connector.CMDStart
import com.lightning.wallet.helper.RichCursor
import com.lightning.wallet.helper.AES
import com.lightning.wallet.Utils.app
import fr.acinq.bitcoin.MilliSatoshi
import fr.acinq.bitcoin.BinaryData
import scala.collection.mutable
import net.sqlcipher.Cursor


object StorageWrap {
  def put(value: String, key: String) = db txWrap {
    db.change(StorageTable.newSql, params = key, value)
    db.change(StorageTable.updSql, params = value, key)
  }

  def get(key: String) = {
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

    case (_, norm: NormalData, _: CommitSig)
      // We should remind cloud to maybe send a scheduled data
      if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
      cloud doProcess CMDStart
  }

  override def onBecome = {
    case (_, norm: NormalData, WAIT_FUNDING_DONE, NORMAL) =>
      // Once a new channel becomes NORMAL we save it's state on a cloud
      val staticChannelState = RefundingData(norm.announce, norm.commitments)
      val packed = AES.encode(staticChannelState.toJson.toString, cloudSecret)
      cloud doProcess CloudAct(packed, Seq("key" -> cloudId.toString), "data/put")
  }
}

object PaymentInfoWrap extends PaymentInfoBag with ChannelListener { me =>
  def uiNotify = app.getContentResolver.notifyChange(db sqlPath PaymentInfoTable.table, null)
  def getRoutingData(hash: BinaryData) = RichCursor apply db.select(RoutingDataTable.selectSql, hash.toString) headTry toRoutingData
  def getPaymentInfo(hash: BinaryData) = RichCursor apply db.select(PaymentInfoTable.selectSql, hash.toString) headTry toPaymentInfo
  def byQuery(query: String): Cursor = db.select(PaymentInfoTable.searchSql, s"$query*")
  def byRecent: Cursor = db select PaymentInfoTable.selectRecentSql

  def toRoutingData(rc: RichCursor) = to[RoutingData](rc string RoutingDataTable.routing)
  def toPaymentInfo(rc: RichCursor) = PaymentInfo(rc string PaymentInfoTable.hash, rc int PaymentInfoTable.incoming,
    rc string PaymentInfoTable.preimage, MilliSatoshi(rc long PaymentInfoTable.amount), rc int PaymentInfoTable.status,
    rc long PaymentInfoTable.stamp, rc string PaymentInfoTable.text)

  def upsertRoutingData(rd: RoutingData) = {
    // Always use REPLACE instead of INSERT INTO for RoutingData and PaymentInfo
    db.change(RoutingDataTable.newSql, rd.pr.paymentHash.toString, rd.toJson.toString)
  }

  def upsertPaymentInfo(info: PaymentInfo) = {
    db.change(PaymentInfoTable.newVirtualSql, s"${info.text} ${info.hash.toString}", info.hash.toString)
    db.change(PaymentInfoTable.newSql, info.hash.toString, info.incoming.toString, info.preimage.toString,
      info.amount.amount.toString, info.status.toString, info.text, System.currentTimeMillis.toString)
  }

  def updateStatus(status: Int, hash: BinaryData) = {
    // Can become HIDDEN -> SUCCESS or WAITING -> FAILURE or SUCCESS
    db.change(PaymentInfoTable.updStatusSql, status.toString, hash.toString)
  }

  def updatePreimg(upd: UpdateFulfillHtlc) = {
    // Incoming payment is in fact fulfilled once we get a preimage
    // so we should save a preimage right away without waiting for peer's next commitment sig
    db.change(PaymentInfoTable.updPreimageSql, upd.paymentPreimage.toString, upd.paymentHash.toString)
  }

  def updateIncoming(add: UpdateAddHtlc) = {
    // Incoming payment may provide a larger amount than what we requested
    // so an actual incoming sum should be updated along with timestamp
    db.change(PaymentInfoTable.updIncomingSql, add.amountMsat.toString,
      System.currentTimeMillis.toString, add.paymentHash.toString)
  }

  // Records a number of retry attempts for a given outgoing payment hash
  val serverCallAttempts = mutable.Map.empty[BinaryData, Int] withDefaultValue 0

  override def onError = {
    case (_, exception: CMDException) =>
      // Useless most of the time but needed for retry add failures
      // CMDException should still be used or channel will be closed
      updateStatus(FAILURE, exception.cmd.rd.pr.paymentHash)
      uiNotify

    case chan \ error =>
      // Close and log an error
      chan process CMDShutdown
      Tools errlog error
  }

  override def onProcess = {
    case (_, _: NormalData, add: UpdateAddHtlc) =>
      // Actual incoming amount paid may be different
      // We need to record exactly how much was paid
      me updateIncoming add

    case (_, _: NormalData, fulfill: UpdateFulfillHtlc) =>
      // We need to save a preimage as soon as we receive it
      me updatePreimg fulfill

    case (_, _: NormalData, cmd: CMDAddHtlc) =>
      // Channel has accepted a possible payment retry
      // must insert or update related routing data

      db txWrap {
        me upsertRoutingData cmd.rd
        me upsertPaymentInfo PaymentInfo(cmd.rd.pr.paymentHash, incoming = 0, NOIMAGE,
          cmd.rd.pr.finalSum, WAITING, System.currentTimeMillis, cmd.rd.pr.textDescription)
      }

      uiNotify

    case (chan, norm: NormalData, _: CommitSig) =>
      // fulfilled: update as SUCCESS in a database
      // failed: re-try or update as FAILURE
      // htlcs: no need to update

      db txWrap {
        for (htlc <- norm.commitments.localCommit.spec.fulfilled)
          updateStatus(SUCCESS, htlc.add.paymentHash)

        for (htlc \ some <- norm.commitments.localCommit.spec.failed) some match {
          // Malformed payment is a special case returned by our peer so just fail it
          case _: UpdateFailMalformedHtlc => updateStatus(FAILURE, htlc.add.paymentHash)

          case fail: UpdateFailHtlc => for {
            // Cut local routes, add failed nodes and channels
            oldRoutingData <- getRoutingData(htlc.add.paymentHash)
            reducedRoutingData = cutRoutes(fail)(oldRoutingData)
            // Try to use the rest of available local routes
          } completeRoutingData(reducedRoutingData) match {

            case Some(updatedRoutingData) =>
              // There are still routes left and we have just used one
              // if accepted: routing info will be upserted in onProcess
              // if not accepted: status will change to FAILURE in onError
              chan process PlainAddHtlc(updatedRoutingData)

            case None =>
              // No more spare routes left
              updateStatus(FAILURE, htlc.add.paymentHash)
              me upsertRoutingData reducedRoutingData

              // Ask server for new routes if all conditions are met
              val peerIsBad = reducedRoutingData.badNodes contains chan.data.announce.nodeId
              val recipientIsBad = reducedRoutingData.badNodes contains oldRoutingData.pr.nodeId
              val nothingExcluded = reducedRoutingData.badNodes.isEmpty && reducedRoutingData.badChannels.isEmpty
              val nope = peerIsBad | recipientIsBad | nothingExcluded | serverCallAttempts(htlc.add.paymentHash) > 4

              if (!nope)
                app.ChannelManager.outPaymentObs(reducedRoutingData)
                  .doOnSubscribe(serverCallAttempts(htlc.add.paymentHash) += 1)
                  .foreach(_ map PlainAddHtlc foreach chan.process, Tools.errlog)
          }
        }
      }

      uiNotify
  }

  override def onBecome = {
    case (_, _, SYNC | NORMAL | NEGOTIATIONS, CLOSING) =>
      // WAITING will either be redeemed or refunded later
      db change PaymentInfoTable.updFailWaitingSql
  }
}