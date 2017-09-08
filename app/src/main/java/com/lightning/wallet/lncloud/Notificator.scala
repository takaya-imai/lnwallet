package com.lightning.wallet.lncloud

import android.app.{AlarmManager, Notification, NotificationManager, PendingIntent}
import com.lightning.wallet.ln.{ChannelListener, Commitments, NormalData}
import android.content.{BroadcastReceiver, Context, Intent}

import android.support.v4.app.NotificationCompat
import com.lightning.wallet.ln.wire.CommitSig
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app
import com.lightning.wallet.R


object Notificator extends ChannelListener {
  lazy val notificatorClass = classOf[Notificator]
  def getIntent = PendingIntent.getBroadcast(app, 0, new Intent(app, notificatorClass), 0)
  def getAlarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]
  def getManager = app.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]

  def getNotification: Notification =
    new NotificationCompat.Builder(app).setSmallIcon(R.drawable.await)
      .setContentText(app getString R.string.ln_htlc_notification)
      .setContentTitle(app getString R.string.app_name).build

  override def onProcess = {
    case (_, norm: NormalData, _: CommitSig)
      // GUARD: no in-flight HTLCs so we cancel all pending alarms
      if Commitments.pendingHtlcs(norm.commitments).isEmpty =>
      try getAlarmManager cancel getIntent catch none
      try getManager.cancelAll catch none

    case (_, _: NormalData, _: CommitSig) => try {
      val inFiveMinutes = System.currentTimeMillis + 1000 * 60 * 5
      getAlarmManager.set(AlarmManager.RTC_WAKEUP, inFiveMinutes, getIntent)
    } catch none
  }
}

import Notificator._
class Notificator extends BroadcastReceiver {
  def onReceive(context: Context, intent: Intent) =
    getManager.notify(1, getNotification)
}