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
  private lazy val notificatorClass = classOf[Notificator]
  def getIntent = PendingIntent.getBroadcast(app, 0, new Intent(app, notificatorClass), 0)
  def getAlarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]

  override def onProcess = {
    case (_, norm: NormalData, _: CommitSig)
      // GUARD: no in-flight HTLCs so cancel all pending alarms
      if Commitments.pendingHtlcs(norm.commitments).isEmpty =>
      try getAlarmManager cancel getIntent catch none

    case (_, _: NormalData, _: CommitSig) =>
      val inFiveMinutes = System.currentTimeMillis + 1000 * 60 * 5
      try getAlarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP,
        inFiveMinutes, getIntent) catch none
  }
}

class Notificator extends BroadcastReceiver {
  def onReceive(context: Context, intent: Intent) = try
    context.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager]
      .notify(1, new NotificationCompat.Builder(context).setSmallIcon(R.drawable.await)
        .setContentTitle(context getString R.string.ln_htlc_notification_title)
        .setContentText(context getString R.string.ln_htlc_notification)
        .build) catch none
}