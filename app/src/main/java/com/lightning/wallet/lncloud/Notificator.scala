package com.lightning.wallet.lncloud

import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.{ChannelListener, Commitments, NormalData}
import android.app.{AlarmManager, NotificationManager, PendingIntent}
import android.content.{BroadcastReceiver, Context, Intent}
import com.lightning.wallet.{MainActivity, R}

import android.support.v4.app.NotificationCompat
import com.lightning.wallet.ln.wire.CommitSig
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.Utils.app


object Notificator extends ChannelListener {
  private lazy val notificatorClass = classOf[Notificator]
  def getIntent = PendingIntent.getBroadcast(app, 0, new Intent(app, notificatorClass), 0)
  def getAlarmManager = app.getSystemService(Context.ALARM_SERVICE).asInstanceOf[AlarmManager]

  override def onBecome = {
    case (_, _, NORMAL | SYNC, CLOSING | NEGOTIATIONS) =>
      try getAlarmManager cancel getIntent catch none
  }

  override def onProcess = {
    case (_, norm: NormalData, _: CommitSig)
      // GUARD: has no in-flight HTLCs, remove delayed notifcation
      if Commitments.pendingHtlcs(norm.commitments).isEmpty =>
      try getAlarmManager cancel getIntent catch none

    case (_, norm: NormalData, _: CommitSig) =>
      val in4Minutes = System.currentTimeMillis + 1000 * 60 * 4
      try getAlarmManager.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP,
        in4Minutes, getIntent) catch none
  }
}

class Notificator extends BroadcastReceiver {
  def onReceive(ct: Context, intent: Intent) = classOf[MainActivity] match { case target =>
    val targetIntent = PendingIntent.getActivity(ct, 0, new Intent(ct, target), PendingIntent.FLAG_UPDATE_CURRENT)
    try ct.getSystemService(Context.NOTIFICATION_SERVICE).asInstanceOf[NotificationManager].notify(1, new NotificationCompat.Builder(ct)
      .setContentIntent(targetIntent).setSmallIcon(R.drawable.dead).setContentTitle(ct getString R.string.ln_htlc_notification_title)
      .setContentText(ct getString R.string.ln_htlc_notification).setAutoCancel(true).build) catch none
  }
}