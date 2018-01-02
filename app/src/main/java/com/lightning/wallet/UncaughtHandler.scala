package com.lightning.wallet

import java.io.{PrintWriter, StringWriter}
import java.lang.Thread.UncaughtExceptionHandler
import com.lightning.wallet.AbstractKit.ERROR_REPORT
import android.content.Intent
import android.app.Activity


class UncaughtHandler(ctxt: Activity)
extends UncaughtExceptionHandler {

  def uncaughtException(thread: Thread, exc: Throwable): Unit = {
    val emerge: Class[EmergencyActivity] = classOf[EmergencyActivity]
    val stackTraceWriter = new StringWriter
    val intent = new Intent(ctxt, emerge)

    exc printStackTrace new PrintWriter(stackTraceWriter)
    ctxt startActivity intent.putExtra(ERROR_REPORT, stackTraceWriter.toString)
    android.os.Process killProcess android.os.Process.myPid
    System exit 10
  }
}
