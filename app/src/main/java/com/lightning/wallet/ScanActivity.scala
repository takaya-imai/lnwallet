package com.lightning.wallet

import R.string._
import com.lightning.wallet.Utils._
import com.journeyapps.barcodescanner._
import com.lightning.wallet.ln.Tools.{none,wrap}

import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import android.widget.Toast
import android.os.Bundle


class ScanActivity extends TimerActivity with BarcodeCallback { me =>
  lazy val reader = findViewById(R.id.reader).asInstanceOf[BarcodeView]
  lazy val beeper = new com.lightning.wallet.helper.SoundPlayer(me)
  type Points = java.util.List[com.google.zxing.ResultPoint]
  private[this] var lastAttempt = System.currentTimeMillis

  // Initialize this activity, method is run once
  override def onCreate(savedInstState: Bundle) =
  {
    setContentView(R.layout.activity_scan)
    super.onCreate(savedInstState)
    reader decodeContinuous me
  }

  def tryParseQR(text: String) = try {
    lastAttempt = System.currentTimeMillis
    beeper.playRawResource(R.raw.beep, false)
    app.TransData parseValue text

    // Find out where to go
    app.TransData.value match {
      case _: BitcoinURI => me exitTo classOf[BtcActivity]
      case _: Address => me exitTo classOf[BtcActivity]
      case _ => me exitTo classOf[LNActivity]
    }

    // Parsing error may occur
  } catch app.TransData.onFail { code =>
    Toast.makeText(app, text, Toast.LENGTH_LONG).show
    showForm(mkChoiceDialog(reader.resume, finish, dialog_ok,
      dialog_cancel).setMessage(code).create)

    // Pause anyway
  } finally reader.pause

  // Only try to decode result if 3 seconds elapsed
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParseQR(rawText)
  }

  override def possibleResultPoints(pts: Points) = none
  override def onResume = wrap(super.onResume)(reader.resume)
  override def onPause = wrap(super.onPause)(reader.pause)
}