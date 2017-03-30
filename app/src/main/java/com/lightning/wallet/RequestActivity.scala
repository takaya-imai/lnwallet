package com.lightning.wallet

import android.graphics._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._

import java.io.{File, FileOutputStream}
import android.os.{Bundle, Environment}
import android.view.View.{GONE, VISIBLE}
import android.nfc.{NdefMessage, NfcEvent}
import android.text.{StaticLayout, TextPaint}
import com.google.zxing.{BarcodeFormat, EncodeHintType}
import android.widget.{ImageButton, ImageView, LinearLayout}

import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel
import org.ndeftools.util.activity.NfcBeamWriterActivity
import android.text.Layout.Alignment.ALIGN_NORMAL
import android.graphics.Bitmap.Config.ARGB_8888
import com.google.zxing.qrcode.QRCodeWriter
import android.graphics.Bitmap.createBitmap
import com.lightning.wallet.ln.Tools.none
import org.ndeftools.wellknown.TextRecord
import com.lightning.wallet.ln.Invoice
import org.bitcoinj.core.Address
import android.content.Intent
import org.ndeftools.Message
import android.view.View
import android.net.Uri


object QRGen {
  val writer = new QRCodeWriter
  val hints = new java.util.Hashtable[EncodeHintType, Any]
  hints.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.M)
  hints.put(EncodeHintType.MARGIN, 1)

  def get(txt: String, size: Int): Bitmap = {
    val bitMatrix = writer.encode(txt, BarcodeFormat.QR_CODE, size, size, hints)
    val (wid, height) = (bitMatrix.getWidth, bitMatrix.getHeight)
    val pixels = new Array[Int](wid * height)

    for (y <- 0 until height) for (x <- 0 until wid)
      pixels(y * wid + x) = bitMatrix.get(x, y) match {
        case true => Color.BLACK case false => Color.WHITE
      }

    val qrBitmap = createBitmap(wid, height, ARGB_8888)
    qrBitmap.setPixels(pixels, 0, wid, 0, 0, wid, height)
    qrBitmap
  }
}

object FileOps {
  def shell(name: String) = {
    val path = Environment.getExternalStorageDirectory
    val dir = new File(path.getAbsolutePath + "/" + appName)
    if (!dir.exists) dir.mkdirs
    new File(dir, name)
  }
}

class RequestActivity extends NfcBeamWriterActivity
with TimerActivity with ViewSwitch { me =>

  lazy val reqCode = findViewById(R.id.reqCode).asInstanceOf[ImageView]
  lazy val reqShare = findViewById(R.id.reqShare).asInstanceOf[ImageButton]
  lazy val copyData = findViewById(R.id.copyData).asInstanceOf[ImageButton]
  lazy val restoreInfo = findViewById(R.id.restoreInfo).asInstanceOf[LinearLayout]

  lazy val views =
    findViewById(R.id.reqNfcEnabled) ::
      findViewById(R.id.reqNfcSettings) :: Nil

  lazy val textBounds = getResources getDimensionPixelSize R.dimen.bitmap_text_bounds
  lazy val bottomSize = getResources getDimensionPixelSize R.dimen.bitmap_bottom_size
  lazy val topSize = getResources getDimensionPixelSize R.dimen.bitmap_top_size
  lazy val qrSize = getResources getDimensionPixelSize R.dimen.bitmap_qr_size
  lazy val btcAddressHint = me getString spend_address_hint

  case class NFCData(data: String) {
    def getNfcMessage: NdefMessage = {
      val textRecord = new TextRecord(data)
      val message = new Message
      message add textRecord
      message.getNdefMessage
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    setContentView(R.layout.activity_request)
    super.onCreate(savedState)
    setDetecting(true)

    app.TransData.value match {
      case inv: Invoice => showInfo(drawAll(withSign(inv.sum), getString(ln_qr_warning).html, _), Invoice serialize inv, GONE)
      case address: Address => showInfo(drawAll(btcAddressHint, humanAddr(address), _), address.toString, VISIBLE)
      case pay: AddrData => showInfo(drawAll(withSign(pay.cn), humanAddr(pay.adr), _), pay.link, VISIBLE)
      case _ => finish
    }
  }

  def showInfo(renderBitmap: Bitmap => Bitmap, data: String, mode: Int) = {
    <(fun = QRGen.get(data, qrSize), fail)(renderBitmap andThen setView)
    copyData setOnClickListener onButtonTap(app setBuffer data)
    app.TransData.value = NFCData(data)
    restoreInfo setVisibility mode
  }

  def setView(displayedImage: Bitmap) = {
    reqShare setOnClickListener onButtonTap {
      <(me saveImage displayedImage, fail) { file =>
        val share = new Intent setAction Intent.ACTION_SEND setType "image/png"
        me startActivity share.putExtra(Intent.EXTRA_STREAM, Uri fromFile file)
      }
    }

    reqCode setImageBitmap displayedImage
    reqShare setEnabled true
  }

  // Low level draw utilites
  def drawAll(top: CharSequence, bot: CharSequence, qrBitmap: Bitmap) = {
    val bitmap = createBitmap(qrSize, topSize + qrSize + bottomSize, ARGB_8888)
    val ypos = topSize + qrSize + bottomSize / 2
    val canvas = new Canvas(bitmap)
    val transRect = new Rect

    canvas drawColor 0xFFEEEEEE
    transRect.set(0, topSize, qrSize, topSize + qrSize)
    canvas.drawBitmap(qrBitmap, null, transRect, null)
    text(canvas, top, qrSize / 2, topSize / 2)
    text(canvas, bot, qrSize / 2, ypos)
    bitmap
  }

  def text(canvas: Canvas, text: CharSequence, x: Float, baseY: Float) = {
    val layout = new StaticLayout(text, paint, textBounds, ALIGN_NORMAL, 1f, 0f, false)
    val y = baseY - layout.getHeight / 2f

    canvas.save
    canvas.translate(x, y)
    layout draw canvas
    canvas.restore
  }

  def paint = {
    val newPaint = new TextPaint(Paint.ANTI_ALIAS_FLAG)
    newPaint setTextSize getResources.getDimensionPixelSize(R.dimen.text_small)
    newPaint setTypeface Typeface.create("Droid Sans", Typeface.NORMAL)
    newPaint setTextAlign Paint.Align.CENTER
    newPaint setStyle Paint.Style.FILL
    newPaint setColor Color.BLACK
    newPaint
  }

  def saveImage(bits: Bitmap) = {
    val imageFile = FileOps shell "qr.png"
    val stream = new FileOutputStream(imageFile)
    bits.compress(Bitmap.CompressFormat.PNG, 80, stream)
    stream.flush
    stream.close
    imageFile
  }

  // NFC

  // No NFC reading in this activity
  def readNdefMessage(m: Message) = none
  def readEmptyNdefMessage = none
  def readNonNdefMessage = none

  // NFC pushing if it is present
  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onNfcFeatureFound = wrap(startPushing)(super.onNfcFeatureFound)
  def onNfcStateChange(ok: Boolean) = if (ok) onNfcStateEnabled else onNfcStateDisabled
  def onNfcPushStateChange(ok: Boolean) = if (ok) onNfcPushStateEnabled else onNfcPushStateDisabled
  def createNdefMessage(e: NfcEvent) = app.TransData.value match { case n: NFCData => n.getNfcMessage case _ => null }

  def onNfcPushStateDisabled = setVis(GONE, VISIBLE)
  def onNfcPushStateEnabled = setVis(VISIBLE, GONE)
  def onNfcStateDisabled = setVis(GONE, VISIBLE)
  def onNfcFeatureNotFound = setVis(GONE, GONE)
  def onNdefPushCompleted = none
  def onNfcStateEnabled = none

  def fail(error: Throwable): Unit = negBld(dialog_ok).setMessage(err_general).show
  def showTip(v: View) = negBld(dialog_ok).setMessage(nfc_payee_tip).show
  def goSettings(v: View) = startNfcSharingSettingsActivity
}