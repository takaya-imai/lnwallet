package com.lightning.wallet

import R.string._
import android.widget._

import scala.util.{Failure, Success, Try}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import org.ndeftools.util.activity.NfcReaderActivity
import org.bitcoinj.wallet.WalletProtobufSerializer
import concurrent.ExecutionContext.Implicits.global
import android.text.method.LinkMovementMethod
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import java.io.FileInputStream
import scala.concurrent.Future
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.view.View


trait ViewSwitch {
  val views: List[View]
  def setVis(ms: Int*) = views zip ms foreach {
    case (view, state) => view setVisibility state
  }
}

class MainActivity extends NfcReaderActivity with TimerActivity with ViewSwitch { me =>
  lazy val mainPassCheck = findViewById(R.id.mainPassCheck).asInstanceOf[Button]
  lazy val mainPassData = findViewById(R.id.mainPassData).asInstanceOf[EditText]
  lazy val greet = findViewById(R.id.mainGreetings).asInstanceOf[TextView]

  lazy val views =
    findViewById(R.id.mainChoice) ::
      findViewById(R.id.mainPassForm) ::
      findViewById(R.id.mainProgress) :: Nil

  lazy val makeKit = Future {
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = try WalletProtobufSerializer parseToProto stream finally stream.close
      wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)

      def startUp = {
        setupAndStartDownload
        exitTo apply classOf[LNActivity]
      }
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    app.TransData.value = null
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_main)
    greet setMovementMethod LinkMovementMethod.getInstance
  }

  // NFC and link

  override def onNoNfcIntentFound = {
    // Filter out failures and nulls, try to set value, proceed if successful and inform if not
    val attempts = Try(getIntent.getDataString) :: Try(getIntent getStringExtra Intent.EXTRA_TEXT) :: Nil
    val valid = attempts collectFirst { case res @ Success(nonNull: String) => res map app.TransData.parseValue }
    if (valid.isEmpty) next else valid foreach { case Failure(err) => app.TransData.onFail(inform)(err) case _ => next }
  }

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData parseValue asText
    app toast nfc_got

  } catch { case _: Throwable =>
    // Could not process a message
    me inform nfc_error
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = me inform nfc_error
  def readEmptyNdefMessage = me inform nfc_error

  // STARTUP LOGIC

  def inform(code: Int): Unit = showForm(mkChoiceDialog(next,
    finish, dialog_ok, dialog_cancel).setMessage(code).create)

  def next =
    (app.walletFile.exists, app.isAlive, LNParams.hasSeed) match {
      case (false, _, _) => setVis(View.VISIBLE, View.GONE, View.GONE)
      case (true, true, true) => exitTo apply classOf[LNActivity]

      case (true, false, _) =>
        // Launch of a previously closed app
        // Also happens if app has become inactive
        setVis(View.GONE, View.VISIBLE, View.GONE)
        mainPassCheck setOnClickListener onButtonTap {
          val proceed = makeKit.map(initialized => setSeed)
          <<(proceed, wrong)(passOk => app.kit.startAsync)
          setVis(View.GONE, View.GONE, View.VISIBLE)
        }

      case (true, true, false) =>
        // Should not happen but whatever
        setVis(View.GONE, View.VISIBLE, View.GONE)
        mainPassCheck setOnClickListener onButtonTap {
          <(setSeed, wrong)(passOk => app.kit.startAsync)
          setVis(View.GONE, View.GONE, View.VISIBLE)
        }
    }

  def wrong(error: Throwable) = {
    setVis(View.GONE, View.VISIBLE, View.GONE)
    app toast password_wrong
  }

  def setSeed = {
    val pass = mainPassData.getText.toString
    val seed = Mnemonic decrypt pass
    LNParams setSeed seed
  }

  def goRestoreWallet(view: View) = me exitTo classOf[WalletRestoreActivity]
  def goCreateWallet(view: View) = me exitTo classOf[WalletCreateActivity]
}