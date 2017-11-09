package com.lightning.wallet

import R.string._
import android.widget._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.Utils.{app, isMnemonicCorrect}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import scala.util.{Failure, Success, Try}

import org.ndeftools.util.activity.NfcReaderActivity
import concurrent.ExecutionContext.Implicits.global
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.Tools.none
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.helper.AES
import fr.acinq.bitcoin.Crypto.sha256
import fr.acinq.bitcoin.BinaryData
import scala.concurrent.Future
import java.io.FileInputStream
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

class MainActivity extends NfcReaderActivity
with TimerActivity with ViewSwitch { me =>

  lazy val mainPassCheck = findViewById(R.id.mainPassCheck).asInstanceOf[Button]
  lazy val mainPassData = findViewById(R.id.mainPassData).asInstanceOf[EditText]
  lazy val greet = me clickableTextField findViewById(R.id.mainGreetings)

  lazy val views =
    findViewById(R.id.mainChoice) ::
      findViewById(R.id.mainPassForm) ::
      findViewById(R.id.mainProgress) :: Nil

  lazy val prepareKit = Future {
    val stream = new FileInputStream(app.walletFile)
    val proto = WalletProtobufSerializer parseToProto stream

    app.kit = new app.WalletKit {
      wallet = (new WalletProtobufSerializer).readWallet(app.params, null, proto)
      store = new org.bitcoinj.store.SPVBlockStore(app.params, app.chainFile)
      blockChain = new BlockChain(app.params, wallet, store)
      peerGroup = new PeerGroup(app.params, blockChain)

      def startUp: Unit = {
        setupAndStartDownload
        next
      }
    }
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_main)
  }

  // NFC AND SHARE

  override def onNoNfcIntentFound = {
    // Filter out failures and nulls, try to set value, proceed if successful and inform if not
    val attempts = Try(getIntent.getDataString) :: Try(getIntent getStringExtra Intent.EXTRA_TEXT) :: Nil
    val valid = attempts collectFirst { case res @ Success(nonNull: String) => res map app.TransData.recordValue }
    if (valid.isEmpty) next else valid foreach { case Failure(err) => app.TransData.onFail(inform)(err) case _ => next }
  }

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
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

  def next: Unit =
    (app.walletFile.exists, app.isAlive, LNParams.isSetUp) match {
      case (false, _, _) => setVis(View.VISIBLE, View.GONE, View.GONE)

      case (true, true, true) =>
        // We go to a last visited activity by default
        val landing = app.prefs.getString(AbstractKit.LANDING, AbstractKit.BITCOIN)
        val target = if (landing == AbstractKit.BITCOIN) classOf[BtcActivity] else classOf[LNActivity]
        me exitTo target

      case (true, false, _) =>
        // Launch of a previously closed app
        // Also happens if app has become inactive
        setVis(View.GONE, View.VISIBLE, View.GONE)
        <<(prepareKit, throw _)(none)

        mainPassCheck setOnClickListener onButtonTap {
          // Check password after wallet initialization is complete
          <<(prepareKit map setup, wrongPass)(_ => app.kit.startAsync)
          setVis(View.GONE, View.GONE, View.VISIBLE)
        }

      case (true, true, false) =>
        // This is not right!
        System exit 0
    }

  // MISC

  def setup(some: Any) = {
    val password = mainPassData.getText.toString
    val rawSeed = app.kit decryptSeed password
    LNParams setup rawSeed.getSeedBytes
  }

  def wrongPass(err: Throwable) = {
    setVis(View.GONE, View.VISIBLE, View.GONE)
    app toast password_wrong
  }

  def inform(messageCode: Int): Unit =
    showForm(mkChoiceDialog(next, finish, dialog_ok,
      dialog_cancel).setMessage(messageCode).create)

  def goRestoreWallet(view: View) = {
    val options: Array[String] = getResources getStringArray R.array.restore_mnemonic_options
    val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
    val alert = mkForm(me negBld dialog_cancel, me getString restore_hint, lst)

    def exitToRestore = me exitTo classOf[WalletRestoreActivity]
    lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, options)
    lst setOnItemClickListener onTap { pos => if (pos == 1) rm(alert)(exitToRestore) else proceed }
    lst setDividerHeight 0
    lst setDivider null

    def proceed = rm(alert) {
      val form = getLayoutInflater.inflate(R.layout.frag_encrypted_mnemonic, null)
      val encryptedMnemonic = form.findViewById(R.id.encryptedMnemonic).asInstanceOf[EditText]
      val oldWalletPassword = form.findViewById(R.id.oldWalletPassword).asInstanceOf[EditText]
      lazy val dialog = mkChoiceDialog(attempt, none, dialog_ok, dialog_cancel)
      lazy val alert1 = mkForm(dialog, me getString wallet_restore, form)
      alert1

      def attempt: Unit = rm(alert1) {
        // Catch wrong password in Future
        <(decryptAndImport, onFail)(none)
      }

      def decryptAndImport = {
        val packed = BinaryData(encryptedMnemonic.getText.toString)
        val hash = sha256(oldWalletPassword.getText.toString.binary.data)
        val plain = AES.decode(hash.toArray)(packed.toArray)
        require(isMnemonicCorrect(plain), "Wrong password")
        app.TransData.value = plain
        exitToRestore
      }
    }
  }

  def goCreateWallet(view: View) =
    me exitTo classOf[WalletCreateActivity]
}