package com.lightning.wallet

import android.widget._
import com.lightning.wallet.R.string._

import android.view.{View, ViewGroup}
import org.bitcoinj.core.{BlockChain, PeerGroup}
import com.lightning.wallet.ln.Tools.{none, wrap, runAnd}
import org.bitcoinj.wallet.{DeterministicSeed, KeyChainGroup, Wallet}

import android.widget.DatePicker.OnDateChangedListener
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import java.util.Calendar
import android.os.Bundle


class WhenPicker(host: TimerActivity, start: Long)
extends DatePicker(host) with OnDateChangedListener { me =>
  def pure = runAnd(me)(try getParent.asInstanceOf[ViewGroup] removeView me catch none)
  def human = java.text.DateFormat getDateInstance java.text.DateFormat.MEDIUM format cal.getTime
  def onDateChanged(view: DatePicker, year: Int, mon: Int, dt: Int) = cal.set(year, mon, dt)
  init(cal get Calendar.YEAR, cal get Calendar.MONTH, cal get Calendar.DATE, me)

  lazy val cal = {
    val calendar = Calendar.getInstance
    calendar setTimeInMillis start
    calendar
  }
}

class WalletRestoreActivity extends TimerActivity with ViewSwitch { me =>
  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  lazy val restoreWallet = findViewById(R.id.restoreWallet).asInstanceOf[Button]
  lazy val restoreCode = findViewById(R.id.restoreCode).asInstanceOf[TextView]
  lazy val restoreWhen = findViewById(R.id.restoreWhen).asInstanceOf[Button]
  lazy val password = findViewById(R.id.restorePass).asInstanceOf[EditText]
  lazy val datePicker = new WhenPicker(me, 1488326400L * 1000)

  lazy val views =
    findViewById(R.id.restoreInfo) ::
      findViewById(R.id.restoreProgress) :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_restore)
    val changeListener = new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = {
        val mnemonicWordsAreOk = restoreCode.getText.toString.split("\\s+").length > 11
        val passIsOk = password.getText.length >= 6

        restoreWallet.setEnabled(mnemonicWordsAreOk & passIsOk)
        if (!mnemonicWordsAreOk) restoreWallet setText restore_mnemonic_wrong
        else if (!passIsOk) restoreWallet setText password_too_short
        else restoreWallet setText restore_wallet
      }
    }

    if (app.TransData.value != null) {
      // This should be an unencrypted mnemonic code
      restoreCode setText app.TransData.value.toString
      app.TransData.value = null
    }

    restoreWhen setText datePicker.human
    password addTextChangedListener changeListener
    restoreCode addTextChangedListener changeListener
  }

  def doRecoverWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
      startAsync

      def startUp = {
        val whenTime = datePicker.cal.getTimeInMillis / 1000
        val mnemonic = restoreCode.getText.toString.toLowerCase.trim
        val seed = new DeterministicSeed(mnemonic, null, "", whenTime)
        val keyChainGroup = new KeyChainGroup(app.params, seed)
        val (crypter, key) = app getCrypter password.getText
        LNParams setup seed.getSeedBytes

        // Recreate encrypted wallet and use checkpoints
        store = new SPVBlockStore(app.params, app.chainFile)
        wallet = new Wallet(app.params, keyChainGroup)
        wallet.encrypt(crypter, key)
        useCheckPoints(whenTime)

        // Must be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          exitTo apply classOf[BtcActivity]
        }
      }
    }

  def recWallet(v: View) = hideKeys(doRecoverWallet)
  def setWhen(v: View) = mkForm(mkChoiceDialog(restoreWhen setText datePicker.human,
    none, dialog_ok, dialog_cancel), title = null, content = datePicker.pure)
}