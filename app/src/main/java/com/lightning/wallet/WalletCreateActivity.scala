package com.lightning.wallet

import R.string._
import org.bitcoinj.core.Utils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import org.bitcoinj.crypto.{EncryptedData, KeyCrypterScrypt}
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import android.widget.{Button, EditText, TextView}
import org.bitcoinj.core.{BlockChain, PeerGroup}

import com.lightning.wallet.ln.Tools.wrap
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import android.text.TextUtils
import android.os.Bundle
import android.view.View
import java.util.Date


object Mnemonic {
  def text(seed: DeterministicSeed) = TextUtils.join("\u0020", seed.getMnemonicCode)
  def decrypt(pass: String) = app.kit.wallet.getKeyCrypter match { case scrypt =>
    app.kit.wallet.getKeyChainSeed.decrypt(scrypt, pass, scrypt deriveKey pass)
  }

  def exportSeed(seed: DeterministicSeed, pass: String) = {
    val crypter = app.kit.wallet.getKeyCrypter.asInstanceOf[KeyCrypterScrypt]
    val cipher = crypter.encrypt(text(seed) getBytes "UTF-8", crypter deriveKey pass)

    val mnemonic = HEX encode cipher.encryptedBytes
    val initVector = HEX encode cipher.initialisationVector
    val salt = HEX encode crypter.getScryptParameters.getSalt.toByteArray
    s"Encrypted BIP32 mnemonic ${new Date}: $initVector:$mnemonic:$salt"
  }

  def importSeed(seed: String, pass: String) = {
    val Array(initVector, mnemonic, salt) = seed split ":"
    val (crypter, key) = app.getCrypterScrypt(HEX decode salt, pass)
    val data = new EncryptedData(HEX decode initVector, HEX decode mnemonic)
    new String(crypter.decrypt(data, key), "UTF-8")
  }
}

class WalletCreateActivity extends TimerActivity with ViewSwitch { me =>
  lazy val createWallet = findViewById(R.id.createWallet).asInstanceOf[Button]
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[EditText]

  // After wallet is created we emphasize an importance of mnemonic
  lazy val mnemonicText = findViewById(R.id.mnemonicText).asInstanceOf[TextView]
  lazy val walletReady = findViewById(R.id.walletReady).asInstanceOf[TextView]
  lazy val openWallet = findViewById(R.id.openWallet).asInstanceOf[Button]
  lazy val info = me clickableTextField findViewById(R.id.mnemonicInfo)

  lazy val views =
    findViewById(R.id.createInfo) ::
    findViewById(R.id.createProgress) ::
    findViewById(R.id.createDone) :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_create)
    createPass addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, st: Int, n: Int, af: Int) = {
        val buttonMessage = if (s.length >= 6) wallet_create else password_too_short
        createWallet setEnabled s.length >= 6
        createWallet setText buttonMessage
      }
    }
  }

  def makeNewWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE, View.GONE)
      startAsync

      override def startUp = {
        // Get seed before encryption
        wallet = new Wallet(app.params, true)
        val seed = wallet.getKeyChainSeed
        LNParams setup seed.getSeedBytes

        // Encrypt wallet and use checkpoints
        val (crypter, key) = app newCrypter createPass.getText
        store = new SPVBlockStore(app.params, app.chainFile)
        useCheckPoints(wallet.getEarliestKeyCreationTime)
        wallet.encrypt(crypter, key)

        // These should be initialized after checkpoints
        blockChain = new BlockChain(app.params, wallet, store)
        peerGroup = new PeerGroup(app.params, blockChain)

        if (app.isAlive) {
          setupAndStartDownload
          wallet saveToFile app.walletFile
          me runOnUiThread mnemonicText.setText(Mnemonic text seed)
          me runOnUiThread setVis(View.GONE, View.GONE, View.VISIBLE)
        }
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def goBtcWallet(view: View) = me exitTo classOf[BtcActivity]
  def newWallet(view: View) = hideKeys(makeNewWallet)

  def revealMnemonic(show: View) = {
    walletReady setText sets_noscreen
    mnemonicText setVisibility View.VISIBLE
    openWallet setVisibility View.VISIBLE
    show setVisibility View.GONE
  }
}