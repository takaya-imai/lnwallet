package com.lightning.wallet

import R.string._
import org.bitcoinj.core.{BlockChain, PeerGroup}
import android.widget.{Button, EditText}

import com.lightning.wallet.ln.Tools.wrap
import org.bitcoinj.store.SPVBlockStore
import com.lightning.wallet.ln.LNParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.wallet.Wallet
import android.os.Bundle
import android.view.View


class WalletCreateActivity extends TimerActivity with ViewSwitch { me =>
  lazy val createWallet = findViewById(R.id.createWallet).asInstanceOf[Button]
  lazy val createPass = findViewById(R.id.createPass).asInstanceOf[EditText]
  lazy val info = me clickableTextField findViewById(R.id.mnemonicInfo)

  lazy val views =
    findViewById(R.id.createInfo) ::
    findViewById(R.id.createProgress) :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    setContentView(R.layout.activity_create)
    createPass addTextChangedListener new TextChangedWatcher {
      override def onTextChanged(s: CharSequence, st: Int, n: Int, af: Int) =
        if (s.length >= 6) wrap(createWallet setEnabled true)(createWallet setText wallet_create)
        else wrap(createWallet setEnabled false)(createWallet setText password_too_short)
    }
  }

  def makeNewWallet =
    app.kit = new app.WalletKit {
      setVis(View.GONE, View.VISIBLE)
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
          me exitTo classOf[BtcActivity]
        }
      }
    }

  override def onBackPressed = wrap(super.onBackPressed)(app.kit.stopAsync)
  def newWallet(view: View) = hideKeys(makeNewWallet)
}