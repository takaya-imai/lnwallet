package com.lightning.wallet

import R.string._
import scala.util.{Success, Try}
import com.lightning.wallet.Utils.{wrap, app}
import com.lightning.wallet.AbstractKit.ERROR_REPORT
import org.bitcoinj.wallet.WalletProtobufSerializer
import com.lightning.wallet.ln.Tools.none
import java.io.FileInputStream
import android.os.Bundle
import android.view.View


class EmergencyActivity extends InfoActivity { me =>
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    wrap(initToolbar)(me setContentView R.layout.activity_emergency)
    add(me getString emerge_subtitle, Informer.EMERGENCY).ui.run
    getSupportActionBar.setTitle(me getString emerge_title)
    <(prepareWallet, _ => app toast err_general)(none)
  }

  def prepareWallet =
    app.kit = new app.WalletKit {
      val stream = new FileInputStream(app.walletFile)
      val proto = try WalletProtobufSerializer parseToProto stream finally stream.close
      wallet = new WalletProtobufSerializer readWallet (app.params, null, proto)

      def startUp = none
      blockChain = null
      peerGroup = null
      store = null
    }

  override def mkSetsForm: Unit = none
  override def notifySubTitle(subtitle: String, infoType: Int) = none
  def emergeMnemonic(view: View) = checkPass(me getString sets_mnemonic)(doViewMnemonic)

  def emergeReport(view: View) = Try(getIntent getStringExtra ERROR_REPORT) match {
    case Success(errorReport: String) => mkForm(me negBld dialog_ok, null, errorReport)
    case _ => mkForm(me negBld dialog_ok, null, me getString err_general)
  }
}
