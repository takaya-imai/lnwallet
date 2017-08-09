package com.lightning.wallet

import java.util.Date

import com.lightning.wallet.R.string._
import com.lightning.wallet.Utils._
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import android.widget._
import android.view.{Menu, MenuItem, View, ViewGroup}
import org.ndeftools.Message
import com.lightning.wallet.ln.LNParams._
import org.ndeftools.util.activity.NfcReaderActivity
import android.os.Bundle
import Utils.app
import android.support.v4.view.MenuItemCompat
import android.support.v7.widget.SearchView.OnQueryTextListener
import android.webkit.URLUtil
import com.lightning.wallet.helper.{ReactCallback, ReactLoader, RichCursor}
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.ln.LNParams.getPathfinder
import com.lightning.wallet.ln._

import scala.concurrent.duration._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.core.Address
import org.bitcoinj.uri.BitcoinURI

import scala.util.{Failure, Success}


trait SearchBar { me =>
  import android.support.v7.widget.SearchView
  protected[this] var searchItem: MenuItem = _
  protected[this] var search: SearchView = _

  private[this] val lst = new OnQueryTextListener {
    def onQueryTextSubmit(queryText: String) = true
    def onQueryTextChange(queryText: String) =
      runAnd(true)(me react queryText)
  }

  def react(query: String)
  def setupSearch(menu: Menu) = {
    searchItem = menu findItem R.id.action_search
    val view = MenuItemCompat getActionView searchItem
    search = view.asInstanceOf[SearchView]
    search setOnQueryTextListener lst
  }
}

trait DataReader extends NfcReaderActivity {
  // Data may come via NFC message or share action
  // so we check both of them at once

  override def onResume =
    wrap(super.onResume)(checkTransData)

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }

  def checkTransData: Unit
  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = app toast nfc_error
  def readEmptyNdefMessage = app toast nfc_error
}

class LNActivity extends DataReader
with ToolbarActivity with HumanTimeDisplay
with ListUpdater with SearchBar { me =>

  lazy val fab = findViewById(R.id.fab).asInstanceOf[com.github.clans.fab.FloatingActionMenu]
  lazy val paymentProvider = new PaymentsDataProvider
  lazy val lnTitle = getString(ln_title)
  lazy val adapter = new LNAdapter

  private[this] var pathfinder: Pathfinder = _

  // Payment history and search results loader
  // Remembers last search term in case of reload
  class PaymentsDataProvider extends ReactCallback(me) { self =>
    def updatePaymentList(payments: InfoVec) = wrap(adapter.notifyDataSetChanged)(adapter.payments = payments)
    def reload(txt: String) = runAnd(lastQuery = txt)(getSupportLoaderManager.restartLoader(1, null, self).forceLoad)
    def recent = new ExtendedPaymentInfoLoader { def getCursor = bag.recentPayments }
    def search = new ExtendedPaymentInfoLoader { def getCursor = bag byQuery lastQuery }
    def onCreateLoader(id: Int, b: Bundle) = if (lastQuery.isEmpty) recent else search
    val observeTablePath = db sqlPath PaymentInfoTable.table
    private var lastQuery = new String

    type InfoVec = Vector[PaymentInfo]
    abstract class ExtendedPaymentInfoLoader extends ReactLoader[PaymentInfo](me) {
      val consume: InfoVec => Unit = payments => me runOnUiThread updatePaymentList(payments)
      def createItem(shifted: RichCursor) = bag toPaymentInfo shifted
    }
  }

  class LNAdapter extends BaseAdapter {
    def getView(position: Int, cv: View, parent: ViewGroup) = {
      val view = if (null == cv) getLayoutInflater.inflate(txLineType, null) else cv
      val hold = if (null == view.getTag) new LNView(view) else view.getTag.asInstanceOf[LNView]
      hold fillView getItem(position)
      view
    }

    var payments = Vector.empty[PaymentInfo]
    def getItem(position: Int) = payments(position)
    def getItemId(position: Int) = position
    def getCount = payments.size
  }

  class LNView(view: View)
  extends TxViewHolder(view) {
    def fillView(info: PaymentInfo) = {
//      val stamp = new Date(info.spec.request.timestamp * 1000)
//      val time = when(System.currentTimeMillis, stamp)
//
//      val image = info.status match {
//        case PaymentSpec.SUCCESS => conf1
//        case PaymentSpec.FAILURE => dead
//        case _ => await
//      }
//
//      transactWhen setText time.html
//      //transactSum setText paymentMarking.html
//      transactCircle setImageResource image
    }
  }

  // INTERFACE IMPLEMENTING METHODS

  def react(query: String) = paymentProvider reload query
  def notifySubTitle(subtitle: String, infoType: Int): Unit = {
    add(subtitle, infoType).timer.schedule(me del infoType, 25000)
    me runOnUiThread ui
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    wrap(initToolbar)(me setContentView R.layout.activity_ln)
    add(me getString ln_notify_operational, Informer.LNSTATE).ui.run

    list setAdapter adapter
    list setFooterDividersEnabled false
    paymentProvider reload new String
    me startListUpdates adapter
    me setDetecting true

    // Wire up general listeners
    app.kit.wallet addCoinsSentEventListener txTracker
    app.kit.wallet addCoinsReceivedEventListener txTracker
    app.kit.wallet addTransactionConfidenceEventListener txTracker
    app.kit.peerGroup addBlocksDownloadedEventListener catchListener
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    app.kit.wallet removeTransactionConfidenceEventListener txTracker
    stopDetecting
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_normal_ops, menu)
    setupSearch(menu)
    true
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSetBackupServer) new SetBackupServer
    else if (m.getItemId == R.id.actionCloseChannel) closeChannel
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => me goTo classOf[BtcActivity]
    case adr: Address => me goTo classOf[BtcActivity]

    case unusable =>
      app.TransData.value = null
      Tools log s"Unusable $unusable"
  }

  // Reactions to menu
  def goBitcoin(top: View) = {
    me goTo classOf[LNOpsActivity]
    fab close true
  }

  def goQR(top: View) = {
    me goTo classOf[ScanActivity]
    fab close true
  }

  // UI MENUS

  def makePaymentRequest = {
    val humanCap = sumIn format withSign(maxHtlcValue)
    val title = getString(ln_receive_max_amount).format(humanCap).html
    val content = getLayoutInflater.inflate(R.layout.frag_input_receive_ln, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), title, content)
    val rateManager = new RateManager(content)

    def attempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) => println(ms)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(attempt)
  }

  def goReceive(top: View) = {
    me delayUI makePaymentRequest
    fab close true
  }

  // USER CAN SET THEIR OWN PATHFINDER

  class SetBackupServer { self =>
    val (view, field) = str2Tuple(cloudPrivateKey.publicKey.toString)
    val dialog = mkChoiceDialog(proceed, none, dialog_next, dialog_cancel)
    val alert = mkForm(dialog, getString(ln_backup_key).html, view)
    field setTextIsSelectable true

    def proceed: Unit = rm(alert) {
      val (view1, field1) = generatePasswordPromptView(inpType = textType, txt = ln_backup_ip)
      val dialog = mkChoiceDialog(trySave(field1.getText.toString), none, dialog_ok, dialog_cancel)
      PrivateDataSaver.tryGetObject.foreach(field1 setText _.url)
      mkForm(dialog, me getString ln_backup, view1)
    }

    def trySave(url: String) =
      if (url.isEmpty) PrivateDataSaver.remove
      else if (URLUtil isValidUrl url) self save PrivateData(Nil, url)
      else mkForm(me negBld dialog_ok, null, me getString ln_backup_url_error)

    def save(privateData: PrivateData) = {
      PrivateDataSaver.saveObject(data = privateData)
      //pathfinder = getPathfinder(activeKits.head.chan)
      app toast ln_backup_success
    }

    def onError(error: Throwable): Unit = error.getMessage match {
      case "keynotfound" => mkForm(me negBld dialog_ok, null, me getString ln_backup_key_error)
      case "siginvalid" => mkForm(me negBld dialog_ok, null, me getString ln_backup_sig_error)
      case _ => mkForm(me negBld dialog_ok, null, me getString ln_backup_net_error)
    }
  }

  def closeChannel = checkPass(me getString ln_close) { pass =>
    for (chan <- app.ChannelManager.alive) chan async CMDShutdown
  }
}