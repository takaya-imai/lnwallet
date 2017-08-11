package com.lightning.wallet

import java.util.Date

import com.lightning.wallet.R.string._
import com.lightning.wallet.Utils._
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
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
import android.app.AlertDialog
import com.lightning.wallet.ln.PaymentInfo._
import com.github.clans.fab.FloatingActionMenu
import android.support.v4.view.MenuItemCompat
import android.support.v7.widget.SearchView.OnQueryTextListener
import android.webkit.URLUtil
import com.lightning.wallet.helper.{ReactCallback, ReactLoader, RichCursor}
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.ln.LNParams.getPathfinder
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Tools.{none, random}
import scala.concurrent.duration._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import fr.acinq.bitcoin.Crypto.sha256
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
  def readEmptyNdefMessage = app toast nfc_error
  def readNonNdefMessage = app toast nfc_error
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none
  def checkTransData: Unit

  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }
}

class LNActivity extends DataReader
with ToolbarActivity with HumanTimeDisplay
with ListUpdater with SearchBar { me =>

  lazy val fab = findViewById(R.id.fab).asInstanceOf[FloatingActionMenu]
  lazy val paymentProvider = new PaymentsDataProvider
  lazy val lnTitle = me getString ln_title
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

  class LNView(view: View) extends TxViewHolder(view) {
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
    if (app.isAlive) {
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
    } else me exitTo classOf[MainActivity]
  }

  override def onResume =
    wrap(run = super.onResume) {
      app.ChannelManager.alive.headOption match {
        case Some(chan) => pathfinder = getPathfinder(chan)
        case None => me goTo classOf[LNOpsActivity]
      }

      checkTransData
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
    case pr: PaymentRequest => sendPayment(pr)

    case unusable =>
      app.TransData.value = null
      Tools log s"Unusable $unusable"
  }

  private def sendPayment(pr: PaymentRequest) = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), me getString ln_receive_title, content)
    println(pr)
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
    val content = getLayoutInflater.inflate(R.layout.frag_input_receive_ln, null, false)
    val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), me getString ln_receive_title, content)

    val (canReceiveMsat, _) = receiveSendStatus
    val maxValue = MilliSatoshi apply math.min(canReceiveMsat, maxHtlcValue.amount)
    val rateManager = new RateManager(getString(satoshi_hint_max_amount) format withSign(maxValue), content)
    def onFail(error: Throwable): Unit = mkForm(me negBld dialog_ok, null, error.getMessage)

    def proceed(sum: Option[MilliSatoshi], preimage: BinaryData) = {
      val description: String = inputDescription.getText.toString.trim
      val paymentRequest = PaymentRequest(chainHash, sum, sha256(preimage), nodePrivateKey, description, None, 3600 * 24)
      PaymentInfoWrap putPaymentInfo IncomingPayment(preimage, paymentRequest, pathfinder.channel.id.get, HIDDEN)
      app.TransData.value = paymentRequest
      me goTo classOf[RequestActivity]
    }

    val go: Option[MilliSatoshi] => Unit = sumOption => {
      add(me getString tx_pr_make, Informer.LNREQUEST).ui.run
      <(proceed(sumOption, bag.newPreimage), onFail)(none)
      timer.schedule(me del Informer.LNREQUEST, 2500)
    }

    def attempt = rateManager.result match {
      case Success(ms) if ms.amount < htlcMinimumMsat => app toast dialog_sum_dusty
      case Success(ms) if ms > maxValue => app toast dialog_capacity
      case ok @ Success(ms) => rm(alert)(go apply ok.toOption)
      case _ => rm(alert)(go apply None)
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

    private def proceed: Unit = rm(alert) {
      val (view1, field1) = generatePasswordPromptView(inpType = textType, txt = ln_backup_ip)
      val dialog = mkChoiceDialog(trySave(field1.getText.toString), none, dialog_ok, dialog_cancel)
      PrivateDataSaver.tryGetObject.foreach(field1 setText _.url)
      mkForm(dialog, me getString ln_backup, view1)
    }

    private def trySave(url: String) = delayUI {
      if (url.isEmpty) PrivateDataSaver.remove
      else self check PrivateData(Nil, url)
    }

    private def check(data: PrivateData) = {
      val pathfinder1 = getPrivatePathfinder(pathfinder.channel)(data)
      val payload = pathfinder1.signedParams(random getBytes 32)
      pathfinder1.lnCloud.call("check", none, payload:_*)
        .subscribe(_ => self save data, onUIError)
    }

    private def save(privateData: PrivateData) = {
      pathfinder = getPathfinder(pathfinder.channel)
      PrivateDataSaver saveObject privateData
      app toast ln_backup_success
    }

    private def onUIError(e: Throwable) = me runOnUiThread onError(e)
    private def onError(error: Throwable): Unit = error.getMessage match {
      case "keynotfound" => mkForm(me negBld dialog_ok, null, me getString ln_backup_key_error)
      case "siginvalid" => mkForm(me negBld dialog_ok, null, me getString ln_backup_sig_error)
      case _ => mkForm(me negBld dialog_ok, null, me getString ln_backup_net_error)
    }
  }

  def closeChannel = checkPass(me getString ln_close) { pass =>
    for (chan <- app.ChannelManager.alive) chan async CMDShutdown
  }

  private def receiveSendStatus: (Long, Long) =
    Some(pathfinder.channel.data) collect { case norm: NormalData =>
      val canReceiveAmount = norm.commitments.localCommit.spec.toRemoteMsat
      val canSendAmount = norm.commitments.localCommit.spec.toLocalMsat
      canReceiveAmount -> canSendAmount
    } getOrElse 0L -> 0L
}