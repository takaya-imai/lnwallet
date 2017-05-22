package com.lightning.wallet

import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import org.bitcoinj.core.listeners._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._

import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import android.provider.Settings.{System => FontSystem}
import com.lightning.wallet.Utils.{app, sumIn, sumOut}
import android.view.{Menu, MenuItem, View, ViewGroup}
import scala.util.{Failure, Success, Try}

import com.lightning.wallet.lncloud.ImplicitConversions.string2Ops
import android.text.format.DateUtils.getRelativeTimeSpanString
import org.ndeftools.util.activity.NfcReaderActivity
import android.widget.AbsListView.OnScrollListener
import com.lightning.wallet.ln.LNParams.minDepth
import android.text.method.LinkMovementMethod
import android.text.format.DateFormat
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import org.bitcoinj.wallet.Wallet
import android.graphics.Typeface
import scala.collection.mutable
import android.content.Intent
import org.ndeftools.Message
import android.os.Bundle
import android.net.Uri

import android.widget.AbsListView.OnScrollListener.SCROLL_STATE_IDLE
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import android.content.DialogInterface.BUTTON_POSITIVE
import Transaction.MIN_NONDUST_OUTPUT


trait HumanTimeDisplay { me: TimerActivity =>
  import R.layout.{frag_transaction_line_wide, frag_transaction_line_narrow}
  lazy val time = (dt: java.util.Date) => new SimpleDateFormat(timeString) format dt
  lazy val bigFont = FontSystem.getFloat(getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val (txLineType, timeString) = DateFormat is24HourFormat me match {
    case false if scrWidth < 2.2 & bigFont => (frag_transaction_line_wide, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false if scrWidth < 2.2 => (frag_transaction_line_narrow, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case false if scrWidth < 2.5 & bigFont => (frag_transaction_line_wide, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false if scrWidth < 2.5 => (frag_transaction_line_narrow, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case false if bigFont => (frag_transaction_line_narrow, "MMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false => (frag_transaction_line_narrow, "MMMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case true if scrWidth < 2.2 & bigFont => (frag_transaction_line_wide, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true if scrWidth < 2.2 => (frag_transaction_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")

    case true if scrWidth < 2.4 & bigFont => (frag_transaction_line_wide, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true if scrWidth < 2.5 => (frag_transaction_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")

    case true if bigFont => (frag_transaction_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true => (frag_transaction_line_narrow, "d MMMM yyyy' <font color=#999999>'HH:mm'</font>'")
  }

  // Relative or absolute date
  def when(now: Long, dat: java.util.Date) = dat.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString else time(dat)
  }
}

trait ListUpdater { me: TimerActivity =>
  lazy val allTxsButton = getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val toggler = allTxsButton.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  lazy val minLinesNum = if (scrHeight < 4.8) 3 else 5
  private[this] var state = SCROLL_STATE_IDLE
  val maxLinesNum = 75

  def startListUpdates(adapter: BaseAdapter) = {
    list setOnScrollListener new OnScrollListener {
      def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = none
      def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
    }

    def go = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
    timer.schedule(anyToRunnable(process = go), 10000, 10000)
  }
}

abstract class TxViewHolder(v: View) {
  val transactCircle = v.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
  val transactWhen = v.findViewById(R.id.transactWhen).asInstanceOf[TextView]
  val transactSum = v.findViewById(R.id.transactSum).asInstanceOf[TextView]
  transactSum setTypeface Typeface.MONOSPACE
  v setTag this
}

class BtcActivity extends NfcReaderActivity
with InfoActivity with HumanTimeDisplay
with ListUpdater { me =>

  lazy val fab = findViewById(R.id.fab).asInstanceOf[com.github.clans.fab.FloatingActionMenu]
  lazy val mnemonicWarn = findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  lazy val mnemonicInfo = findViewById(R.id.mnemonicInfo).asInstanceOf[TextView]
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val feeIncoming = getString(txs_fee_incoming)
  lazy val feeDetails = getString(txs_fee_details)
  lazy val feeAbsent = getString(txs_fee_absent)
  lazy val walletEmpty = getString(wallet_empty)
  lazy val btcTitle = getString(txs_title)
  lazy val adapter = new BtcAdapter

  private[this] val txsTracker =
    new NativeTxTracker with TransactionConfidenceEventListener {
      override def nativeCoinsReceived(tx: Transaction, pb: Coin, nb: Coin) = me runOnUiThread tell(tx)
      override def nativeCoinsSent(tx: Transaction, pb: Coin, nb: Coin) = me runOnUiThread tell(tx)
      def onTransactionConfidenceChanged(wallet: Wallet, tx: Transaction) =
        if (tx.getConfidence.getDepthInBlocks <= minDepth)
          me runOnUiThread adapter.notifyDataSetChanged

      def tell(tx: Transaction) = {
        adapter.transactions prepend tx
        mnemonicWarn setVisibility View.GONE
        adapter.notifyDataSetChanged
      }
    }

  // Adapter for btc tx list
  class BtcAdapter extends BaseAdapter {
    def getView(transactionPosition: Int, cv: View, parent: ViewGroup) = {
      val view = if (null == cv) getLayoutInflater.inflate(me.txLineType, null) else cv
      val hold = if (null == view.getTag) new BtcView(view) else view.getTag.asInstanceOf[BtcView]
      hold fillView getItem(transactionPosition)
      view
    }

    var transactions = mutable.Buffer.empty[Transaction]
    def getItem(position: Int) = transactions(position)
    def getItemId(position: Int) = position
    def getCount = transactions.size
  }

  // Here we update both subtitle and title
  def updateTitleAndSub(sub: String, infoType: Int) =
    app.kit.currentBalance match { case balance: Coin =>
      val exactValue = btcTitle format coin2String(balance)
      val title = if (balance.isZero) walletEmpty else exactValue
      me runOnUiThread getSupportActionBar.setTitle(title.html)
      me runOnUiThread add(sub, infoType).ui
    }

  def notifySubTitle(subtitle: String, infoType: Int) = {
    me.updateTitleAndSub(subtitle, infoType)
    timer.schedule(me del infoType, 25000)
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    if (app.isAlive) {
      super.onCreate(savedInstanceState)
      wrap(initToolbar)(me setContentView R.layout.activity_btc)
      mnemonicInfo setMovementMethod LinkMovementMethod.getInstance
      updateTitleAndSub(constListener.mkTxt, Informer.PEER)
      fab setAnimated false
      setDetecting(true)

      list setOnItemClickListener onTap { pos =>
        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
        val confNumber = detailsWrapper.findViewById(R.id.confNumber).asInstanceOf[TextView]
        val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

        val tx = adapter getItem pos
        val coloring = paymentMarking(tx)
        val direction = value(tx).amount > 0

        val payDatas = tx.getOutputs.asScala.filter(_.isMine(app.kit.wallet) == direction).map(outputToPayData).flatMap(_.toOption)
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, payDatas.map(_ cute coloring).toArray)
        lst setHeaderDividersEnabled false
        lst addHeaderView detailsWrapper

        outside setOnClickListener onButtonTap {
          val uri = "https://blockexplorer.com/tx/" + tx.getHashAsString
          me startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
        }

        val sumPretty: String = coloring format withSign(me value tx)
        val title = s"$sumPretty<br><small>${me time tx.getUpdateTime}</small>"
        mkForm(me negBld dialog_ok, title.html, lst)
        confNumber setText status(tx).html
      }

      // Wait for transactions list
      <(nativeTransactions, onFail) { result =>
        app.kit.wallet addTransactionConfidenceEventListener txsTracker
        app.kit.wallet addCoinsReceivedEventListener txsTracker
        app.kit.wallet addCoinsSentEventListener txsTracker

        // Show limited txs list
        me startListUpdates adapter
        if (result.size > minLinesNum) list addFooterView allTxsButton
        else if (result.isEmpty) mnemonicWarn setVisibility View.VISIBLE
        adapter.transactions = result take minLinesNum
        list setFooterDividersEnabled false
        list setAdapter adapter
      }

      // Wire up general listeners
      app.kit.wallet addCoinsSentEventListener tracker
      app.kit.wallet addCoinsReceivedEventListener tracker
      app.kit.wallet addTransactionConfidenceEventListener tracker
      app.kit.peerGroup addBlocksDownloadedEventListener new CatchTracker
      app.kit.peerGroup addDisconnectedEventListener constListener
      app.kit.peerGroup addConnectedEventListener constListener
    } else me exitTo classOf[MainActivity]
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeTransactionConfidenceEventListener txsTracker
    app.kit.wallet removeCoinsReceivedEventListener txsTracker
    app.kit.wallet removeCoinsSentEventListener txsTracker

    app.kit.wallet removeCoinsSentEventListener tracker
    app.kit.wallet removeCoinsReceivedEventListener tracker
    app.kit.wallet removeTransactionConfidenceEventListener tracker
    app.kit.peerGroup removeDisconnectedEventListener constListener
    app.kit.peerGroup removeConnectedEventListener constListener
    stopDetecting
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.transactions_ops, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onOptionsItemSelected(menu: MenuItem) = runAnd(true) {
    if (menu.getItemId == R.id.actionBuyCoins) localBitcoinsAndGlidera
    else if (menu.getItemId == R.id.actionSettings) mkSetsForm
  }

  private def localBitcoinsAndGlidera = {
    val msg = getString(buy_info).format(app.kit.currentAddress.toString)
    mkForm(me negBld dialog_cancel, me getString action_buy, msg.html)
  }

  override def onResume = {
    wrap(super.onResume)(checkTransData)
    app.TransData.value = null
  }

  // NFC
  def readNdefMessage(msg: Message) = try {
    val asText = readFirstTextNdefMessage(msg)
    app.TransData recordValue asText
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }

  // Working with transitional data and NFC
  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => sendBtcTxPopup.set(Try(uri.getAmount), uri.getAddress)
    case bitcoinAddress: Address => sendBtcTxPopup setAddress bitcoinAddress
    case unusable => println(s"Unknown TransData: $unusable")
  }

  def onNfcStateEnabled = none
  def onNfcStateDisabled = none
  def onNfcFeatureNotFound = none
  def onNfcStateChange(ok: Boolean) = none
  def readNonNdefMessage = app toast nfc_error
  def readEmptyNdefMessage = app toast nfc_error

  // Expand all txs
  def toggle(v: View) = {
    allTxsButton setVisibility View.GONE
    <(nativeTransactions, onFail) { result =>
      val contracted = adapter.getCount <= minLinesNum
      if (contracted) showMore(result) else showLess(result)
      allTxsButton setVisibility View.VISIBLE
      adapter.notifyDataSetChanged
    }
  }

  type TransactionBuffer = mutable.Buffer[Transaction]
  private val showMore = (result: TransactionBuffer) => {
    toggler.setImageResource(R.drawable.ic_expand_less_black_24dp)
    adapter.transactions = result take maxLinesNum
  }

  private val showLess = (result: TransactionBuffer) => {
    toggler.setImageResource(R.drawable.ic_expand_more_black_24dp)
    adapter.transactions = result take minLinesNum
  }

  // Reactions to menu buttons
  def onFail(e: Throwable): Unit = mkForm(me negBld dialog_ok, null, e.getMessage)
  def viewMnemonic(top: View) = passPlus(me getString sets_mnemonic)(doViewMnemonic)

  def goQR(top: View) = {
    me goTo classOf[ScanActivity]
    fab close false
  }

  def goLN(top: View) = {
    me goTo classOf[LNActivity]
    fab close false
  }

  def goReceiveBtcAddress(top: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
    fab close false
  }

  def sendBtcTxPopup: BtcManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_send, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), me getString action_bitcoin_send, content)

    // Can input satoshis and address
    val rateManager = new RateManager(content)
    val spendManager = new BtcManager(rateManager)

    def attempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_dusty
      case Success(_) if spendManager.getAddress == null => app toast dialog_addr_wrong

      case ok @ Success(ms) =>
        val processor = new TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)
          def processTx(password: String, fee: Coin): Unit = {
            <(app.kit blockingSend makeTx(password, fee), onTxFail)(none)
            add(me getString tx_announce, Informer.BTCEVENT).ui.run
          }

          def doOnError = rm(alert) {
            // User may want to try again
            sendBtcTxPopup.set(ok, pay.adr)
          }
        }

        // Initiate the spending sequence
        rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(attempt)
    spendManager
  }

  def goPay(top: View): Unit = {
    val run = anyToRunnable(sendBtcTxPopup)
    timer.schedule(run, 50)
    fab close false
  }

  // Working with transactions
  private def paymentMarking(tx: Transaction) = if (value(tx).amount > 0) sumIn else sumOut
  private def value(tx: Transaction) = coin2MilliSatoshi(tx getValue app.kit.wallet)

  private def status(trans: Transaction) = {
    val cfs = app.plurOrZero(txsConfs, trans.getConfidence.getDepthInBlocks)
    if (value(trans).amount > 0) feeIncoming format cfs else trans.getFee match {
      case null => feeAbsent format cfs case fee => feeDetails.format(withSign(fee), cfs)
    }
  }

  private def nativeTransactions =
    app.kit.wallet.getTransactionsByTime.asScala
      .take(maxLinesNum).filterNot(_ isWatched app.kit.wallet)

  private def outputToPayData(out: TransactionOutput) = Try(out.getScriptPubKey) map {
    case publicKeyScript if publicKeyScript.isSentToP2WSH => P2WSHData(out.getValue, publicKeyScript)
    case publicKeyScript => AddrData(out.getValue, publicKeyScript getToAddress app.params)
  }

  class BtcView(view: View)
  extends TxViewHolder(view) {
    def fillView(tx: Transaction) = {
      val time = when(System.currentTimeMillis, tx.getUpdateTime)
      val shortValue = paymentMarking(tx).format(value(tx): String)

      val image =
        if (tx.getConfidence.getConfidenceType == DEAD) dead
        else if (tx.getConfidence.getDepthInBlocks >= minDepth) conf1
        else await

      transactWhen setText time.html
      transactSum setText shortValue.html
      transactCircle setImageResource image
    }
  }
}