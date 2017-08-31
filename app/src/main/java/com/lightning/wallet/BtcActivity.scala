package com.lightning.wallet

import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lncloud.ImplicitConversions._

import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import android.provider.Settings.{System => FontSystem}
import android.view.{Menu, MenuItem, View, ViewGroup}
import scala.util.{Failure, Success, Try}

import android.widget.AbsListView.OnScrollListener.SCROLL_STATE_IDLE
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import android.text.format.DateUtils.getRelativeTimeSpanString
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.content.DialogInterface.BUTTON_POSITIVE
import android.widget.AbsListView.OnScrollListener
import com.lightning.wallet.ln.LNParams.minDepth
import com.lightning.wallet.ln.PaymentRequest
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import java.text.SimpleDateFormat
import android.graphics.Typeface
import android.content.Intent
import android.os.Bundle
import android.net.Uri


trait HumanTimeDisplay { me: TimerActivity =>
  import R.layout.{frag_transfer_line_wide, frag_transfer_line_narrow}
  lazy val time = (dt: java.util.Date) => new SimpleDateFormat(timeString) format dt
  lazy val bigFont = FontSystem.getFloat(getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val (txLineType, timeString) = DateFormat is24HourFormat me match {
    case false if scrWidth < 2.2 & bigFont => (frag_transfer_line_wide, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false if scrWidth < 2.2 => (frag_transfer_line_narrow, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case false if scrWidth < 2.5 & bigFont => (frag_transfer_line_wide, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false if scrWidth < 2.5 => (frag_transfer_line_narrow, "MM/dd/yy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case false if bigFont => (frag_transfer_line_narrow, "MMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'")
    case false => (frag_transfer_line_narrow, "MMMM dd, yyyy' <font color=#999999>'h:mm'<small>'a'</small></font>'")

    case true if scrWidth < 2.2 & bigFont => (frag_transfer_line_wide, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true if scrWidth < 2.2 => (frag_transfer_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")

    case true if scrWidth < 2.4 & bigFont => (frag_transfer_line_wide, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true if scrWidth < 2.5 => (frag_transfer_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")

    case true if bigFont => (frag_transfer_line_narrow, "d MMM yyyy' <font color=#999999>'HH:mm'</font>'")
    case true => (frag_transfer_line_narrow, "d MMMM yyyy' <font color=#999999>'HH:mm'</font>'")
  }

  // Relative or absolute date
  def when(now: Long, date: java.util.Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString else time(date)
  }
}

trait ListUpdater { me: TimerActivity =>
  private[this] var state = SCROLL_STATE_IDLE
  lazy val allTxsButton = getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val toggler = allTxsButton.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  val maxLinesNum = 25
  val minLinesNum = 5

  def startListUpdates(adapter: BaseAdapter) =
    list setOnScrollListener new OnScrollListener {
      def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = none
      def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
      def maybeUpdate = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
      timer.schedule(anyToRunnable(maybeUpdate), 10000, 10000)
      allTxsButton setVisibility View.GONE
      list addFooterView allTxsButton
    }

  class TxViewHolder(view: View) {
    val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
    val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
    val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
    transactSum setTypeface Typeface.MONOSPACE
    view setTag this
  }

  abstract class CutAdapter[T] extends BaseAdapter {
    // Automatically manages switching list view from short to long and back
    def switch = cut = if (cut == minLinesNum) maxLinesNum else minLinesNum
    def getItemId(position: Int): Long = position
    def getCount: Int = visibleItems.size

    var cut: Int = minLinesNum
    var visibleItems = Vector.empty[T]
    var availableItems = Vector.empty[T]

    val set: Vector[T] => Unit = items1 => {
      val visibility = if (items1.size > minLinesNum) View.VISIBLE else View.GONE
      val resource = if (cut == minLinesNum) R.drawable.ic_expand_more_black_24dp
        else R.drawable.ic_expand_less_black_24dp

      allTxsButton setVisibility visibility
      toggler setImageResource resource
      visibleItems = items1 take cut
      availableItems = items1
    }
  }
}

class BtcActivity extends DataReader
with ToolbarActivity with HumanTimeDisplay
with ListUpdater { me =>

  lazy val fab = findViewById(R.id.fab).asInstanceOf[com.github.clans.fab.FloatingActionMenu]
  lazy val mnemonicWarn = findViewById(R.id.mnemonicWarn).asInstanceOf[LinearLayout]
  lazy val mnemonicInfo = me clickableTextField findViewById(R.id.mnemonicInfo)
  lazy val txsConfs = getResources getStringArray R.array.txs_confs
  lazy val feeIncoming = getString(txs_fee_incoming)
  lazy val feeDetails = getString(txs_fee_details)
  lazy val feeAbsent = getString(txs_fee_absent)
  lazy val walletEmpty = getString(wallet_empty)

  lazy val adapter = new CutAdapter[TxWrap] {
    def getItem(position: Int) = visibleItems(position)
    def getView(position: Int, cv: View, parent: ViewGroup) = {
      val view = if (null == cv) getLayoutInflater.inflate(txLineType, null) else cv
      val hold = if (null == view.getTag) new BtcView(view) else view.getTag.asInstanceOf[BtcView]
      hold fillView getItem(position)
      view
    }
  }

  private[this] val txsTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = me runOnUiThread tell(tx)
    override def coinsReceived(tx: Transaction): Unit = me runOnUiThread tell(tx)
    override def txConfirmed(tx: Transaction) = me runOnUiThread adapter.notifyDataSetChanged

    def tell(wrap: TxWrap) = if (!wrap.nativeValue.isZero) {
      // Only update interface if this is not a watched transaction
      // and ESTIMATED_SPENDABLE takes care of correct balance

      mnemonicWarn setVisibility View.GONE
      adapter.set(wrap +: adapter.availableItems)
      adapter.notifyDataSetChanged
    }
  }

  def updateTitleAndSub(sub: String, infoType: Int) =
    coin2MSat(app.kit.currentBalance) match { case msat =>
      val title = if (msat.amount < 1) walletEmpty else denom withSign msat
      runOnUiThread(getSupportActionBar setTitle title)
      add(sub, infoType).animate
    }

  def notifySubTitle(sub: String, infoType: Int) = {
    // Here we update not just subtitle but also a title
    timer.schedule(delete(infoType).animate, 20000)
    me.updateTitleAndSub(sub, infoType)
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    if (app.isAlive) {
      super.onCreate(savedInstanceState)
      app.prefs.edit.putString(AbstractKit.LANDING, AbstractKit.BITCOIN).commit
      wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_btc)
      updateTitleAndSub(constListener.mkTxt, Informer.PEER)
      me startListUpdates adapter
      me setDetecting true

      toolbar setOnClickListener onButtonTap {
        wrap(adapter.notifyDataSetChanged)(changeDenom)
        updateTitleAndSub(constListener.mkTxt, Informer.PEER)
      }

      list setAdapter adapter
      list setFooterDividersEnabled false
      list setOnItemClickListener onTap { pos =>
        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
        val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

        val wrap = adapter getItem pos
        val confirms = app.plurOrZero(txsConfs, wrap.tx.getConfidence.getDepthInBlocks)
        val objects = wrap.payDatas.flatMap(_.toOption).map(_.cute(wrap.marking).html).toArray
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, objects)
        lst setHeaderDividersEnabled false
        lst addHeaderView detailsWrapper

        outside setOnClickListener onButtonTap {
          val uri = "https://blockexplorer.com/tx/" + wrap.tx.getHashAsString
          me startActivity new Intent(Intent.ACTION_VIEW, Uri parse uri)
        }

        wrap.fee match {
          case _ if wrap.tx.getConfidence.getConfidenceType == DEAD => mkForm(me negBld dialog_ok, title = sumOut.format("x").html, lst)
          case _ if wrap.nativeValue.isPositive => mkForm(me negBld dialog_ok, title = feeIncoming.format(confirms).html, content = lst)
          case Some(fee) => mkForm(me negBld dialog_ok, humanFiat(feeDetails.format(confirms, denom withSign fee), fee).html, lst)
          case None => mkForm(me negBld dialog_ok, feeAbsent.format(confirms).html, lst)
        }
      }

      // Wait for transactions list
      <(nativeTransactions, onFail) { txs =>
        app.kit.wallet addCoinsSentEventListener txsTracker
        app.kit.wallet addCoinsReceivedEventListener txsTracker
        app.kit.wallet addTransactionConfidenceEventListener txsTracker
        if (txs.isEmpty) mnemonicWarn setVisibility View.VISIBLE
        adapter set txs
      }

      // Wire up general listeners
      app.kit.wallet addCoinsSentEventListener txTracker
      app.kit.wallet addCoinsReceivedEventListener txTracker
      app.kit.peerGroup addBlocksDownloadedEventListener catchListener
      app.kit.wallet addTransactionConfidenceEventListener txTracker
      app.kit.peerGroup addDisconnectedEventListener constListener
      app.kit.peerGroup addConnectedEventListener constListener
    } else me exitTo classOf[MainActivity]
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeTransactionConfidenceEventListener txsTracker
    app.kit.wallet removeCoinsReceivedEventListener txsTracker
    app.kit.wallet removeCoinsSentEventListener txsTracker

    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    app.kit.wallet removeTransactionConfidenceEventListener txTracker
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

  override def onResume: Unit =
    wrap(super.onResume)(checkTransData)

  // DATA READING AND BUTTON ACTIONS

  def checkTransData =
    app.TransData.value match {
      case pr: PaymentRequest =>
        me goTo classOf[LNActivity]

      case uri: BitcoinURI =>
        app.TransData.value = null
        val tryAmount = Try(uri.getAmount: MilliSatoshi)
        sendBtcTxPopup.set(tryAmount, uri.getAddress)

      case adr: Address =>
        app.TransData.value = null
        sendBtcTxPopup setAddress adr

      case unusable =>
        app.TransData.value = null
        println(s"Unusable $unusable")
    }

  // Buy bitcoins
  private def localBitcoinsAndGlidera = {
    val msg = getString(buy_info).format(app.kit.currentAddress.toString)
    mkForm(me negBld dialog_cancel, me getString action_buy, msg.html)
  }

  def toggle(v: View) = {
    // Expand or collapse all txs
    // adapter contains all history

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  // Reactions to menu buttons

  def goQR(top: View) = {
    val activity = classOf[ScanActivity]
    delayUI(me goTo activity)
    fab close true
  }

  def goLN(top: View) = {
    val activity = classOf[LNActivity]
    delayUI(me goTo activity)
    fab close true
  }

  def goReceiveBtcAddress(top: View) = {
    app.TransData.value = app.kit.currentAddress
    val activity = classOf[RequestActivity]
    delayUI(me goTo activity)
    fab close true
  }

  def sendBtcTxPopup: BtcManager = {
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_btc, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), me getString action_bitcoin_send, content)
    val rateManager = new RateManager(getString(amount_hint_wallet).format(denom withSign app.kit.currentBalance), content)
    val spendManager = new BtcManager(rateManager)

    def attempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case ok @ Success(ms) =>
        val processor = new TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)

          override def processTx(pass: String, fee: Coin) = {
            <(app.kit blockingSend makeTx(pass, fee), onTxFail)(none)
            add(getString(tx_announce), Informer.BTCEVENT).animate
          }

          override def onTxFail(err: Throwable) =
            mkForm(mkChoiceDialog(me delayUI sendBtcTxPopup.set(ok, pay.address), none,
              dialog_ok, dialog_cancel), null, messageWhenMakingTx apply err)
        }

        // Initiate the spending sequence
        rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(attempt)
    spendManager
  }

  def goPay(top: View) = {
    delayUI(sendBtcTxPopup)
    fab close true
  }

  def viewMnemonic(top: View) = checkPass(me getString sets_mnemonic)(doViewMnemonic)
  def nativeTransactions = app.kit.wallet.getTransactionsByTime.asScala.take(maxLinesNum)
    .toVector.map(bitcoinjTx2Wrap).filterNot(_.nativeValue.isZero)

  class BtcView(view: View) extends TxViewHolder(view) {
    // Display given Bitcoin transaction properties to user

    def fillView(wrap: TxWrap) = {
      val statusImage = if (wrap.tx.getConfidence.getConfidenceType == DEAD) dead
        else if (wrap.tx.getConfidence.getDepthInBlocks >= minDepth) conf1
        else await

      transactWhen setText when(System.currentTimeMillis, wrap.tx.getUpdateTime).html
      transactSum setText wrap.marking.format(denom formatted wrap.nativeValueWithoutFee).html
      transactCircle setImageResource statusImage
    }
  }
}