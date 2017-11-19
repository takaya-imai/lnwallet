package com.lightning.wallet

import android.widget._
import org.bitcoinj.core._
import collection.JavaConverters._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import android.provider.Settings.{System => FontSystem}
import com.lightning.wallet.ln.{PaymentRequest, Tools}
import android.view.{Menu, MenuItem, View, ViewGroup}
import scala.util.{Failure, Success, Try}

import android.widget.AbsListView.OnScrollListener.SCROLL_STATE_IDLE
import org.bitcoinj.core.TransactionConfidence.ConfidenceType.DEAD
import android.text.format.DateUtils.getRelativeTimeSpanString
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.content.DialogInterface.BUTTON_POSITIVE
import android.widget.AbsListView.OnScrollListener
import com.lightning.wallet.ln.LNParams.minDepth
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

trait ListUpdater extends HumanTimeDisplay { me: TimerActivity =>
  lazy val allTxsButton = getLayoutInflater.inflate(R.layout.frag_txs_all, null)
  lazy val toggler = allTxsButton.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  lazy val list = findViewById(R.id.itemsList).asInstanceOf[ListView]
  private[this] var state = SCROLL_STATE_IDLE
  val maxLinesNum = 24
  val minLinesNum = 4

  def startListUpdates(adapter: BaseAdapter) =
    list setOnScrollListener new OnScrollListener {
      def onScroll(v: AbsListView, first: Int, visible: Int, total: Int) = none
      def onScrollStateChanged(v: AbsListView, newState: Int) = state = newState
      def maybeUpdate = if (SCROLL_STATE_IDLE == state) adapter.notifyDataSetChanged
      timer.schedule(anyToRunnable(maybeUpdate), 10000, 10000)
      allTxsButton setVisibility View.GONE
      list addFooterView allTxsButton
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

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val view = if (null == savedView) getLayoutInflater.inflate(txLineType, null) else savedView
      val hold = if (null == view.getTag) getHolder(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView visibleItems(position)
      view
    }

    def getHolder(view: View): TxViewHolder
    abstract class TxViewHolder(view: View) {
      val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
      val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
      val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
      transactSum setTypeface Typeface.MONOSPACE
      def fillView(data: T): Unit
      view setTag this
    }
  }
}

class BtcActivity extends DataReader with ToolbarActivity with ListUpdater { me =>
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
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(wrap: TxWrap) = {
        val statusImage = if (wrap.tx.getConfidence.getConfidenceType == DEAD) dead
          else if (wrap.tx.getConfidence.getDepthInBlocks >= minDepth) conf1
          else await

        val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
        val finalSum = if (wrap.nativeValue.isPositive) coin2MSat(wrap.nativeValue)
          else coin2MSat(wrap.fee map wrap.nativeValue.add getOrElse wrap.nativeValue)

        transactWhen setText when(System.currentTimeMillis, wrap.tx.getUpdateTime).html
        transactSum setText marking.format(denom formatted finalSum).html
        transactCircle setImageResource statusImage
      }
    }
  }

  private[this] val txsTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = me runOnUiThread tell(tx)
    override def coinsReceived(tx: Transaction): Unit = me runOnUiThread tell(tx)
    override def txConfirmed(tx: Transaction) = me runOnUiThread adapter.notifyDataSetChanged

    def tell(wrap: TxWrap) =
      if (!wrap.nativeValue.isZero) {
        mnemonicWarn setVisibility View.GONE
        adapter.set(wrap +: adapter.availableItems)
        adapter.notifyDataSetChanged
      }
  }

  def updTitle = coin2MSat(app.kit.currentBalance) match { case msat =>
    val text = if (msat.amount < 1) walletEmpty else denom withSign msat
    animateTitle(text)
  }

  def notifySubTitle(sub: String, infoType: Int) = {
    // Here we update not just subtitle but also a title
    wrap(updTitle)(add(sub, infoType).flash.run)
    timer.schedule(delete(infoType), 8000)
  }

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =

    if (app.isAlive) {
      super.onCreate(savedInstanceState)

      // Set action bar, main view content, animate title, wire up list events
      wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_btc)
      wrap(updTitle)(add(me getString constListener.status, Informer.PEER).flash.run)
      me startListUpdates adapter
      me setDetecting true

      toolbar setOnClickListener onButtonTap {
        wrap(adapter.notifyDataSetChanged)(changeDenom)
        updTitle
      }

      list setAdapter adapter
      list setFooterDividersEnabled false
      list setOnItemClickListener onTap { pos =>
        val lst = getLayoutInflater.inflate(R.layout.frag_center_list, null).asInstanceOf[ListView]
        val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_transaction_details, null)
        val outside = detailsWrapper.findViewById(R.id.viewTxOutside).asInstanceOf[Button]

        val wrap = adapter getItem pos
        val marking = if (wrap.nativeValue.isPositive) sumIn else sumOut
        val confirms = app.plurOrZero(txsConfs, wrap.tx.getConfidence.getDepthInBlocks)
        val outputs = wrap.payDatas(wrap.nativeValue.isPositive).flatMap(_.toOption)
        val humanViews = for (payData <- outputs) yield payData.cute(marking).html

        // Wire up a popup list
        lst setAdapter new ArrayAdapter(me, R.layout.frag_top_tip, R.id.actionTip, humanViews.toArray)
        lst setOnItemClickListener onTap { position => outputs(position - 1).onClick }
        lst setHeaderDividersEnabled false
        lst addHeaderView detailsWrapper

        outside setOnClickListener onButtonTap {
          val blockcypher = "https://live.blockcypher.com/btc-testnet/tx/"
          val uri = Uri.parse(blockcypher + wrap.tx.getHashAsString)
          me startActivity new Intent(Intent.ACTION_VIEW, uri)
        }

        wrap.fee match {
          case _ if wrap.tx.getConfidence.getConfidenceType == DEAD =>
            mkForm(me negBld dialog_ok, sumOut.format(txsConfs.last).html, lst)

          case _ if wrap.nativeValue.isPositive =>
            val details = feeIncoming.format(confirms)
            mkForm(me negBld dialog_ok, details.html, lst)

          case Some(fee) =>
            val details = feeDetails.format(marking.format(denom withSign fee), confirms)
            mkForm(me negBld dialog_ok, humanFiat(details, fee).html, lst)

          case None =>
            val details = feeAbsent.format(confirms).html
            mkForm(me negBld dialog_ok, details, lst)
        }
      }

      // Wait for transactions list
      <(nativeTransactions, onFail) { txs =>
        app.kit.wallet addCoinsSentEventListener txsTracker
        app.kit.wallet addCoinsReceivedEventListener txsTracker
        app.kit.wallet addTransactionConfidenceEventListener txsTracker
        if (txs.isEmpty) mnemonicWarn setVisibility View.VISIBLE
        mnemonicInfo setText getString(mnemonic_info).html
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

  override def onResume: Unit = wrap(super.onResume) {
    app.prefs.edit.putString(AbstractKit.LANDING, AbstractKit.BITCOIN).commit
    checkTransData
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData =
    app.TransData.value match {
      case pr: PaymentRequest =>
        me goTo classOf[LNActivity]

      case uri: BitcoinURI =>
        app.TransData.value = null
        val amt: TryMSat = Try(uri.getAmount)
        sendBtcTxPopup.set(amt, uri.getAddress)

      case adr: Address =>
        app.TransData.value = null
        sendBtcTxPopup setAddress adr

      case unusable =>
        app.TransData.value = null
        Tools log s"Unusable $unusable"
    }

  // Get bitcoins
  private def localBitcoinsAndGlidera = {
    val uri = Uri parse "https://testnet.coinfaucet.eu/en/"
    me startActivity new Intent(Intent.ACTION_VIEW, uri)
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

    def sendAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case _ if spendManager.getAddress == null => app toast dialog_address_wrong
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case ok @ Success(ms) =>
        val processor = new TxProcessor {
          val pay = AddrData(ms, spendManager.getAddress)

          override def processTx(pass: String, fee: Coin) = {
            <(app.kit blockingSend makeTx(pass, fee), onTxFail)(Tools.log)
            add(me getString tx_announcing, Informer.BTCEVENT).flash.run
          }

          override def onTxFail(err: Throwable) =
            mkForm(mkChoiceDialog(me delayUI sendBtcTxPopup.set(ok, pay.address), none,
              dialog_ok, dialog_cancel), title = null, messageWhenMakingTx apply err)
        }

        // Initiate the spending sequence
        rm(alert)(processor.chooseFee)
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(sendAttempt)
    spendManager
  }

  def goPay(top: View) = {
    delayUI(sendBtcTxPopup)
    fab close true
  }

  def viewMnemonic(top: View) = checkPass(me getString sets_mnemonic)(doViewMnemonic)
  def nativeTransactions = app.kit.wallet.getRecentTransactions(maxLinesNum, false).asScala
    .toVector.map(bitcoinjTx2Wrap).filterNot(_.nativeValue.isZero)
}