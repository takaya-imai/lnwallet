package com.lightning.wallet

import Utils._
import R.string._
import android.text._
import android.view._
import org.bitcoinj.core._
import org.bitcoinj.core.listeners._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.lncloud._
import org.bitcoinj.wallet.listeners._
import com.lightning.wallet.lncloud.ImplicitConversions._
import android.widget.{ArrayAdapter, LinearLayout, ListView, TextView}
import android.widget.{AdapterView, Button, EditText, RadioGroup}
import android.content.{Context, DialogInterface, Intent}
import com.lightning.wallet.ln.Tools.{none, runAnd, wrap}
import org.bitcoinj.wallet.{SendRequest, Wallet}
import scala.util.{Failure, Success, Try}
import android.app.{AlertDialog, Dialog}
import R.id.{typeCNY, typeEUR, typeUSD}
import java.util.{Timer, TimerTask}

import org.bitcoinj.wallet.Wallet.ExceededMaxTransactionSize
import org.bitcoinj.wallet.Wallet.CouldNotAdjustDownwards
import android.widget.RadioGroup.OnCheckedChangeListener
import info.hoang8f.android.segmented.SegmentedGroup
import concurrent.ExecutionContext.Implicits.global
import android.view.inputmethod.InputMethodManager
import com.lightning.wallet.ln.LNParams.minDepth
import android.support.v7.app.AppCompatActivity
import org.bitcoinj.crypto.KeyCrypterException
import android.text.method.LinkMovementMethod
import com.lightning.wallet.ln.Tools.random
import android.support.v7.widget.Toolbar
import android.view.View.OnClickListener
import com.lightning.wallet.ln.LNParams
import org.bitcoinj.store.SPVBlockStore
import android.app.AlertDialog.Builder
import fr.acinq.bitcoin.MilliSatoshi
import language.implicitConversions
import android.util.DisplayMetrics
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.script.Script
import scala.concurrent.Future
import android.os.Bundle

import ViewGroup.LayoutParams.WRAP_CONTENT
import InputMethodManager.HIDE_NOT_ALWAYS
import Context.INPUT_METHOD_SERVICE


object Utils { me =>
  type TryMSat = Try[MilliSatoshi]
  // Cannot have lazy var so use this
  var startupAppReference: WalletApp = _
  lazy val app = startupAppReference

  val passType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD
  val textType = InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD

  // Mapping from text to Android id integer
  val Seq(strDollar, strEuro, strYuan) = Seq("dollar", "euro", "yuan")
  val fiatMap = Map(typeUSD -> strDollar, typeEUR -> strEuro, typeCNY -> strYuan)
  val revFiatMap = Map(strDollar -> typeUSD, strEuro -> typeEUR, strYuan -> typeCNY)

  val appName = "Bitcoin"
  val nullFail = Failure(null)
  lazy val sumIn = app getString txs_sum_in
  lazy val sumOut = app getString txs_sum_out

  def humanPubkey(key: String) = key grouped 3 mkString "\u0020"
  def humanAddr(adr: Address) = s"$adr" grouped 4 mkString "\u0020"
  def humanFiat(prefix: String, ms: MilliSatoshi, div: String = "<br>"): String = inFiat(ms) match {
    case Success(amt) if currentFiatName == strYuan => s"$prefix$div<font color=#999999>≈ ${baseFiat format amt} CNY</font>"
    case Success(amt) if currentFiatName == strEuro => s"$prefix$div<font color=#999999>≈ ${baseFiat format amt} EUR</font>"
    case Success(amt) => s"$prefix$div<font color=#999999>≈ ${baseFiat format amt} USD</font>"
    case _ => prefix
  }

  // Fiat rates related functions, all transform a Try monad
  // Rate is fiat per BTC so we need to divide by btc factor in the end
  def currentFiatName: String = app.prefs.getString(AbstractKit.CURRENCY, strDollar)
  def inFiat(ms: MilliSatoshi) = currentRate.map(perBtc => ms.amount * perBtc / btcFactor)
  def currentRate: Try[Double] = Try(RatesSaver.rates exchange currentFiatName)
}

trait ToolbarActivity extends TimerActivity { me =>
  def initToolbar = me setSupportActionBar findViewById(R.id.toolbar).asInstanceOf[Toolbar]
  def tellWrongPass: Unit = wrap(app toast password_wrong)(mkSetsForm)
  def tellGenError: Unit = wrap(app toast err_general)(mkSetsForm)
  private[this] var currentAnimation = Option.empty[TimerTask]
  protected[this] var infos = List.empty[Informer]

  def animate = new Runnable { self =>
    private[this] val nextText = infos.head.value
    private[this] val currentText = getSupportActionBar.getSubtitle.toString
    private[this] val maxLength = math.max(nextText.length, currentText.length)

    for (an <- currentAnimation) an.cancel
    currentAnimation = Some apply uiTask(self)
    timer.schedule(currentAnimation.get, 0, 50)

    private[this] var index = 1
    override def run = getSupportActionBar match { case bar =>
      bar setSubtitle s"${nextText take index}${currentText drop index}".trim
      if (index < maxLength) index += 1 else for (an <- currentAnimation) an.cancel
    }
  }

  val catchListener = new BlocksListener {
    def getNextTracker(initBlocksLeft: Int) = new BlocksListener {
      def onBlocksDownloaded(peer: Peer, block: Block, fb: FilteredBlock, blocksLeft: Int) = {
        if (blocksLeft % blocksPerDay == 0) update(app.plurOrZero(syncOps, blocksLeft / blocksPerDay), Informer.CHAINSYNC)
        if (blocksLeft < 1) add(getString(info_progress_done), Informer.CHAINSYNC).delAndAnimate(Informer.CHAINSYNC, 3000)
        if (blocksLeft < 1) app.kit.peerGroup removeBlocksDownloadedEventListener this
        animate
      }

      // We only add a SYNC item if we have a large enough
      // lag (more than a day), otherwise no updates are visible
      private val syncOps = app.getResources getStringArray R.array.info_progress
      private val text = app.plurOrZero(syncOps, initBlocksLeft / blocksPerDay)
      if (initBlocksLeft > blocksPerDay) add(text, Informer.CHAINSYNC)
    }

    def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = {
      app.kit.peerGroup addBlocksDownloadedEventListener getNextTracker(left)
      app.kit.peerGroup removeBlocksDownloadedEventListener this
    }
  }

  val constListener =
    new PeerConnectedEventListener with PeerDisconnectedEventListener {
      def onPeerConnected(p: Peer, pc: Int) = update(mkTxt, Informer.PEER).animate
      def onPeerDisconnected(p: Peer, pc: Int) = update(mkTxt, Informer.PEER).animate
      def mkTxt = app.plurOrZero(peersInfoOpts, app.kit.peerGroup.numConnectedPeers)
      lazy val peersInfoOpts = getResources getStringArray R.array.info_peers
    }

  val txTracker = new TxTracker {
    override def coinsSent(tx: Transaction) = notifySubTitle(me getString tx_sent, Informer.BTCEVENT)
    override def coinsReceived(tx: Transaction) = notifySubTitle(me getString tx_received, Informer.BTCEVENT)
    override def txConfirmed(tx: Transaction) = notifySubTitle(me getString tx_confirmed, Informer.TXCONFIRMED)
  }

  // Informer CRUD
  def delAndAnimate(tag: Int, delay: Int) = {
    infos = infos.filterNot(_.tag == tag)
    timer.schedule(animate, delay)
  }

  def add(text: String, tag: Int) = runAnd(me) {
    infos = new Informer(text, tag) :: infos
  }

  def update(text: String, tag: Int) = runAnd(me) {
    for (inf <- infos if inf.tag == tag) inf.value = text
  }

  // Password checking popup
  def passPlus(title: CharSequence)(next: String => Unit) = {
    val (passAsk, secret) = generatePasswordPromptView(passType, password_old)
    mkForm(mkChoiceDialog(infoAndNext, none, dialog_next, dialog_cancel), title, passAsk)

    def infoAndNext = {
      add(app getString pass_checking, Informer.CODECHECK).animate
      delAndAnimate(Informer.CODECHECK, delay = 3000)
      next(secret.getText.toString)
    }
  }

  def checkPass(title: CharSequence)(next: String => Unit) = passPlus(title) { password =>
    <(app.kit.wallet checkPassword password, _ => tellGenError)(if (_) next(password) else tellWrongPass)
  }

  def doViewMnemonic(password: String) =
    <(Mnemonic decrypt password, _ => tellGenError) { seed =>
      // This method is used by both button on btc activity and settings menu
      mkForm(me negBld dialog_ok, me getString sets_noscreen, Mnemonic text seed)
    }

  def mkSetsForm: Unit = {
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = mkForm(me negBld dialog_cancel, getString(read_settings).html, form)
    val setBackupServer = form.findViewById(R.id.setBackupServer).asInstanceOf[Button]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]

    setBackupServer setOnClickListener onButtonTap {
      // Power users may provide their own backup servers
      rm(menu)(new SetBackupServer)
    }

    rescanWallet setOnClickListener onButtonTap {
      def openForm = checkPass(me getString sets_rescan) { _ =>
        mkForm(mkChoiceDialog(go, none, dialog_ok, dialog_cancel)
          setMessage sets_rescan_ok, null, null)
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0

      rm(menu)(openForm)
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Provided as an external function because may be accessed from main page
      def openForm = checkPass(me getString sets_mnemonic)(doViewMnemonic)
      rm(menu)(openForm)
    }

    changePass setOnClickListener onButtonTap {
      def openForm = checkPass(me getString sets_pass_change) { oldPass =>
        val (textAsk, secretField) = generatePasswordPromptView(textType, password_new)
        mkForm(mkChoiceDialog(proceed, none, dialog_ok, dialog_cancel), me getString sets_pass_change, textAsk)
        def proceed = if (newPass.length >= 6) changePassword else app toast password_too_short
        def newPass = secretField.getText.toString.trim

        def changePassword = {
          <(rotatePass, _ => System exit 0)(_ => app toast sets_password_ok)
          add(app getString pass_changing, Informer.CODECHECK).animate
          delAndAnimate(Informer.CODECHECK, 3000)
        }

        def rotatePass = {
          app.kit.wallet decrypt oldPass
          val (crypter, key) = app getCrypter newPass
          app.kit.wallet.encrypt(crypter, key)
        }
      }

      rm(menu)(openForm)
    }
  }

  class SetBackupServer { self =>
    val (view, field) = str2Tuple(LNParams.cloudPrivateKey.publicKey.toString)
    val dialog = mkChoiceDialog(proceed, none, dialog_next, dialog_cancel)
    val alert = mkForm(dialog, getString(ln_backup_key).html, view)
    field setTextIsSelectable true

    def proceed: Unit = rm(alert) {
      val (view1, field1) = generatePasswordPromptView(inpType = textType, txt = ln_backup_ip)
      val dialog = mkChoiceDialog(trySave(field1.getText.toString), none, dialog_ok, dialog_cancel)
      PrivateDataSaver.tryGetObject.foreach(field1 setText _.url)
      mkForm(dialog, me getString ln_backup, view1)
    }

    def trySave(url: String) = delayUI {
      if (url.isEmpty) PrivateDataSaver.remove
      else self check PrivateData(Nil, url)
    }

    def check(data: PrivateData) = {
      val failover = LNParams getFailoverCloud data
      val params = new PrivateStorage(failover).signedParams(random getBytes 32)
      failover.call("check", none, params:_*).subscribe(_ => self save data, onError)
    }

    def save(privateData: PrivateData) = {
      PrivateDataSaver saveObject privateData
      LNParams.resetCloudAndStorage
      app toast ln_backup_success
    }

    def onError(error: Throwable) = error.getMessage match {
      case "keynotfound" => onFail(me getString ln_backup_key_error)
      case "siginvalid" => onFail(me getString ln_backup_sig_error)
      case _ => onFail(me getString ln_backup_net_error)
    }
  }

  abstract class TxProcessor {
    def onTxFail(exc: Throwable): Unit
    def processTx(password: String, fee: Coin)
    val pay: PayData

    import RatesSaver.rates
    def chooseFee: Unit = passPlus(pay.cute(sumOut).html) { password =>
      <(makeTx(password, rates.feeRisky), onTxFail) { feeEstimate: Transaction =>
        val riskyFinalFee = rates.feeRisky multiply feeEstimate.unsafeBitcoinSerialize.length div 1000
        val liveFinalFee = rates.feeLive multiply feeEstimate.unsafeBitcoinSerialize.length div 1000

        // Mark fees as red because we are the ones who always pay them
        val riskyFeePretty = sumOut format withSign(riskyFinalFee)
        val liveFeePretty = sumOut format withSign(liveFinalFee)

        // Show formatted fees in satoshis as well as in current fiat value
        val feeRiskyComplete = getString(fee_risky) format humanFiat(riskyFeePretty, riskyFinalFee, " ")
        val feeLiveComplete = getString(fee_live) format humanFiat(liveFeePretty, liveFinalFee, " ")
        val feesOptions = Array(feeRiskyComplete.html, feeLiveComplete.html)

        val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
        val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]
        val slot = android.R.layout.select_dialog_singlechoice
        lst setAdapter new ArrayAdapter(me, slot, feesOptions)
        lst.setItemChecked(0, true)

        def proceed = processTx(password, if (lst.getCheckedItemPosition == 0) rates.feeRisky else rates.feeLive)
        lazy val dialog: Builder = mkChoiceDialog(rm(alert)(proceed), none, dialog_pay, dialog_cancel)
        lazy val alert = mkForm(dialog, getString(title_fee).format(pay cute sumOut).html, form)
        alert
      }
    }

    def makeTx(password: String, fee: Coin) = {
      val crypter = app.kit.wallet.getKeyCrypter
      val keyParameter = crypter deriveKey password
      val request = pay.sendRequest

      request.feePerKb = fee
      request.aesKey = keyParameter
      app.kit.wallet completeTx request
      request.tx
    }

    def errorWhenMakingTx: PartialFunction[Throwable, String] = {
      case insufficientMoneyException: InsufficientMoneyException =>
        val missing = withSign(insufficientMoneyException.missing)
        val balance = withSign(app.kit.currentBalance)
        val sending = withSign(pay.cn)

        val template = app getString err_not_enough_funds
        template.format(balance, sending, missing)

      case _: ExceededMaxTransactionSize => app getString err_transaction_too_large
      case _: CouldNotAdjustDownwards => app getString err_empty_shrunk
      case _: KeyCrypterException => app getString err_pass
      case _: Throwable => app getString err_general
    }
  }

  // Temporairly update subtitle info
  def notifySubTitle(subtitle: String, infoType: Int)
}

trait TimerActivity extends AppCompatActivity { me =>
  val goTo: Class[_] => Unit = me startActivity new Intent(me, _)
  val exitTo: Class[_] => Unit = goto => wrap(finish)(goTo apply goto)
  val timer = new Timer

  // Screen size in inches and prefs reference
  lazy val maxDialog = metrics.densityDpi * 2.1
  lazy val scrWidth = metrics.widthPixels.toDouble / metrics.densityDpi
  lazy val metrics = new DisplayMetrics match { case metrix =>
    getWindowManager.getDefaultDisplay getMetrics metrix
    metrix
  }

  // Basis for dialog forms
  def str2Tuple(textFieldData: CharSequence): (LinearLayout, TextView) = {
    val view = getLayoutInflater.inflate(R.layout.frag_top_tip, null).asInstanceOf[LinearLayout]
    val titleTextField = me clickableTextField view.findViewById(R.id.actionTip)
    titleTextField setText textFieldData
    view -> titleTextField
  }

  def generatePasswordPromptView(inpType: Int, txt: Int): (LinearLayout, EditText) = {
    val passAsk = getLayoutInflater.inflate(R.layout.frag_changer, null).asInstanceOf[LinearLayout]
    val secretInputField = passAsk.findViewById(R.id.secretInput).asInstanceOf[EditText]
    passAsk.findViewById(R.id.secretTip).asInstanceOf[TextView] setText txt
    secretInputField setInputType inpType
    passAsk -> secretInputField
  }

  def delayUI(fun: => Unit): Unit = timer.schedule(anyToRunnable(fun), 225)
  def rm(previous: Dialog)(fun: => Unit): Unit = wrap(previous.dismiss)(me delayUI fun)
  def mkErrorUiUnsafeForm(error: String): Unit = mkForm(me negBld dialog_ok, null, error)
  def onFail(error: String): Unit = me runOnUiThread mkErrorUiUnsafeForm(error)
  def onFail(error: Throwable): Unit = onFail(error.getMessage)

  def mkForm(builder: Builder, title: View, content: View) =
    showForm(builder.setCustomTitle(title).setView(content).create)

  def showForm(alertDialog: AlertDialog) = {
    alertDialog setCanceledOnTouchOutside false
    alertDialog.getWindow.getAttributes.windowAnimations = R.style.SlidingDialog
    try alertDialog.show catch none finally if (scrWidth > 2.3) alertDialog.getWindow
      .setLayout(maxDialog.toInt, WRAP_CONTENT)

    alertDialog
  }

  def negBld(neg: Int): Builder = new Builder(me).setNegativeButton(neg, null)
  def negPosBld(neg: Int, pos: Int): Builder = negBld(neg).setPositiveButton(pos, null)
  def mkChoiceDialog(ok: => Unit, no: => Unit, okResource: Int, noResource: Int): Builder = {
    val cancel = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = no }
    val again = new DialogInterface.OnClickListener { def onClick(x: DialogInterface, w: Int) = ok }
    new Builder(me).setPositiveButton(okResource, again).setNegativeButton(noResource, cancel)
  }

  // Show an emergency page in case of a fatal error
  override def onCreate(savedInstanceState: Bundle): Unit = {
    // TODO: remove in production
    //Thread setDefaultUncaughtExceptionHandler new UncaughtHandler(me)
    super.onCreate(savedInstanceState)
  }

  override def onDestroy = wrap(super.onDestroy)(timer.cancel)
  implicit def uiTask(process: => Runnable): TimerTask = new TimerTask { def run = me runOnUiThread process }
  implicit def str2View(res: CharSequence): LinearLayout = str2Tuple(res) match { case (view, _) => view }

  // Run computation in Future, deal with results on UI thread
  def <[T](fun: => T, no: Throwable => Unit)(ok: T => Unit) = <<(Future(fun), no)(ok)
  def <<[T](future: Future[T], no: Throwable => Unit)(ok: T => Unit) = future onComplete {
    case Success(rs) => runOnUiThread(ok apply rs) case Failure(ex) => runOnUiThread(no apply ex)
  }

  // Utils
  def hideKeys(fun: => Unit): Unit = try {
    val mgr = getSystemService(INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    mgr.hideSoftInputFromWindow(getCurrentFocus.getWindowToken, HIDE_NOT_ALWAYS)
  } catch none finally me delayUI fun

  def onTap(run: Int => Unit) = new AdapterView.OnItemClickListener {
    def onItemClick(p: AdapterView[_], v: View, pos: Int, id: Long) = run(pos)
  }

  def onButtonTap(run: => Unit) = new OnClickListener {
    def onClick(tappedButtonView: View) = me hideKeys run
  }

  def clickableTextField(view: View): TextView =
    view.asInstanceOf[TextView] match { case textView =>
      textView setMovementMethod LinkMovementMethod.getInstance
      textView
    }
}

class RateManager(extra: String, val content: View) { me =>
  val satInput = content.findViewById(R.id.inputAmount).asInstanceOf[EditText]
  val fiatType = content.findViewById(R.id.fiatType).asInstanceOf[SegmentedGroup]
  val fiatInput = content.findViewById(R.id.fiatInputAmount).asInstanceOf[EditText]
  def setSum(res: TryMSat) = satInput.setText(res map milliSatoshi2String getOrElse null)
  def result: TryMSat = Try apply satString2MilliSatoshi(satInput.getText.toString.noCommas)

  val fiatListener = new TextChangedWatcher {
    def fiatDecimal = BigDecimal(fiatInput.getText.toString.noCommas)
    def upd = setSum(currentRate.map(perBtc => fiatDecimal / perBtc) map btcBigDecimal2MilliSatoshi)
    def onTextChanged(s: CharSequence, start: Int, b: Int, c: Int) = if (fiatInput.hasFocus) upd
  }

  val bitListener = new TextChangedWatcher {
    def upd = fiatInput.setText(result flatMap inFiat map baseFiat.format getOrElse null)
    def onTextChanged(s: CharSequence, start: Int, b: Int, c: Int) = if (satInput.hasFocus) upd
  }

  fiatType setOnCheckedChangeListener new OnCheckedChangeListener {
    def onCheckedChanged(radioGroupView: RadioGroup, newFiatName: Int) = {
      app.prefs.edit.putString(AbstractKit.CURRENCY, fiatMap apply newFiatName).commit
      if (fiatInput.hasFocus) fiatListener.upd else bitListener.upd
      fiatInput setHint currentFiatName
    }
  }

  val hintMsat = content.findViewById(R.id.hintMsat).asInstanceOf[TextView]
  hintMsat setText app.getString(spend_amount_hint).format(extra).html

  satInput addTextChangedListener bitListener
  fiatInput addTextChangedListener fiatListener
  fiatType check revFiatMap(currentFiatName)
  satInput.requestFocus
}

class BtcManager(val man: RateManager) { me =>
  val addressData = man.content.findViewById(R.id.addressData).asInstanceOf[TextView]
  val addressPaste = man.content.findViewById(R.id.addressPaste).asInstanceOf[Button]
  def set(tm: TryMSat, adr: Address) = wrap(man setSum tm)(me setAddress adr)
  def getAddress = addressData.getTag.asInstanceOf[Address]

  addressPaste setOnClickListener new OnClickListener {
    def onClick(button: View) = try setAddress(app getTo app.getBuffer)
      catch { case _: Throwable => app toast dialog_address_absent }
  }

  def setAddress(adr: Address) = {
    addressPaste setVisibility View.GONE
    addressData setVisibility View.VISIBLE
    addressData setText humanAddr(adr)
    addressData setTag adr
  }
}


trait PayData {
  def destination: String
  def sendRequest: SendRequest
  def cn: Coin

  def cute(direction: String) = {
    val colored = direction format destination
    val top = colored + "<br><br>" + withSign(cn)
    humanFiat(top, cn)
  }
}

case class AddrData(cn: Coin, adr: Address) extends PayData {
  def link = BitcoinURI.convertToBitcoinURI(adr, cn, null, null)
  def sendRequest = SendRequest.to(adr, cn)
  def destination = humanAddr(adr)
}

case class EmptyAddrData(adr: Address) extends PayData {
  def link = BitcoinURI.convertToBitcoinURI(adr, cn, null, null)
  def sendRequest = SendRequest emptyWallet adr
  def destination = humanAddr(adr)
  def cn = app.kit.currentBalance
}

case class P2WSHData(cn: Coin, pay2wsh: Script) extends PayData {
  // For now this will be exlusively used for funding payment channels
  def destination = app getString txs_p2wsh

  def sendRequest = {
    val funding = new Transaction(app.params)
    funding.addOutput(cn, pay2wsh)
    SendRequest forTx funding
  }
}


abstract class TextChangedWatcher extends TextWatcher {
  override def beforeTextChanged(s: CharSequence, x: Int, y: Int, z: Int) = none
  override def afterTextChanged(editableCharSequence: Editable) = none
}

trait BlocksListener extends PeerDataEventListener {
  def getData(peer: Peer, message: GetDataMessage) = null
  def onChainDownloadStarted(peer: Peer, blocksLeft: Int) = none
  def onPreMessageReceived(peer: Peer, message: Message) = message
  val blocksPerDay = 144
}

abstract class TxTracker extends WalletCoinsSentEventListener
  with WalletCoinsReceivedEventListener with TransactionConfidenceEventListener {
  def onCoinsSent(w: Wallet, tx: Transaction, a: Coin, b: Coin) = if (a isGreaterThan b) coinsSent(tx)
  def onCoinsReceived(w: Wallet, tx: Transaction, a: Coin, b: Coin) = if (b isGreaterThan a) coinsReceived(tx)
  def onTransactionConfidenceChanged(wallet: Wallet, tx: Transaction) = if (tx.getConfidence.getDepthInBlocks == minDepth)
    txConfirmed(tx)

  def coinsSent(tx: Transaction): Unit = none
  def coinsReceived(tx: Transaction): Unit = none
  def txConfirmed(tx: Transaction): Unit = none
}