package com.lightning.wallet

import android.view._
import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Tools._
import android.text.format.DateUtils._
import com.journeyapps.barcodescanner._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.Denomination._
import android.support.v4.view.MenuItemCompat._
import com.lightning.wallet.lnutils.JsonHttpUtils._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._

import scala.util.{Success, Try}
import co.infinum.goldfinger.{Error => GFError}
import android.content.{DialogInterface, Intent}
import co.infinum.goldfinger.{Goldfinger, Warning}
import android.support.v7.widget.{SearchView, Toolbar}
import android.provider.Settings.{System => FontSystem}
import com.lightning.wallet.ln.wire.{NodeAnnouncement, WalletZygote}
import fr.castorflex.android.smoothprogressbar.{SmoothProgressBar, SmoothProgressDrawable}
import com.ogaclejapan.smarttablayout.utils.v4.{FragmentPagerItemAdapter, FragmentPagerItems}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.walletZygoteCodec
import android.support.v4.view.ViewPager.OnPageChangeListener
import com.lightning.wallet.ln.Channel.estimateTotalCanSend
import android.content.DialogInterface.OnDismissListener
import com.lightning.wallet.lnutils.olympus.OlympusWrap
import android.content.Context.LAYOUT_INFLATER_SERVICE
import com.ogaclejapan.smarttablayout.SmartTabLayout
import org.ndeftools.util.activity.NfcReaderActivity
import android.widget.AbsListView.OnScrollListener
import com.lightning.wallet.lnutils.RatesSaver
import android.view.ViewGroup.LayoutParams
import android.support.v4.view.ViewPager
import org.bitcoinj.store.SPVBlockStore
import android.support.v4.app.Fragment
import com.lightning.wallet.helper.AES
import android.text.format.DateFormat
import fr.acinq.bitcoin.MilliSatoshi
import org.bitcoinj.uri.BitcoinURI
import com.google.common.io.Files
import java.text.SimpleDateFormat
import org.bitcoinj.core.Address
import scala.collection.mutable
import android.text.InputType
import org.ndeftools.Message
import android.os.Bundle
import android.net.Uri
import java.util.Date
import java.io.File


trait SearchBar { me =>
  var react: String => Unit = _
  var searchView: SearchView = _

  val queryListener = new SearchView.OnQueryTextListener {
    def onQueryTextChange(ask: String) = runAnd(true)(me react ask)
    def onQueryTextSubmit(ask: String) = true
  }

  def setupSearch(menu: Menu) = {
    val item = menu findItem R.id.action_search
    searchView = getActionView(item).asInstanceOf[SearchView]
    searchView setOnQueryTextListener queryListener
  }
}

trait HumanTimeDisplay {
  val host: TimerActivity
  val time: Date => String = date => new SimpleDateFormat(timeString) format date
  lazy val bigFont = FontSystem.getFloat(host.getContentResolver, FontSystem.FONT_SCALE, 1) > 1

  // Should be accessed after activity is initialized
  lazy val timeString = DateFormat is24HourFormat host match {
    case false if host.scrWidth < 2.2 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if host.scrWidth < 2.2 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if host.scrWidth < 2.5 & bigFont => "MM/dd/yy' <small>'h:mma'</small>'"
    case false if host.scrWidth < 2.5 => "MM/dd/yy' <small>'h:mma'</small>'"

    case false if bigFont => "MMM dd, yyyy' <small>'h:mma'</small>'"
    case false => "MMMM dd, yyyy' <small>'h:mma'</small>'"

    case true if host.scrWidth < 2.2 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if host.scrWidth < 2.2 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if host.scrWidth < 2.4 & bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true if host.scrWidth < 2.5 => "d MMM yyyy' <small>'HH:mm'</small>'"

    case true if bigFont => "d MMM yyyy' <small>'HH:mm'</small>'"
    case true => "d MMMM yyyy' <small>'HH:mm'</small>'"
  }

  def when(now: Long, date: Date) = date.getTime match { case ago =>
    if (now - ago < 129600000) getRelativeTimeSpanString(ago, now, 0).toString
    else time(date)
  }
}

trait ToolbarFragment extends HumanTimeDisplay { me =>
  // Manages multiple info tickers in toolbar subtitle
  var infos = List.empty[Informer]
  val toolbar: Toolbar

  // Informer CRUD
  def delete(tag: Int) = host UITask {
    infos = infos.filterNot(_.tag == tag)
    toolbar setSubtitle infos.head.value
  }

  def add(text: String, tag: Int) = host UITask {
    infos = new Informer(text, tag) :: infos
    toolbar setSubtitle infos.head.value
  }

  def update(text: String, tag: Int) = host UITask {
    for (vs <- infos if vs.tag == tag) vs.value = text
    toolbar setSubtitle infos.head.value
  }

  def setTitle(titleText: String) = {
    // A workaround to prevent ellipsis
    toolbar setTitle titleText
    toolbar setTitle titleText
  }
}

trait ListToggler extends HumanTimeDisplay {
  lazy val allTxsWrapper = host.getLayoutInflater.inflate(R.layout.frag_toggler, null)
  lazy val toggler = allTxsWrapper.findViewById(R.id.toggler).asInstanceOf[ImageButton]
  val minLinesNum = 4

  abstract class CutAdapter[T](val max: Int, viewLine: Int) extends BaseAdapter {
    // Automatically switches list view from short to long version and back again
    def switch = cut = if (cut == minLinesNum) max else minLinesNum
    def getItemId(position: Int) = position
    def getCount = visibleItems.size

    var cut = minLinesNum
    var visibleItems = Vector.empty[T]
    var availableItems = Vector.empty[T]

    val set: Vector[T] => Unit = items1 => {
      val visibility = if (items1.size > minLinesNum) View.VISIBLE else View.GONE
      val resource = if (cut == minLinesNum) R.drawable.ic_expand_more_black_24dp
        else R.drawable.ic_expand_less_black_24dp

      allTxsWrapper setVisibility visibility
      toggler setImageResource resource
      visibleItems = items1 take cut
      availableItems = items1
    }

    def getView(position: Int, savedView: View, parent: ViewGroup) = {
      val view = if (null == savedView) host.getLayoutInflater.inflate(viewLine, null) else savedView
      val hold = if (null == view.getTag) getHolder(view) else view.getTag.asInstanceOf[TxViewHolder]
      hold fillView visibleItems(position)
      view
    }

    def getHolder(view: View): TxViewHolder
    abstract class TxViewHolder(view: View) {
      val transactCircle = view.findViewById(R.id.transactCircle).asInstanceOf[ImageView]
      val transactWhen = view.findViewById(R.id.transactWhen).asInstanceOf[TextView]
      val transactSum = view.findViewById(R.id.transactSum).asInstanceOf[TextView]
      def fillView(data: T): Unit
      view setTag this
    }
  }
}

object WalletActivity {
  val REDIRECT = "goLnOps"
  val frags = mutable.Set.empty[Fragment]
  def lnOpt = frags collectFirst { case ln: FragLN => ln.worker }
  def btcOpt = frags collectFirst { case btc: FragBTC => btc.worker }
}

class WalletActivity extends NfcReaderActivity with TimerActivity { me =>
  lazy val walletPager = findViewById(R.id.walletPager).asInstanceOf[ViewPager]
  lazy val walletPagerTab = findViewById(R.id.walletPagerTab).asInstanceOf[SmartTabLayout]
  lazy val layoutInflater = app.getSystemService(LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
  lazy val viewParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
  lazy val container = findViewById(R.id.container).asInstanceOf[FrameLayout]
  private[this] var listState = OnScrollListener.SCROLL_STATE_IDLE
  private[this] var pagerState = ViewPager.SCROLL_STATE_IDLE
  import WalletActivity.{lnOpt, btcOpt}

  val listListener = new OnScrollListener {
    def onScroll(view: AbsListView, first: Int, vis: Int, total: Int) = none
    def onScrollStateChanged(view: AbsListView, state: Int) = listState = state
  }

  val pagerListener = new OnPageChangeListener {
    def onPageSelected(pos: Int) = app.prefs.edit.putInt(AbstractKit.LANDING, pos).commit
    def onPageScrolled(pos: Int, positionOffset: Float, offsetPixels: Int) = none
    def onPageScrollStateChanged (state: Int) = pagerState = state
  }

  lazy val adapter = {
    val fragScan = classOf[FragScan]
    val fragBTC = classOf[FragBTC]
    val fragLN = classOf[FragLN]

    val items = FragmentPagerItems `with` me
    val ready = items.add("bitcoin", fragBTC).add("lightning", fragLN).add("scanner", fragScan)
    new FragmentPagerItemAdapter(getSupportFragmentManager, ready.create)
  }

  def placeLoader = {
    val loader = layoutInflater.inflate(R.layout.frag_progress_bar, null).asInstanceOf[SmoothProgressBar]
    val drawable: SmoothProgressDrawable = loader.getIndeterminateDrawable.asInstanceOf[SmoothProgressDrawable]
    timer.schedule(drawable.setColors(getResources getIntArray R.array.bar_colors), 100)
    container.addView(loader, viewParams)
    loader
  }

  def removeLoader(loader: SmoothProgressBar) = {
    timer.schedule(container removeView loader, 3000)
    timer.schedule(loader.progressiveStop, 200)
  }

  override def onResume = wrap(super.onResume) {
    val pos = app.prefs.getInt(AbstractKit.LANDING, 0)
    walletPager.setCurrentItem(math.min(pos, 1), false)
  }

  override def onDestroy = wrap(super.onDestroy)(stopDetecting)
  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionSettings) makeSettingsForm
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    // Called after fragLN sets toolbar as actionbar
    getMenuInflater.inflate(R.menu.ln, menu)
    for (ln <- lnOpt) ln setupSearch menu
    true
  }

  override def onBackPressed =
    if (walletPager.getCurrentItem < 2) super.onBackPressed
    else walletPager.setCurrentItem(walletPager.getCurrentItem - 1)

  def updateListsTime = {
    val notPaging = ViewPager.SCROLL_STATE_IDLE == pagerState
    val notScrolling = OnScrollListener.SCROLL_STATE_IDLE == listState
    if (notPaging && notScrolling) for (ln <- lnOpt) ln.adapter.notifyDataSetChanged
    if (notPaging && notScrolling) for (btc <- btcOpt) btc.adapter.notifyDataSetChanged
  }

  def INIT(state: Bundle) = if (app.isAlive) {
    me setContentView R.layout.activity_wallet

    walletPager setAdapter adapter
    walletPager setOffscreenPageLimit 2
    walletPagerTab setViewPager walletPager
    walletPagerTab setOnPageChangeListener pagerListener

    // NFC will happen while app is open so
    // there is no need to run it from fragment
    wrap(me setDetecting true)(me initNfc state)
    timer.schedule(updateListsTime, 10000, 10000)
  } else me exitTo classOf[MainActivity]

  // NFC

  def readEmptyNdefMessage = app toast nfc_error
  def readNonNdefMessage = app toast nfc_error
  def onNfcStateChange(ok: Boolean) = none
  def onNfcFeatureNotFound = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNdefMessage(msg: Message) = try {
    val data: String = readFirstTextNdefMessage(msg)
    app.TransData recordValue data
    checkTransData

  } catch { case _: Throwable =>
    // Could not process a message
    app toast nfc_error
  }

  // EXTERNAL DATA CHECK

  private def doTheCheck(ok: Any) = {
    // Find out what to do depending on
    // what kind of TransData we have

    app.TransData.value match {
      case _: NodeAnnouncement => walletPager.setCurrentItem(1, false)
      case _: PaymentRequest => walletPager.setCurrentItem(1, false)
      case _: BitcoinURI => walletPager.setCurrentItem(0, false)
      case _: Address => walletPager.setCurrentItem(0, false)
      case _ =>
    }

    app.TransData.value match {
      case _: NodeAnnouncement => me goTo classOf[LNStartFundActivity]
      case lnPaymentRequest: PaymentRequest => for (ln <- lnOpt) ln.sendPayment(lnPaymentRequest)
      case bu: BitcoinURI => for (btc <- btcOpt) btc.sendBtcPopup.set(Try(bu.getAmount), bu.getAddress)
      case addr: Address => for (btc <- btcOpt) btc.sendBtcPopup.setAddr(addr)
      case WalletActivity.REDIRECT => goChanDetails(null)
      case _ =>
    }

    app.TransData.value match {
      case _: NodeAnnouncement => // nope
      case _ => app.TransData.value = null
    }
  }

  def checkTransData = app.getBufferTry match {
    case Success(pr) if app.TransData.lnLink.findFirstIn(pr).isDefined =>
      // Only LN payment requests since autopasting other stuff is dangerous
      <(app.TransData recordValue pr, doTheCheck)(doTheCheck)
      app.setBuffer(new String, andNotify = false)

    case _ =>
      // Not interesting
      doTheCheck(null)
  }

  // BUTTONS REACTIONS

  def goSendBTC(top: View) =
    for (btc <- btcOpt) btc.sendBtcPopup

  def goReceiveBTC(top: View) = {
    app.TransData.value = app.kit.currentAddress
    me goTo classOf[RequestActivity]
  }

  def goReceiveLN(top: View) = for (ln <- lnOpt) ln.makePaymentRequest

  def goChanDetails(top: View) = {
    val nothingToShow = app.ChannelManager.all.isEmpty
    if (nothingToShow) app toast ln_status_none
    else me goTo classOf[LNOpsActivity]
  }

  def goAddChannel(top: View) = {
    val tokens = MilliSatoshi(500000L)
    val minAmt = RatesSaver.rates.feeLive multiply 2
    val humanIn = humanFiat(coloredIn(tokens), tokens, " ")
    val warningMessage = getString(tokens_warn).format(humanIn)

    val warn = baseTextBuilder(warningMessage.html)
      .setCustomTitle(me getString action_ln_open)

    if (!broadcaster.isSynchronized) app toast dialog_chain_behind
    else if (app.kit.conf1Balance isLessThan minAmt) showForm(negBuilder(dialog_ok, notEnoughFunds.html, null).create)
    else if (app.ChannelManager.all.isEmpty) mkForm(me goTo classOf[LNStartActivity], none, warn, dialog_ok, dialog_cancel)
    else me goTo classOf[LNStartActivity]

    lazy val notEnoughFunds = {
      val txt = getString(err_ln_not_enough_funds)
      val zeroConf = app.kit.conf0Balance minus app.kit.conf1Balance
      val canSend = sumIn format denom.withSign(app.kit.conf1Balance)
      val minRequired = sumIn format denom.withSign(minAmt)
      val pending = sumIn format denom.withSign(zeroConf)
      txt.format(canSend, minRequired, pending)
    }
  }

  def showDenomChooser = {
    val btcFunds = coin2MSat(app.kit.conf0Balance)
    val lnFunds = MilliSatoshi(app.ChannelManager.notClosingOrRefunding.map(estimateTotalCanSend).sum)
    val btcPrettyFunds = humanFiat(sumIn format denom.withSign(btcFunds), btcFunds, " ")
    val lnPrettyFunds = humanFiat(sumIn format denom.withSign(lnFunds), lnFunds, " ")
    val title = getString(fiat_set_denom).format(btcPrettyFunds, lnPrettyFunds)
    val denominations = getResources.getStringArray(R.array.denoms).map(_.html)
    val form = getLayoutInflater.inflate(R.layout.frag_input_choose_fee, null)
    val lst = form.findViewById(R.id.choiceList).asInstanceOf[ListView]

    lst setOnItemClickListener onTap { pos =>
      // Persist user setting first and update view

      denom = denoms(pos)
      app.prefs.edit.putInt(AbstractKit.DENOM_TYPE, pos).commit
      for (btc <- btcOpt) wrap(btc.adapter.notifyDataSetChanged)(btc.updTitle)
      for (ln <- lnOpt) wrap(ln.adapter.notifyDataSetChanged)(ln.updTitleSubtitleAndButtons)
    }

    lst setAdapter new ArrayAdapter(me, singleChoice, denominations)
    lst.setItemChecked(app.prefs.getInt(AbstractKit.DENOM_TYPE, 0), true)
    showForm(negBuilder(dialog_ok, title.html, form).create)
  }

  // SETTINGS FORM

  def makeSettingsForm = {
    val feePerKb = denom withSign RatesSaver.rates.feeLive
    val title = getString(read_settings).format(feePerKb).html
    val form = getLayoutInflater.inflate(R.layout.frag_settings, null)
    val menu = showForm(negBuilder(dialog_ok, title, form).create)

    val recoverChannelFunds = form.findViewById(R.id.recoverChannelFunds).asInstanceOf[Button]
    val useFingerprint = form.findViewById(R.id.useFingerprint).asInstanceOf[Button]
    val manageOlympus = form.findViewById(R.id.manageOlympus).asInstanceOf[Button]
    val createZygote = form.findViewById(R.id.createZygote).asInstanceOf[Button]
    val rescanWallet = form.findViewById(R.id.rescanWallet).asInstanceOf[Button]
    val viewMnemonic = form.findViewById(R.id.viewMnemonic).asInstanceOf[Button]
    val changePass = form.findViewById(R.id.changePass).asInstanceOf[Button]
    val gf = new Goldfinger.Builder(me).build

    manageOlympus setOnClickListener onButtonTap {
      // Just show a list of available Olympus servers
      def proceed = me goTo classOf[OlympusActivity]
      rm(menu)(proceed)
    }

    if (app.kit.wallet.isEncrypted && gf.hasEnrolledFingerprint) {
      if (FingerPassCode.exists) setButtonDisable else setButtonEnable
      useFingerprint setVisibility View.VISIBLE

      def setButtonEnable = {
        def proceed = rm(menu) {
          passWrap(me getString fp_enable) apply checkPass { pass =>
            // The password is guaranteed to be correct here so proceed
            val content = getLayoutInflater.inflate(R.layout.frag_touch, null)
            val alert = showForm(negBuilder(dialog_cancel, getString(fp_enable).html, content).create)

            val callback = new Goldfinger.Callback {
              def onWarning(warn: Warning) = FingerPassCode informUser warn
              def onError(err: GFError) = wrap(FingerPassCode informUser err)(alert.dismiss)
              def onSuccess(cipher: String) = runAnd(alert.dismiss)(FingerPassCode record cipher)
            }

            alert setOnDismissListener new OnDismissListener {
              def onDismiss(dialog: DialogInterface) = gf.cancel
              gf.encrypt(fileName, pass, callback)
            }
          }
        }

        // Offer user to enter a passcode and then fingerprint
        useFingerprint setOnClickListener onButtonTap(proceed)
        useFingerprint setText fp_enable
      }

      def setButtonDisable = {
        def proceed = rm(menu) {
          passWrap(me getString fp_disable, fp = false) apply checkPass { _ =>
            // Ask user for a passcode before disabling fingerprint unlocking
            // and don't allow fingerprint unlocking this is special case
            runAnd(app toast fp_err_disabled)(FingerPassCode.erase)
          }
        }

        // Disable fingerprint auth if user rememebers passcode
        useFingerprint setOnClickListener onButtonTap(proceed)
        useFingerprint setText fp_disable
      }
    }

    recoverChannelFunds setOnClickListener onButtonTap {
      // When wallet data is lost users may recover channel funds
      // by fetching encrypted static channel params from server

      rm(menu) {
        if (!broadcaster.isSynchronized) app toast dialog_chain_behind else {
          // We only allow recovering once BTC is synchronized to avoid issues
          val bld = baseTextBuilder(me getString channel_recovery_info)
          mkForm(recover, none, bld, dialog_next, dialog_cancel)
        }

        def recover: Unit = {
          OlympusWrap.getBackup(cloudId).foreach(backups => {
            // Decrypt channel recovery data upon successful call and put
            // them into an active channel list, then connect to peers

            for {
              encrypted <- backups
              jsonDecoded = AES.decode(encrypted, cloudSecret)
              // This may be some garbage so omit this one if it fails
              refundingData <- Try apply to[RefundingData](jsonDecoded)
              // Now throw it away if it is already present in list of local channels
              if !app.ChannelManager.all.exists(_(_.channelId) contains refundingData.commitments.channelId)
              chan = app.ChannelManager.createChannel(app.ChannelManager.operationalListeners, refundingData)
              // Start watching this channel's funding tx output right away
              _ = app.kit watchFunding refundingData.commitments
            } app.ChannelManager.all +:= chan
            app.ChannelManager.initConnect
          }, none)

          // Let user know it's happening
          app toast dialog_recovering
        }
      }
    }

    createZygote setOnClickListener onButtonTap {
      def openForm = mkForm(ok = <(createZygote, onFail) { zygote =>
        val zygoteFileShare = new Intent setAction Intent.ACTION_SEND setType "text/plain"
        me startActivity zygoteFileShare.putExtra(Intent.EXTRA_STREAM, Uri fromFile zygote)
      }, none, baseTextBuilder(getString(zygote_details).html), dialog_next, dialog_cancel)

      def createZygote = {
        val zygote = FileOps shell s"zygote ${new Date}.txt"
        val dbFile = new File(app.getDatabasePath(dbFileName).getPath)
        val sourceFilesSeq = Seq(dbFile, app.walletFile, app.chainFile)
        val Seq(dbBytes, walletBytes, chainBytes) = sourceFilesSeq map Files.toByteArray
        val encoded = walletZygoteCodec encode WalletZygote(1, dbBytes, walletBytes, chainBytes)
        Files.write(encoded.require.toByteArray, zygote)
        zygote
      }

      rm(menu)(openForm)
    }

    rescanWallet setOnClickListener onButtonTap {
      // May be needed in case of blockchain glitches
      // warn user as this is a time consuming operation

      rm(menu) {
        val bld = baseTextBuilder(me getString sets_rescan_ok)
        mkForm(go, none, bld, dialog_ok, dialog_cancel)
      }

      def go = try {
        app.chainFile.delete
        app.kit.wallet.reset
        app.kit.store = new SPVBlockStore(app.params, app.chainFile)
        app.kit useCheckPoints app.kit.wallet.getEarliestKeyCreationTime
        app.kit.wallet saveToFile app.walletFile
      } catch none finally System exit 0
    }

    viewMnemonic setOnClickListener onButtonTap {
      // Can be accessed here and from page button
      rm(menu)(me viewMnemonic null)
    }

    changePass setOnClickListener onButtonTap {
      def decryptAndMaybeEncrypt(oldPass: CharSequence)(newPass: CharSequence) = {
        // First decrypt a wallet using an old passcode, then encrypt with a new one
        runAnd(app.kit.wallet decrypt oldPass)(FingerPassCode.erase)
        maybeEncrypt(newPass)
      }

      def maybeEncrypt(newPass: CharSequence) = {
        // Leave the wallet decrypted if there is no new passcode
        if (newPass.length > 0) app.encryptWallet(app.kit.wallet, newPass)
      }

      val showNewPassForm = (changePass: CharSequence => Unit) => {
        val view \ field \ _ = generatePromptView(InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD, secret_info, null)
        mkForm(changePasscode, none, baseBuilder(me getString sets_secret_change, view), dialog_ok, dialog_cancel)
        def inform = showForm(negTextBuilder(dialog_ok, getString(sets_secret_ok) format field.getText).create)

        def changePasscode = runAnd(app toast secret_changing) {
          // Maybe decrypt a wallet and set a new passcode, inform user afterwards
          <(changePass(field.getText), onFail)(_ => if (field.getText.length > 0) inform)
        }
      }

      rm(menu) {
        if (!app.kit.wallet.isEncrypted) showNewPassForm apply maybeEncrypt
        else passWrap(me getString sets_secret_change) apply checkPass { pass =>
          // Remember a correct passcode and use it later for wallet decryption
          showNewPassForm apply decryptAndMaybeEncrypt(pass)
        }
      }
    }
  }
}

class FragScan extends Fragment with BarcodeCallback { me =>
  type Points = java.util.List[com.google.zxing.ResultPoint]
  lazy val host = getActivity.asInstanceOf[WalletActivity]
  var lastAttempt = System.currentTimeMillis
  var barcodeReader: BarcodeView = _

  override def onCreateView(inflator: LayoutInflater, vg: ViewGroup, bn: Bundle) =
    inflator.inflate(R.layout.frag_view_pager_scan, vg, false)

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    barcodeReader = view.findViewById(R.id.reader).asInstanceOf[BarcodeView]
    barcodeReader decodeContinuous me
  }

  override def setUserVisibleHint(isVisibleToUser: Boolean) = {
    if (barcodeReader != null) if (isVisibleToUser) barcodeReader.resume else barcodeReader.pause
    if (!isVisibleToUser) getFragmentManager.beginTransaction.detach(me).attach(me).commit
    super.setUserVisibleHint(isVisibleToUser)
  }

  // Only try to decode result after 2 seconds
  override def possibleResultPoints(points: Points) = none
  override def barcodeResult(res: BarcodeResult) = Option(res.getText) foreach {
    rawText => if (System.currentTimeMillis - lastAttempt > 3000) tryParseQR(rawText)
  }

  def tryParseQR(text: String) = try {
    // May throw which is expected and fine
    lastAttempt = System.currentTimeMillis
    app.TransData recordValue text
    host.checkTransData

  } catch app.TransData.onFail { code =>
    // Inform user about error details
    app toast host.getString(code)
  }
}