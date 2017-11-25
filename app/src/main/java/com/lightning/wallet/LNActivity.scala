package com.lightning.wallet

import android.view._
import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.helper._
import com.lightning.wallet.lnutils._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lnutils.ImplicitConversions._
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi, Satoshi}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import com.lightning.wallet.ln.Tools.{none, random}
import com.lightning.wallet.ln.Tools.{runAnd, wrap}
import scala.util.{Failure, Success, Try}

import fr.castorflex.android.smoothprogressbar.SmoothProgressDrawable
import fr.castorflex.android.smoothprogressbar.SmoothProgressBar
import android.support.v7.widget.SearchView.OnQueryTextListener
import android.content.Context.LAYOUT_INFLATER_SERVICE
import android.content.DialogInterface.BUTTON_POSITIVE
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.clans.fab.FloatingActionMenu
import com.lightning.wallet.ln.wire.CommitSig
import android.support.v4.view.MenuItemCompat
import android.view.ViewGroup.LayoutParams
import com.lightning.wallet.Utils.app
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import scala.language.postfixOps
import android.app.AlertDialog
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date


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

class LNActivity extends DataReader with ToolbarActivity with ListUpdater with SearchBar { me =>
  lazy val layoutInflater = app.getSystemService(LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
  lazy val viewParams = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
  lazy val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  lazy val container = findViewById(R.id.container).asInstanceOf[RelativeLayout]
  lazy val fab = findViewById(R.id.fab).asInstanceOf[FloatingActionMenu]
  lazy val paymentsViewProvider = new PaymentsViewProvider
  val imgMap = Array(await, await, conf1, dead)

  lazy val adapter = new CutAdapter[PaymentInfo] {
    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(info: PaymentInfo) = {
        val timestamp = new Date(info.stamp)
        val markedPaymentSum = info.incoming match {
          case 1 => sumIn.format(denom formatted info.amount)
          case _ => sumOut.format(denom formatted info.amount * -1)
        }

        transactWhen setText when(System.currentTimeMillis, timestamp).html
        transactSum setText s"$markedPaymentSum\u00A0${info.text}".html
        transactCircle setImageResource imgMap(info.actualStatus)
      }
    }
  }

  val chanListener = new ChannelListener {
    // Updates local UI according to changes in channel
    // Should be removed when activity is stopped

    override def onError = {
      case _ \ ReserveException(PlainAddHtlc(out), missingSat, reserveSat) =>
        // Current commit tx fee and channel reserve forbid sending of this payment
        // Inform user with all the details laid out as cleanly as possible

        val message = me getString err_ln_fee_overflow
        val reserve = coloredIn apply Satoshi(reserveSat)
        val missing = coloredOut apply Satoshi(missingSat)
        val sending = coloredOut apply MilliSatoshi(out.amountWithFee)
        onFail(error = message.format(reserve, sending, missing).html)

      case _ \ AddException(_: PlainAddHtlc, code) =>
        // Let user know why payment could not be added
        onFail(me getString code)
    }

    override def onBecome = {
      case (chan, _, _, _) if !chan.isOperational => evacuate
      case (_, _: NormalData, _, SYNC) => update(me getString ln_notify_connecting, Informer.LNSTATE).flash.run
      case (_, _: NormalData, _, NORMAL) => update(me getString ln_notify_operational, Informer.LNSTATE).flash.run
    }

    override def onProcess = {
      case (chan, norm: NormalData, _: CommitSig)
        // GUARD: Update UI once we have some fulfilled HTLCs
        if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
        notifySubTitle(me getString ln_done, Informer.LNSUCCESS)
        me updTitle chan
    }
  }

  private[this] var sendPayment: PaymentRequest => Unit = none
  private[this] var closePaymentChannel = anyToRunnable(none)
  private[this] var makePaymentRequest = anyToRunnable(none)
  private[this] var whenStop = anyToRunnable(super.onStop)
  override def onStop = whenStop.run

  def evacuate = me exitTo classOf[LNOpsActivity]
  def react(qs: String) = paymentsViewProvider reload qs
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will updated separately so just update subtitle
    timer.schedule(delete(infoType), 8000)
    add(subtitle, infoType).flash.run
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =

    if (app.isAlive) {
      super.onCreate(savedState)

      // Set action bar, main view content, wire up list events, update title later
      wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln)
      add(me getString ln_notify_connecting, Informer.LNSTATE)
      me startListUpdates adapter
      me setDetecting true

      list setAdapter adapter
      list setFooterDividersEnabled false
      paymentsViewProvider reload new String
      app.kit.wallet addCoinsSentEventListener txTracker
      app.kit.wallet addCoinsReceivedEventListener txTracker
      app.kit.wallet addTransactionConfidenceEventListener txTracker
      app.kit.peerGroup addBlocksDownloadedEventListener catchListener
    } else me exitTo classOf[MainActivity]

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

  override def onResume: Unit = wrap(super.onResume) {
    app.prefs.edit.putString(AbstractKit.LANDING, AbstractKit.LIGHTNING).commit
    app.ChannelManager.all.find(_.isOperational) map manageActive getOrElse evacuate
  }

  // APP MENU

  override def onOptionsItemSelected(menu: MenuItem) = runAnd(true) {
    if (menu.getItemId == R.id.actionCloseChannel) closePaymentChannel.run
    else if (menu.getItemId == R.id.actionSettings) mkSetsForm
  }

  def updTitle(chan: Channel) = animateTitle {
    val canSend = chan(_.localCommit.spec.toLocalMsat)
    denom withSign MilliSatoshi(canSend getOrElse 0L)
  }

  def manageActive(chan: Channel) = {
    val pay = (manualRequestRoutingData: RoutingData) => {
      def onPaymentError(er: Throwable) = er.getMessage match {
        case FROMBLACKLISTED => onFail(me getString err_ln_black)
        case techDetails => onFail(techDetails)
      }

      val progressBarManager = new ProgressBarManager
      notifySubTitle(me getString ln_send, Informer.LNPAYMENT)
      app.ChannelManager.outPaymentObs(rd = manualRequestRoutingData)
        .doOnTerminate(progressBarManager.delayedRemove).foreach(onNext = {
          // Must account for a special kind of error when no routes are found
          case Some(outgoingPayment) => chan process PlainAddHtlc(outgoingPayment)
          case None => onFail(me getString err_ln_no_route)
        }, onPaymentError)
    }

    list setOnItemClickListener onTap { pos =>
      val info @ PaymentInfo(hash, incoming, preimg, amount, _, _, _) = adapter getItem pos
      val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_ln_payment_details, null)
      val paymentRetryAgain = detailsWrapper.findViewById(R.id.paymentRetryAgain).asInstanceOf[Button]
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]

      bag getRoutingData hash foreach { rd =>
        val description = me getDescription rd.pr
        val humanHash = hash.toString.grouped(32).map(_ grouped 8 mkString "\u00A0")
        val humanStatus = s"<strong>${paymentStatesMap apply info.actualStatus}</strong>"
        paymentHash setOnClickListener onButtonTap(app setBuffer hash.toString)
        paymentHash setText humanHash.mkString("\n")

        if (info.actualStatus == SUCCESS) {
          paymentProof setVisibility View.VISIBLE
          paymentProof setOnClickListener onButtonTap {
            // Users may copy request and preimage to prove it
            val serialzied: String = PaymentRequest.write(pr = rd.pr)
            app setBuffer getString(ln_proof).format(serialzied, preimg.toString)
          }
        }

        if (incoming == 1) {
          val humanReceived = humanFiat(coloredIn(amount), amount)
          val title = getString(ln_incoming_title).format(humanStatus)
          paymentDetails setText s"$description<br><br>$humanReceived".html
          mkForm(me negBld dialog_ok, title.html, detailsWrapper)
        } else {
          val humanSent = humanFiat(coloredOut(amount), amount)
          val fee = MilliSatoshi(rd.amountWithFee - rd.pr.finalSum.amount)
          val title = getString(ln_outgoing_title).format(coloredOut(fee), humanStatus)
          val alert: AlertDialog = mkForm(me negBld dialog_ok, title.html, detailsWrapper)
          paymentDetails setText s"$description<br><br>$humanSent".html

          def retry = rm(alert) { pay apply rd }
          if (info.actualStatus != SUCCESS && rd.pr.isFresh) {
            // Users may issue new request to get an updated set of routes
            // routing data will likely contain bad nodes and channels here
            paymentRetryAgain setOnClickListener onButtonTap(retry)
            paymentRetryAgain setVisibility View.VISIBLE
          }
        }
      }
    }

    toolbar setOnClickListener onButtonTap {
      wrap(adapter.notifyDataSetChanged)(changeDenom)
      me updTitle chan
    }

    sendPayment = pr => {
      val title = getString(ln_send_title).format(me getDescription pr)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val alert = mkForm(negPosBld(dialog_cancel, dialog_pay), title.html, content)
      val rateManager = new RateManager(new String, content)

      def sendAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxHtlcValue < ms => app toast dialog_sum_big
        case Success(ms) if pr.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
        case Success(ms) if htlcMinimumMsat > ms.amount => app toast dialog_sum_small
        case Success(ms) if pr.amount.exists(_ > ms) => app toast dialog_sum_small
        case _ => rm(alert) { pay compose emptyRD apply pr }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- pr.amount) rateManager setSum Try(sum)
    }

    closePaymentChannel = anyToRunnable {
      val nodeView = me getString ln_ops_start_node_view
      val humanId = chan.data.announce.nodeId.toString grouped 3 mkString "\u0020"
      val humanAddress = chan.data.announce.addresses.headOption.map(_.getHostString).orNull
      val nodeDetails = nodeView.format(chan.data.announce.alias, humanAddress, humanId)

      // Close all of the channels just in case we have more than one active left
      checkPass(s"$nodeDetails<br><br>${me getString ln_close}".html) { pass =>
        for (chan <- app.ChannelManager.notClosing) chan process CMDShutdown
      }
    }

    makePaymentRequest = anyToRunnable {
      // Somewhat counterintuitive: localParams.channelReserveSat is THEIR unspendable reseve
      // peer's balance can't go below their unspendable reserve so it should be taken into account here
      val canReceive = chan(c => c.localCommit.spec.toRemoteMsat - c.localParams.channelReserveSat * 1000L)
      val finalCanReceive = math.min(canReceive.filter(_ > 0L) getOrElse 0L, maxHtlcValue.amount)
      val maxMsat = MilliSatoshi(finalCanReceive)

      val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
      val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
      val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), me getString ln_receive, content)
      val hint = getString(amount_hint_maxamount).format(denom withSign maxMsat)
      val rateManager = new RateManager(hint, content)

      def proceed(amount: Option[MilliSatoshi], preimg: BinaryData) = {
        val paymentRequest = PaymentRequest(chainHash, amount, Crypto sha256 preimg, nodePrivateKey,
          inputDescription.getText.toString.trim, fallbackAddress = None, 3600 * 6, extra = Vector.empty)

        bag upsertRoutingData emptyRD(paymentRequest)
        // Amount of 0 is a special case which means they can pay whatever they want
        // unfulfilled incoming HTLCs are marked HIDDEN and not displayed to user by default
        bag upsertPaymentInfo PaymentInfo(hash = paymentRequest.paymentHash, incoming = 1, preimg,
          paymentRequest.amount getOrElse MilliSatoshi(0), HIDDEN, System.currentTimeMillis,
          paymentRequest.description.right getOrElse new String)

        app.TransData.value = paymentRequest
        me goTo classOf[RequestActivity]
      }

      def receiveAttempt = rateManager.result match {
        case Success(ms) if maxMsat < ms => app toast dialog_sum_big
        case Success(ms) if htlcMinimumMsat > ms.amount => app toast dialog_sum_small

        case result => rm(alert) {
          // Payment request may contain no amount
          notifySubTitle(me getString ln_pr_make, Informer.LNPAYMENT)
          <(proceed(result.toOption, random getBytes 32), onFail)(none)
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(receiveAttempt)
    }

    whenStop = anyToRunnable {
      chan.listeners -= chanListener
      super.onStop
    }

    checkTransData
    // Call checkTransData first
    chan.listeners += chanListener
    chanListener reloadOnBecome chan
    me updTitle chan
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => me goTo classOf[BtcActivity]
    case adr: Address => me goTo classOf[BtcActivity]

    case pr: PaymentRequest =>
      app.TransData.value = null
      if (pr.isFresh) sendPayment(pr)
      else onFail(me getString err_ln_old)

    case otherwise =>
      app.TransData.value = null
      Tools log s"Unusable $otherwise"
  }

  // Reactions to menu
  def goBitcoin(top: View) = {
    val activity = classOf[BtcActivity]
    delayUI(me goTo activity)
    fab close true
  }

  def goQR(top: View) = {
    val activity = classOf[ScanActivity]
    delayUI(me goTo activity)
    fab close true
  }

  def goReceive(top: View) = {
    delayUI(makePaymentRequest.run)
    fab close true
  }

  def toggle(v: View) = {
    // Expand or collapse all txs
    // adapter contains all history

    adapter.switch
    adapter set adapter.availableItems
    adapter.notifyDataSetChanged
  }

  // Payment history and search results loader
  class PaymentsViewProvider extends ReactCallback(me) { self =>
    def onCreateLoader(id: Int, bundle: Bundle) = if (lastQuery.isEmpty) recent else search
    def search = new ExtendedPaymentInfoLoader { def getCursor = bag byQuery lastQuery }
    def recent = new ExtendedPaymentInfoLoader { def getCursor = bag byRecent }
    val observeTablePath = db sqlPath PaymentInfoTable.table
    var lastQuery = new String

    def reload(txt: String) = runAnd(lastQuery = txt) {
      // Remember last search term to handle possible reload
      getSupportLoaderManager.restartLoader(1, null, self).forceLoad
    }

    abstract class ExtendedPaymentInfoLoader extends ReactLoader[PaymentInfo](me) {
      val consume: InfoVec => Unit = payments => me runOnUiThread updatePaymentList(payments)
      def updatePaymentList(pays: InfoVec) = wrap(adapter.notifyDataSetChanged)(adapter set pays)
      def createItem(shifted: RichCursor) = bag toPaymentInfo shifted
      type InfoVec = Vector[PaymentInfo]
    }
  }

  class ProgressBarManager {
    def delayedRemove = try timer.schedule(anyToRunnable(progressBar.progressiveStop), 250) catch none
    val progressBar = layoutInflater.inflate(R.layout.frag_progress_bar, null).asInstanceOf[SmoothProgressBar]
    val drawable = progressBar.getIndeterminateDrawable.asInstanceOf[SmoothProgressDrawable]

    container.addView(progressBar, viewParams)
    drawable setCallbacks new SmoothProgressDrawable.Callbacks {
      // In some cases activity may be killed while progress bar is active so timer will throw here
      def onStop = try timer.schedule(anyToRunnable(container removeView progressBar), 250) catch none
      def onStart = try drawable.setColors(getResources getIntArray R.array.bar_colors) catch none
    }
  }

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${me getString ln_no_description}</i>"
  }
}