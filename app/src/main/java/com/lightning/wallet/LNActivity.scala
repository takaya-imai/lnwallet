package com.lightning.wallet

import android.widget._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.lncloud._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.lncloud.ImplicitConversions._

import com.lightning.wallet.helper.{ReactCallback, ReactLoader, RichCursor}
import com.lightning.wallet.ln.wire.{CommitSig, RevokeAndAck}
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import com.lightning.wallet.ln.Tools.{runAnd, wrap}
import android.view.{Menu, MenuItem, View}
import scala.util.{Failure, Success, Try}

import android.support.v7.widget.SearchView.OnQueryTextListener
import android.content.DialogInterface.BUTTON_POSITIVE
import org.ndeftools.util.activity.NfcReaderActivity
import com.github.clans.fab.FloatingActionMenu
import android.support.v4.view.MenuItemCompat
import com.lightning.wallet.ln.Tools.none
import org.bitcoinj.uri.BitcoinURI
import org.bitcoinj.core.Address
import android.app.AlertDialog
import org.ndeftools.Message
import android.os.Bundle
import java.util.Date
import Utils.app


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
with ToolbarActivity with ListUpdater
with SearchBar { me =>

  val imgMap = Array(await, await, await, conf1, dead)
  lazy val paymentStatesMap = getResources getStringArray R.array.ln_payment_states
  lazy val addFailures = getResources getStringArray R.array.txs_ln_add_failures
  lazy val routesLeft = getResources getStringArray R.array.ln_retry_routes_left
  lazy val fab = findViewById(R.id.fab).asInstanceOf[FloatingActionMenu]
  lazy val paymentsViewProvider = new PaymentsViewProvider

  lazy val adapter = new CutAdapter[PaymentInfo] {
    def getItem(position: Int) = visibleItems(position)
    def getHolder(view: View) = new TxViewHolder(view) {

      def fillView(info: PaymentInfo) = {
        val timestamp = new Date(info.request.timestamp * 1000)
        val purpose = info.request.description.right getOrElse new String

        val marking = info match {
          case in: IncomingPayment => sumIn.format(denom formatted in.received)
          case out: OutgoingPayment => sumOut.format(denom formatted out.received)
        }

        transactWhen setText when(System.currentTimeMillis, timestamp).html
        transactCircle setImageResource imgMap(info.actualStatus)
        transactSum setText s"$marking\u00A0$purpose".html
      }
    }
  }

  private[this] var sendPayment: PaymentRequest => Unit = none
  private[this] var makePaymentRequest = anyToRunnable(none)
  private[this] var whenStop = anyToRunnable(super.onStop)
  override def onStop = whenStop.run

  def evacuate = me exitTo classOf[LNOpsActivity]
  def react(qs: String) = paymentsViewProvider reload qs
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will updated separately so just update subtitle
    timer.schedule(delete(infoType), 10000)
    add(subtitle, infoType).flash.run
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
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

  override def onResume: Unit = wrap(super.onResume) {
    app.prefs.edit.putString(AbstractKit.LANDING, AbstractKit.LIGHTNING).commit
    app.ChannelManager.alive.headOption map manageActive getOrElse evacuate
  }

  // APP MENU

  override def onOptionsItemSelected(menu: MenuItem) = runAnd(true) {
    if (menu.getItemId == R.id.actionCloseChannel) closeAllActiveChannels
    else if (menu.getItemId == R.id.actionSettings) mkSetsForm
  }

  def closeAllActiveChannels = checkPass(me getString ln_close) { pass =>
    // Close all of the channels just in case we have more than one active
    for (chan <- app.ChannelManager.alive) chan process CMDShutdown
  }

  def manageActive(chan: Channel) = {
    list setOnItemClickListener onTap { pos =>
      val detailsWrapper = getLayoutInflater.inflate(R.layout.frag_ln_payment_details, null)
      val paymentDetails = detailsWrapper.findViewById(R.id.paymentDetails).asInstanceOf[TextView]
      val paymentRetryAgain = detailsWrapper.findViewById(R.id.paymentRetryAgain).asInstanceOf[Button]
      val paymentProof = detailsWrapper.findViewById(R.id.paymentProof).asInstanceOf[Button]
      val paymentHash = detailsWrapper.findViewById(R.id.paymentHash).asInstanceOf[Button]

      val payment = adapter getItem pos
      val description = me getDescription payment.request
      val humanStatus = s"<strong>${paymentStatesMap apply payment.status}</strong>"
      paymentHash.setText(payment.request.paymentHash.toString grouped 8 mkString "\u0020")
      paymentHash setOnClickListener onButtonTap(app setBuffer payment.request.paymentHash.toString)
      if (payment.status == SUCCESS) paymentProof setVisibility View.VISIBLE

      paymentProof setOnClickListener onButtonTap {
        app setBuffer getString(ln_proof).format(payment.request.nodeId.toString, description,
          PaymentRequest write payment.request, payment.request.paymentHash.toString,
          payment.preimage.toString)
      }

      payment match {
        case in: IncomingPayment =>
          val humanReceived = humanFiat(sumIn.format(denom withSign in.received), in.received)
          mkForm(me negBld dialog_ok, getString(ln_incoming_title).format(humanStatus).html, detailsWrapper)
          paymentDetails setText s"$description<br><br>$humanReceived".html

        case out: OutgoingPayment =>
          val fee = MilliSatoshi(out.routing.amountWithFee - out.request.finalSum.amount)
          val title = getString(ln_outgoing_title).format(sumOut.format(denom withSign fee), humanStatus)
          val humanSent = humanFiat(sumOut.format(denom withSign out.request.finalSum), out.request.finalSum)
          val alert = mkForm(me negBld dialog_ok, title.html, detailsWrapper)
          paymentDetails setText s"$description<br><br>$humanSent".html

          paymentRetryAgain setOnClickListener onButtonTap {
            collectRoutesAndSend(alert, out.request, RetryAddHtlc)
          }

          // No more routes left to try and request is not expired so we can try to send it again
          val canRetry = out.status == FAILURE && out.routing.routes.isEmpty && out.request.isFresh
          if (canRetry) paymentRetryAgain setVisibility View.VISIBLE
      }
    }

    toolbar setOnClickListener onButtonTap {
      wrap(adapter.notifyDataSetChanged)(changeDenom)
      updTitle
    }

    def updTitle = animateTitle {
      val canSend = chan.pull(_.localCommit.spec.toLocalMsat)
      denom withSign MilliSatoshi(canSend getOrElse 0L)
    }

    def collectRoutesAndSend(alert: AlertDialog, request: PaymentRequest,
                      out2Command: OutgoingPayment => CMDAddHtlc) =

      rm(alert) {
        notifySubTitle(me getString ln_send, Informer.LNPAYMENT)
        app.ChannelManager.outPaymentObs(request).foreach(_ match {
          case Some(outPayment) => chan process out2Command(outPayment)
          case _ => onFail(me getString err_general)
        }, onRouteError)
      }

    def onRouteError(err: Throwable) = err.getMessage match {
      case "fromblacklisted" => onFail(me getString err_ln_black)
      case "noroutefound" => onFail(me getString err_ln_no_route)
      case techDetails => onFail(techDetails)
    }

    val chanListener = new ChannelListener {
      // Updates UI accordingly to changes in channel

      override def onBecome = {
        case (_, norm: NormalData, _, _) if norm.isClosing => evacuate
        case (_, _: ClosingData | _: NegotiationsData | _: WaitFundingDoneData, _, _) => evacuate
        case (_, _: NormalData, _, SYNC) => update(me getString ln_notify_connecting, Informer.LNSTATE).flash.run
        case (_, _: NormalData, _, NORMAL) => update(me getString ln_notify_operational, Informer.LNSTATE).flash.run
      }

      override def onError = {
        case AddException(cmd: RetryAddHtlc, _) => Tools log s"Retry payment rejected $cmd"
        case AddException(cmd: SilentAddHtlc, _) => Tools log s"Silent payment rejected $cmd"
        case AddException(_: PlainAddHtlc, code) => onFail(addFailures apply code)
        case _ => chan process CMDShutdown
      }

      override def onProcess = {
        case (_, _, retry: RetryAddHtlc) =>
          val humanLeft = app.plurOrZero(routesLeft, retry.out.routing.routes.size)
          me runOnUiThread app.toast(me getString ln_retry format humanLeft)

        case (_, norm: NormalData, _: CommitSig)
          // GUARD: notify and vibrate because HTLC is fulfilled
          if norm.commitments.localCommit.spec.fulfilled.nonEmpty =>
          notifySubTitle(me getString ln_done, Informer.LNSUCCESS)
          Vibr vibrate Vibr.confirmed
          updTitle

        case (_, _, cmd: CMDAddHtlc) => Vibr vibrate Vibr.processed
        case (_, _, _: RevokeAndAck | _: CommitSig) => updTitle
      }
    }

    sendPayment = request => {
      val info = me getDescription request
      val canSend = chan.pull(_.localCommit.spec.toLocalMsat)
      val maxMsat = MilliSatoshi apply math.min(canSend getOrElse 0L, maxHtlcValue.amount)
      val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
      val rateManager = new RateManager(getString(amount_hint_maxamount).format(denom withSign maxMsat), content)
      val alert = mkForm(negPosBld(dialog_cancel, dialog_pay), getString(ln_send_title).format(info).html, content)

      def sendAttempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if maxMsat < ms => app toast dialog_sum_big
        case Success(ms) if htlcMinimumMsat > ms.amount => app toast dialog_sum_small
        case Success(ms) if request.amount.exists(_ > ms) => app toast dialog_sum_small
        case Success(ms) if request.amount.exists(_ * 2 < ms) => app toast dialog_sum_big
        case Success(ms) => collectRoutesAndSend(alert, request, PlainAddHtlc)
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(sendAttempt)
      for (sum <- request.amount) rateManager setSum Try(sum)
    }

    makePaymentRequest = anyToRunnable {
      val canReceive = chan.pull(_.localCommit.spec.toRemoteMsat)
      val content = getLayoutInflater.inflate(R.layout.frag_ln_input_receive, null, false)
      val inputDescription = content.findViewById(R.id.inputDescription).asInstanceOf[EditText]
      val alert = mkForm(negPosBld(dialog_cancel, dialog_ok), me getString ln_receive_title, content)
      val maxMsat = MilliSatoshi apply math.min(canReceive getOrElse 0L, maxHtlcValue.amount)
      val hint = getString(amount_hint_maxamount).format(denom withSign maxMsat)
      val rateManager = new RateManager(hint, content)

      def proceed(amount: Option[MilliSatoshi], preimg: BinaryData) = chan.pull(_.channelId) foreach { id =>
        val (description, hash, stamp) = (inputDescription.getText.toString.trim, Crypto sha256 preimg, 3600 * 6)
        val paymentRequest = PaymentRequest(chainHash, amount, hash, nodePrivateKey, description, None, stamp)
        bag putPaymentInfo IncomingPayment(preimg, paymentRequest, MilliSatoshi(0), id, HIDDEN)
        app.TransData.value = paymentRequest
        me goTo classOf[RequestActivity]
      }

      def receiveAttempt = rateManager.result match {
        case Success(ms) if maxMsat < ms => app toast dialog_sum_big
        case Success(ms) if htlcMinimumMsat > ms.amount => app toast dialog_sum_small

        case result => rm(alert) {
          notifySubTitle(me getString ln_pr_make, Informer.LNPAYMENT)
          <(proceed(result.toOption, bag.newPreimage), onFail)(none)
        }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(receiveAttempt)
    }

    whenStop = anyToRunnable {
      chan.listeners -= chanListener
      super.onStop
    }

    chan.listeners += chanListener
    chanListener reloadOnBecome chan
    checkTransData
    updTitle
  }

  // DATA READING AND BUTTON ACTIONS

  def checkTransData = app.TransData.value match {
    case uri: BitcoinURI => me goTo classOf[BtcActivity]
    case adr: Address => me goTo classOf[BtcActivity]

    case pr: PaymentRequest =>
      app.TransData.value = null
      if (pr.isFresh) sendPayment(pr)
      else onFail(me getString err_ln_old)

    case unusable =>
      app.TransData.value = null
      Tools log s"Unusable $unusable"
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
    def recent = new ExtendedPaymentInfoLoader { def getCursor = bag.recentPayments }
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

  def getDescription(pr: PaymentRequest) = pr.description match {
    case Right(description) if description.nonEmpty => description take 140
    case Left(descriptionHash) => s"<i>${descriptionHash.toString}</i>"
    case _ => s"<i>${me getString ln_no_description}</i>"
  }
}