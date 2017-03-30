package com.lightning.wallet

import android.nfc.NfcEvent
import android.text.method.LinkMovementMethod
import com.lightning.wallet.R.string._
import android.content._
import android.text.Html
import com.lightning.wallet.Utils._
import com.lightning.wallet.R.drawable.{await, conf1, dead}
import org.bitcoinj.core.Utils.HEX
import android.content.DialogInterface.BUTTON_POSITIVE
import collection.JavaConverters.seqAsJavaListConverter
import com.lightning.wallet.Utils.{runAnd, wrap}
import android.widget._
import android.view.{Menu, MenuItem, View, ViewGroup}
import org.ndeftools.Message
import org.ndeftools.util.activity.NfcReaderActivity
import spray.json._
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT

import concurrent.ExecutionContext.Implicits.global
import android.os.Bundle
import Utils.app
import com.lightning.wallet.ln.Tools.none
import android.database.Cursor
import android.support.v4.view.MenuItemCompat
import android.support.v7.widget.SearchView.OnCloseListener
import android.view.View.OnClickListener
import android.webkit.URLUtil
import com.lightning.wallet.helper.JsonHttpUtils._
import com.lightning.wallet.helper._
import com.lightning.wallet.ln.wire.UpdateAddHtlc
import com.lightning.wallet.ln._
import fr.acinq.bitcoin.{BinaryData, Crypto, MilliSatoshi}
import org.bitcoinj.core.{Address, Sha256Hash, Transaction}
import rx.lang.scala.schedulers.IOScheduler

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


class LNActivity extends NfcReaderActivity
with ToolbarActivity with HumanTimeDisplay
with ListUpdater { me =>

  def onFail(e: Throwable): Unit = negBld(dialog_ok).setMessage(e.getMessage).show
  lazy val lnItemsList = findViewById(R.id.lnItemsList).asInstanceOf[ListView]
  lazy val lnTitle = me getString ln_title
  //lazy val adapter = new LNAdapter
  private[this] var hasNfc = true
  private[this] val TEXT = "text"

  // Adapter for ln txs list
//  class LNAdapter extends BaseAdapter {
//    def getView(paymentPosition: Int, cv: View, parent: ViewGroup) = {
//      val view = if (null == cv) getLayoutInflater.inflate(R.layout.frag_transaction_line_narrow, null) else cv
//      val hold = if (null == view.getTag) new LNView(view) else view.getTag.asInstanceOf[LNView]
//      hold fillView getItem(paymentPosition)
//      view
//    }
//
//    var payments = Vector.empty[HtlcWrap]
//    def getItemId(position: Int) = position
//    def getItem(position: Int) = payments(position)
//    def getCount = payments.length
//  }

  // Payment history and search results loader
  // Remembers last search term in case of reload
//  class RecentPayments extends ReactCallback(me) {
//    val observeTablePath = app.db sqlPath Payments.table
//    private var lastBundle: Bundle = null
//
//    def onCreateLoader(id: Int, b: Bundle) =
//      if (lastBundle == null) recent else search
//
//    def reload(bundle: Bundle) = runAnd(lastBundle = bundle) {
//      getSupportLoaderManager.restartLoader(1, null, this).forceLoad
//    }
//
//    def search = new HtlcWrapLoader {
//      val queryTerm = lastBundle.getString(TEXT) + "*"
//      def getCursor = app.db.select(Payments.searchVirtualSql, queryTerm)
//    }
//
//    def recent = new HtlcWrapLoader {
//      val cutoff = System.currentTimeMillis - 3600 * 24 * 2 * 1000
//      def getCursor = app.db.select(Payments selectRecentSql cutoff)
//    }
//
//    abstract class HtlcWrapLoader extends ReactLoader[HtlcWrap](me) {
//      def createItem(shiftCursor: RichCursor) = PaymentsWrap toHtlcWrap shiftCursor
//      val consume: Vector[HtlcWrap] => Unit = items => println(items.size)
//    }
//  }


//  lazy val recentPayments = new RecentPayments

  // Payment view
//  class LNView(view: View)
//  extends TxViewHolder(view) {
//    def fillView(hw: HtlcWrap) = {
//      println(s"Filling view $hw")
////      val paymentMarking = if (lnp.incoming > 0) sumIn else sumOut
////      val humanSum = paymentMarking format lnp.invoice.amount.bit
////      val time = when(System.currentTimeMillis, lnp.stamp)
////      val image = if (lnp.status == Payments.fail) dead
////        else if (lnp.status == Payments.await) await
////        else conf1
////
////      transactSum setText Html.fromHtml(humanSum)
////      transactWhen setText Html.fromHtml(time)
////      transactCircle setImageResource image
//    }
//  }

  // NDEF management
  // Do not react in any way by default
  def onNfcFeatureNotFound = hasNfc = false
  def onNfcStateChange(ok: Boolean) = none
  def onNfcStateDisabled = none
  def onNfcStateEnabled = none

  def readNonNdefMessage = none
  def readEmptyNdefMessage = none
  def readNdefMessage(msg: Message) = none

  // Temporairly update title and subtitle info
  def notifySubTitle(subtitle: String, infoType: Int) = {
    add(subtitle, infoType).timer.schedule(me del infoType, 25000)
    me runOnUiThread ui
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    wrap(initToolbar)(me setContentView R.layout.activity_ln)
    add(me getString ln_notify_connecting, Informer.LNSTATE).ui.run

      //wrap(initToolbar)(me setContentView R.layout.activity_ln)
//      add(me getString ln_notify_connecting, Informer.LNSTATE).ui.run
//      app.prefs.edit.putString(AbstractKit.LANDING, AbstractKit.LN).commit
//
//      lnItemsList setAdapter adapter
//      recentPayments.reload(null)
//      timer.schedule(anyToRunnable {
//        println("external update notification happened")
//        app.getContentResolver.notifyChange(app.db sqlPath Payments.table, null)
//      }, 10000, 5000)

      //(new BlindTokensListenerSpec).allTests
//      startListUpdates(adapter, lnItemsList)

//      val a = System.currentTimeMillis
//
//      val wraps = for (n <- 1 to 50000) yield {
//        val preimage = BinaryData(Utils.rand getBytes 32)
//
//        val add = UpdateAddHtlc(100, 100, 10000, 1000, Crypto.sha256(preimage), BinaryData("0x0000"))
//        val htlc = Htlc(n % 2 == 0, add, None, Some(s"Hello $n"))
//
//        HtlcWrap(if (n % 2 == 0) Some(preimage.toString) else None, htlc,
//          if (n % 10 == 0) Payments.waitHidden else Payments.waitVisible)
//      }
//
//      println(System.currentTimeMillis - a)
//
//      app.db txWrap {
//        wraps.foreach(PaymentsWrap.put)
//      }
//
//      println(System.currentTimeMillis - a)

//      val a = System.currentTimeMillis
//      val cursor = app.db.select(Payments.selectRecentSql(0))
//      cursor.moveToFirst()
//      println(PaymentsWrap.toHtlcWrap(RichCursor(cursor)))
//      println(System.currentTimeMillis - a)

      //(new BlindTokensListenerSpec).allTests
  }

  // Menu area
  import android.support.v7.widget.SearchView
  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_transactions_ops, menu)
    val view = MenuItemCompat.getActionView(menu findItem R.id.action_search)
    val search = view.asInstanceOf[SearchView]

    search setOnQueryTextListener
      new SearchView.OnQueryTextListener {
        override def onQueryTextSubmit(text: String) = true
        override def onQueryTextChange(text: String) = runAnd(true) {
          val bundle = if (text.isEmpty) null else mkBundle(TEXT -> text)
          //recentPayments reload bundle
        }
      }

    true
  }

  override def onOptionsItemSelected(m: MenuItem) = runAnd(true) {
    if (m.getItemId == R.id.actionGoBtcWallet) me goTo classOf[BtcActivity]
    else if (m.getItemId == R.id.actionSetBackupServer) new SetBackupServer
    else if (m.getItemId == R.id.actionRefillChannel) startChannelRefill
    else if (m.getItemId == R.id.actionCloseChannel) closeChannel
  }

  override def onResume = wrap(super.onResume)(app.TransData.value = null)

  def startChannelRefill: RateManager = {
//    val builder = negPosBld(dialog_back, dialog_next)
//    val content = getLayoutInflater.inflate(R.layout.frag_input_receive, null)
//    val dialog = mkForm(builder, me getString ln_refill, content)
//    val ok = dialog getButton BUTTON_POSITIVE
//    val man = new RateManager(content)
//
//    ok setOnClickListener new OnClickListener {
//      def onClick(view: View) = man.result match {
//        case Failure(amountIsEmpty) => toast(dialog_sum_empty)
//        case Success(ms) if ms.coin isLessThan MIN_NONDUST_OUTPUT => toast(dialog_sum_dusty)
//        case Success(ms) => rm(prev = dialog)(fun = completePay(ms).showForm)
//      }
//    }
//
//    def completePay(msat: MSat) =
//      new CompletePay(me, msat, app.kit.currentAddress) {
//        val title = Html fromHtml PayData(amount, adr).humanSum
//        def restart = startChannelRefill setSum Try(msat)
//        def confirm = println("ok")
//      }
//
//    man
    null
  }

  class SetBackupServer {
//    val (view, field) = str2Tuple(LNParams.standaloneCloudKey.getPublicKeyAsHex)
//    val dialog = mkChoiceDialog(ok = proceed, no = none, dialog_next, dialog_back)
//    val alert = mkForm(dialog, title = getString(ln_backup_key).html, view)
//    field setTextIsSelectable true
//
//    def proceed: Unit = rm(alert) {
//      val (view1, field) = generatePasswordPromptView(inpType = textType, txt = ln_backup_ip)
//      val dialog = mkChoiceDialog(trySave(field.getText.toString), new SetBackupServer, dialog_ok, dialog_back)
//      for (savedPrivCloud <- StandaloneCloud.getTry) field setText savedPrivCloud.url
//      mkForm(dialog, getString(ln_backup), view1)
//    }
//
//    def trySave(ip: String) =
//      if (ip.isEmpty) StandaloneCloud.remove
//      else if (URLUtil isValidUrl ip) this proceedSave StandaloneCloud(ip)
//      else mkForm(me negBld dialog_ok, null, me getString ln_backup_url_error)
//
//    def proceedSave(pc: StandaloneCloud) =
//      pc.tryIfWorks(rand getBytes 32).map(_ => StandaloneCloud save pc)
//        .foreach(_ => me runOnUiThread toast(ln_backup_success),
//          err => me runOnUiThread onError(err), println)
//
//    def onError(error: Throwable): Unit = error.getMessage match {
//      case "errkey" => mkForm(me negBld dialog_ok, null, me getString ln_backup_key_error)
//      case "errsig" => mkForm(me negBld dialog_ok, null, me getString ln_backup_sig_error)
//      case _ => mkForm(me negBld dialog_ok, null, me getString ln_backup_net_error)
//    }
  }

  def closeChannel = passPlus(me getString ln_close)(none) { pass =>
    println(pass)
  }
}