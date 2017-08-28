package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._

import scala.util.{Failure, Success}
import android.view.{Menu, View, ViewGroup}
import com.lightning.wallet.Utils.{app, denom}
import android.widget.{BaseAdapter, ListView, TextView}
import com.lightning.wallet.ln.Tools.{none, random, wrap}

import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.ln.Scripts.multiSig2of2
import com.lightning.wallet.helper.ThrottledWork
import android.support.v4.view.MenuItemCompat
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import fr.acinq.bitcoin.Script
import org.bitcoinj.core.Coin
import android.os.Bundle


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartDetails = findViewById(R.id.lnStartDetails).asInstanceOf[TextView]
  lazy val views = lnStartNodesList :: lnStartDetails :: Nil
  lazy val nodeView = getString(ln_ops_start_node_view)
  lazy val notifyWorking = getString(ln_notify_working)
  lazy val selectPeer = getString(ln_select_peer)
  private[this] val adapter = new NodesAdapter

  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  private[this] val worker = new ThrottledWork[AnnounceChansNumVec] {
    def work(radixNodeAliasOrNodeIdQuery: String) = LNParams.cloud findNodes radixNodeAliasOrNodeIdQuery
    def process(res: AnnounceChansNumVec) = wrap(me runOnUiThread adapter.notifyDataSetChanged)(adapter.nodes = res)
  }

  // May change back pressed action throughout an activity lifecycle
  private[this] var whenBackPressed = anyToRunnable(super.onBackPressed)
  override def onBackPressed: Unit = whenBackPressed.run

  def react(query: String) = worker onNewQuery query
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will never be updated so just update subtitle
    timer.schedule(delete(infoType).animate, 20000)
    add(subtitle, infoType).animate
  }

  // Adapter for btc tx list
  class NodesAdapter extends BaseAdapter {
    def getView(nodePosition: Int, cv: View, parent: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]
      textLine setText mkNodeView(nodes apply nodePosition)
      view
    }

    var nodes = Vector.empty[AnnounceChansNum]
    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    if (app.isAlive) {
      super.onCreate(savedState)
      wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_start)
      add(me getString ln_select_peer, Informer.LNSTATE).animate
      getSupportActionBar setTitle ln_ops_start

      // Initialize nodes list view and search with empty query
      lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
      lnStartNodesList setAdapter adapter
      react(new String)

      app.kit.wallet addCoinsSentEventListener txTracker
      app.kit.wallet addCoinsReceivedEventListener txTracker
      app.kit.wallet addTransactionConfidenceEventListener txTracker
    } else me exitTo classOf[MainActivity]
  }

  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeCoinsSentEventListener txTracker
    app.kit.wallet removeCoinsReceivedEventListener txTracker
    app.kit.wallet removeTransactionConfidenceEventListener txTracker
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }

  private def onPeerSelected(position: Int) = hideKeys {
    val (announce: NodeAnnouncement, _) = adapter getItem position
    val chan = app.ChannelManager createChannel InitData(announce)

    val socketOpenListener = new ConnectionListener {
      override def onMessage(msg: LightningMessage) = chan process msg
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == announce.nodeId) chan async CMDShutdown
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == announce.nodeId) chan async CMDShutdown
      override def onOperational(id: PublicKey, their: Init) = if (id == announce.nodeId)
        me runOnUiThread askForFunding(chan, their)
    }

    chan.listeners += new ChannelListener { chanOpenListener =>
      // Updates UI accordingly to internal changes in channel

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we ask user for a tx feerate
          me runOnUiThread askForFeerate(chan, cmd, accept)

        case (_, _, _, CLOSING) =>
          // Disconnect, remote Error, back button pressed
          ConnectionManager.listeners -= socketOpenListener
          chan.listeners -= chanOpenListener
          me runOnUiThread setListView

        case (_, _, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Never forget to remove listeners related to local view
          app.ChannelManager.all = chan +: app.ChannelManager.all
          ConnectionManager.listeners -= socketOpenListener
          chan.listeners -= chanOpenListener
          me exitTo classOf[LNOpsActivity]
      }

      override def onError = {
        case error: Throwable =>
          // Aborts channel, updates UI
          chan process CMDShutdown
      }
    }

    ConnectionManager.listeners += socketOpenListener
    whenBackPressed = anyToRunnable(chan async CMDShutdown)
    ConnectionManager requestConnection announce
    me setPeerView position
  }

  // UI utilities

  private def mkNodeView(info: AnnounceChansNum) = {
    val (announce: NodeAnnouncement, connections) = info
    val humanConnections = app.plurOrZero(chansNumber, connections)
    val humanId = announce.nodeId.toString grouped 3 mkString "\u0020"
    nodeView.format(announce.alias, humanConnections, humanId).html
  }

  private def setListView = {
    whenBackPressed = anyToRunnable(super.onBackPressed)
    update(selectPeer, Informer.LNSTATE).animate
    setVis(View.VISIBLE, View.GONE)
    app toast ln_ops_start_abort
  }

  private def setPeerView(pos: Int) = {
    MenuItemCompat collapseActionView searchItem
    lnStartDetails setText mkNodeView(adapter getItem pos)
    update(notifyWorking, Informer.LNSTATE).animate
    setVis(View.GONE, View.VISIBLE)
  }

  def askForFunding(chan: Channel, their: Init) = {
    val walletBalance = denom withSign app.kit.currentBalance
    val maxCapacity = denom withSign LNParams.maxChannelCapacity
    val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), getString(ln_ops_start_fund_title).html, content)
    val rateManager = new RateManager(getString(amount_hint_newchan).format(maxCapacity, walletBalance), content)

    def attempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_sum_big
      case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_small

      case Success(ms) => rm(alert) {
        val amountSat = ms.amount / sat2msatFactor
        val chanReserveSat = (amountSat * LNParams.reserveToFundingRatio).toLong
        val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
        val localParams = LNParams.makeLocalParams(chanReserveSat, finalPubKeyScript, System.currentTimeMillis)
        chan async CMDOpenChannel(localParams, random getBytes 32, LNParams.broadcaster.feeRatePerKw, 0, their, amountSat)
      }
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(attempt)
  }

  def askForFeerate(chan: Channel, cmd: CMDOpenChannel, accept: AcceptChannel): Unit = {
    val multisig = multiSig2of2(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
    val scriptPubKey = Script.write(Script pay2wsh multisig)

    new TxProcessor {
      val funding = Coin valueOf cmd.fundingAmountSat
      val pay = P2WSHData(funding, scriptPubKey)
      chooseFee

      def processTx(password: String, fee: Coin) = chan async {
        val fundingTransaction: fr.acinq.bitcoin.Transaction = makeTx(password, fee)
        val outIndex = Scripts.findPubKeyScriptIndex(fundingTransaction, scriptPubKey)
        fundingTransaction -> outIndex
      }

      def onTxFail(exc: Throwable) =
        mkForm(mkChoiceDialog(me delayUI askForFeerate(chan, cmd, accept),
          none, dialog_ok, dialog_cancel), null, errorWhenMakingTx apply exc)
    }
  }
}