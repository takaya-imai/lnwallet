package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.ln.Scripts.multiSig2of2
import com.lightning.wallet.helper.ThrottledWork
import com.lightning.wallet.lnutils.RatesSaver
import android.support.v4.view.MenuItemCompat
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import org.bitcoinj.core.Coin
import android.os.Bundle

import android.widget.{BaseAdapter, Button, ListView, TextView}
import com.lightning.wallet.ln.Tools.{none, random, wrap}
import fr.acinq.bitcoin.{MilliSatoshi, Script}
import com.lightning.wallet.Utils.{app, denom}
import android.view.{Menu, View, ViewGroup}
import scala.util.{Failure, Success}


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartDetailsText = findViewById(R.id.lnStartDetailsText).asInstanceOf[TextView]
  lazy val lnCancel = findViewById(R.id.lnCancel).asInstanceOf[Button]

  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val views = lnStartNodesList :: findViewById(R.id.lnStartDetails) :: Nil
  lazy val nodeView = getString(ln_ops_start_node_view)
  lazy val notifyWorking = getString(notify_working)
  lazy val selectPeer = getString(ln_select_peer)
  private[this] val adapter = new NodesAdapter

  // May change back pressed action throughout an activity lifecycle
  private[this] var whenBackPressed = anyToRunnable(super.onBackPressed)
  override def onBackPressed: Unit = whenBackPressed.run

  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  private[this] val worker = new ThrottledWork[String, AnnounceChansNumVec] {
    def work(radixNodeAliasOrNodeIdQuery: String) = LNParams.cloud.connector findNodes radixNodeAliasOrNodeIdQuery
    def process(res: AnnounceChansNumVec) = wrap(me runOnUiThread adapter.notifyDataSetChanged)(adapter.nodes = res)
    def error(err: Throwable) = Tools errlog err
  }

  def react(query: String) = worker addWork query
  def notifySubTitle(subtitle: String, infoType: Int) = {
    // Title will never be updated so just update subtitle
    timer.schedule(delete(infoType), 10000)
    add(subtitle, infoType).flash.run
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

    if (app.isAlive) {
      super.onCreate(savedState)

      // Set action bar, main view content, animate title
      wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_start)
      add(me getString ln_select_peer, Informer.LNSTATE).flash.run
      animateTitle(me getString ln_ops_start)

      // Wire up list and load peers with empty query string
      lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
      lnStartNodesList setAdapter adapter
      react(new String)

      app.kit.wallet addCoinsSentEventListener txTracker
      app.kit.wallet addCoinsReceivedEventListener txTracker
      app.kit.wallet addTransactionConfidenceEventListener txTracker
    } else me exitTo classOf[MainActivity]

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
      override def onMessage(message: LightningMessage) = chan process message
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == announce.nodeId) chan process CMDShutdown
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == announce.nodeId) chan process CMDShutdown
      override def onOperational(nodeId: PublicKey, their: Init) = if (nodeId == announce.nodeId)
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
          // Channel has just been saved to db so we can proceed
          app.ChannelManager.all = chan +: app.ChannelManager.all
          ConnectionManager.listeners -= socketOpenListener
          chan.listeners -= chanOpenListener
          me exitTo classOf[LNOpsActivity]
      }
    }

    // Sending CMDShutdown to channel indirectly updates the view
    lnCancel setOnClickListener onButtonTap(chan process CMDShutdown)
    whenBackPressed = anyToRunnable(chan process CMDShutdown)
    ConnectionManager.listeners += socketOpenListener
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
    update(selectPeer, Informer.LNSTATE).flash.run
    setVis(View.VISIBLE, View.GONE)
    app toast ln_ops_start_abort
  }

  private def setPeerView(pos: Int) = {
    MenuItemCompat collapseActionView searchItem
    lnStartDetailsText setText mkNodeView(adapter getItem pos)
    update(notifyWorking, Informer.LNSTATE).flash.run
    setVis(View.GONE, View.VISIBLE)
  }

  def askForFunding(chan: Channel, their: Init) = {
    val walletBalance = denom withSign app.kit.currentBalance
    val maxCapacity = denom withSign LNParams.maxChannelCapacity
    val minCapacity: MilliSatoshi = RatesSaver.rates.feeLive multiply 4

    val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), getString(ln_ops_start_fund_title).html, content)
    val extra = getString(amount_hint_newchan).format(denom withSign minCapacity, maxCapacity, walletBalance)
    val rateManager = new RateManager(extra, content)

    def askAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) if ms.amount < minCapacity.amount => app toast dialog_sum_small
      case Success(ms) if ms.amount > LNParams.maxChannelCapacity.amount => app toast dialog_sum_big

      case Success(ms) => rm(alert) {
        val amountSat = ms.amount / sat2msatFactor
        val chanReserveSat = (amountSat * LNParams.reserveToFundingRatio).toLong
        val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
        val localParams = LNParams.makeLocalParams(chanReserveSat, finalPubKeyScript, System.currentTimeMillis)
        chan process CMDOpenChannel(localParams, random getBytes 32, 10000 /* TODO: LNParams.broadcaster.feeRatePerKw */, 0, their, amountSat)
      }
    }

    val ok = alert getButton BUTTON_POSITIVE
    ok setOnClickListener onButtonTap(askAttempt)
  }

  def askForFeerate(chan: Channel, cmd: CMDOpenChannel, accept: AcceptChannel): Unit = {
    val multisig = multiSig2of2(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
    val scriptPubKey = Script.write(Script pay2wsh multisig)

    new TxProcessor {
      val funding = Coin valueOf cmd.fundingAmountSat
      val pay = P2WSHData(funding, scriptPubKey)
      chooseFee

      def processTx(pass: String, fee: Coin) =
        <(makeTx(pass, fee): fr.acinq.bitcoin.Transaction, onTxFail) { fundTx =>
          val outIndex = Scripts.findPubKeyScriptIndex(fundTx, scriptPubKey)
          chan process fundTx -> outIndex
        }

      def onTxFail(err: Throwable) =
        mkForm(mkChoiceDialog(me delayUI askForFeerate(chan, cmd, accept),
          none, dialog_ok, dialog_cancel), null, messageWhenMakingTx apply err)
    }
  }
}