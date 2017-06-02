package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.lncloud.ImplicitConversions._

import scala.util.{Failure, Success}
import android.view.{Menu, View, ViewGroup}
import com.lightning.wallet.Utils.{app, sumIn}
import android.widget.{BaseAdapter, ListView, TextView}
import com.lightning.wallet.ln.Tools.{none, random, wrap}
import com.lightning.wallet.helper.{SocketListener, ThrottledWork}
import com.lightning.wallet.ln.wire.{AcceptChannel, Init, NodeAnnouncement}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.lncloud.{LNCloudPrivateSaver, RatesSaver}
import com.lightning.wallet.ln.Scripts.{ScriptEltSeq, multiSig2of2}

import concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.Utils.humanPubkey
import android.support.v4.view.MenuItemCompat
import org.bitcoinj.script.ScriptBuilder
import scala.concurrent.Future
import fr.acinq.bitcoin.Script
import org.bitcoinj.core.Coin
import android.os.Bundle

import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import android.content.DialogInterface.BUTTON_POSITIVE


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartDetails = findViewById(R.id.lnStartDetails).asInstanceOf[TextView]
  lazy val views = lnStartNodesList :: lnStartDetails :: Nil
  lazy val nodeView = getString(ln_ops_start_node_view)
  lazy val notifyWorking = getString(ln_notify_working)
  lazy val selectPeer = getString(ln_select_peer)
  private[this] val adapter = new NodesAdapter

  private[this] var whenBackPressed =
    anyToRunnable(super.onBackPressed)

  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  private[this] val worker = new ThrottledWork[AnnounceChansNumVec] {
    def work(radixNodeAliasOrNodeIdQuery: String) = actualCloud findNodes radixNodeAliasOrNodeIdQuery
    def process(res: AnnounceChansNumVec) = wrap(me runOnUiThread adapter.notifyDataSetChanged)(adapter.nodes = res)
    lazy val actualCloud = LNCloudPrivateSaver.actualCloudObject
  }

  def react(query: String) = worker onNewQuery query
  def notifySubTitle(subtitle: String, infoType: Int) = {
    add(subtitle, infoType).timer.schedule(me del infoType, 25000)
    me runOnUiThread ui
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
      wrap(initToolbar)(me setContentView R.layout.activity_ln_start)
      lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
      add(me getString ln_select_peer, Informer.LNSTATE).ui.run
      getSupportActionBar setTitle ln_ops_start
      lnStartNodesList setAdapter adapter
      react(new String)

      app.kit.wallet addCoinsSentEventListener tracker
      app.kit.wallet addCoinsReceivedEventListener tracker
      app.kit.wallet addTransactionConfidenceEventListener tracker
    } else me exitTo classOf[MainActivity]
  }

  override def onBackPressed = whenBackPressed.run
  override def onDestroy = wrap(super.onDestroy) {
    app.kit.wallet removeCoinsSentEventListener tracker
    app.kit.wallet removeCoinsReceivedEventListener tracker
    app.kit.wallet removeTransactionConfidenceEventListener tracker
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }

  private def onPeerSelected(position: Int): Unit = hideKeys {
    val (announce: NodeAnnouncement, _) = adapter getItem position

    val kit: ChannelKit =
      ChannelKit apply new Channel {
        data = InitData apply announce
        state = Channel.WAIT_FOR_INIT
      }

    val sockOpenListener = new SocketListener {
      // They can interrupt socket connection at any time
      override def onDisconnect = me runOnUiThread setListView
    }

    val channelOpenListener = new StateMachineListener { self =>
      override def onBecome: PartialFunction[Transition, Unit] = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we ask user to sign a funding
          askForFeerate(kit.chan, cmd, accept)

        case (_, _, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // Peer has provided a signature for a first commit
          kit.socket.listeners -= sockOpenListener
          kit.chan.listeners -= self
          app.TransData.value = kit
          finish
      }

      override def onPostProcess = {
        case theirInitMessage: Init =>
          // Connection works, ask user for a funding
          askForFunding(kit.chan, theirInitMessage)
      }
    }

    me setPeerView position
    whenBackPressed = anyToRunnable {
      kit.chan.listeners -= channelOpenListener
      kit.socket.listeners -= sockOpenListener
      Future { kit.chan process CMDShutdown }
      setListView
    }

    kit.chan.listeners += channelOpenListener
    kit.socket.listeners += sockOpenListener
    kit.socket.start
  }

  // UI utilities

  private def mkNodeView(info: AnnounceChansNum) = {
    val (announce: NodeAnnouncement, quantity) = info
    val humanId = humanPubkey(announce.nodeId.toString)
    val humanChansNumber = app.plurOrZero(chansNumber, quantity)
    nodeView.format(announce.alias, humanChansNumber, humanId).html
  }

  private def setListView = {
    whenBackPressed = anyToRunnable(super.onBackPressed)
    update(selectPeer, Informer.LNSTATE).ui.run
    setVis(View.VISIBLE, View.GONE)
    app toast ln_ops_abort
  }

  private def setPeerView(position: Int) = {
    MenuItemCompat collapseActionView searchItem
    lnStartDetails setText mkNodeView(adapter getItem position)
    update(notifyWorking, Informer.LNSTATE).ui.run
    setVis(View.GONE, View.VISIBLE)
  }

  def askForFunding(chan: Channel, their: Init) = runOnUiThread {
    val humanBalance = sumIn format withSign(app.kit.currentBalance)
    val humanCap = sumIn format withSign(LNParams.maxChannelCapacity)
    val titleTop = getString(ln_ops_start_fund_title).format(humanBalance, humanCap).html
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_noaddress, null, false)
    val builder = negPosBld(dialog_cancel, dialog_next)

    def showFundingForm = {
      val alert = mkForm(builder, titleTop, content)
      val rateManager = new RateManager(content)

      def attempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_capacity
        case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_dusty
        case Success(ms) => rm(alert) { openChannel(ms.amount / satFactor) /* proceed */ }
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(attempt)
    }

    def openChannel(amountSat: Long) = Future {
      val chanReserveSat = (amountSat * LNParams.reserveToFundingRatio).toLong
      val initFeeratePerKw = LNParams feerateKB2Kw RatesSaver.rates.feeLive.value
      val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
      val localParams = LNParams.makeLocalParams(chanReserveSat, finalPubKeyScript, System.currentTimeMillis)
      chan process CMDOpenChannel(localParams, random getBytes 32, 10000, pushMsat = 0L, their, amountSat)
    }

    // Pick a sum
    showFundingForm
  }

  def askForFeerate(chan: Channel, cmd: CMDOpenChannel, accept: AcceptChannel): Unit = runOnUiThread {
    val multisig: ScriptEltSeq = multiSig2of2(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
    val scriptPubKey = Script.write(Script pay2wsh multisig)

    val processor = new TxProcessor {
      val funding = Coin valueOf cmd.fundingAmountSat
      val pay = P2WSHData(funding, scriptPubKey)

      def onTxFail(exc: Throwable) =
        mkForm(mkChoiceDialog(me delayUI askForFeerate(chan, cmd, accept),
          none, dialog_ok, dialog_cancel), null, errorWhenMakingTx apply exc)

      def processTx(password: String, fee: Coin) = Future {
        val tx: fr.acinq.bitcoin.Transaction = makeTx(password, fee)
        val outIndex = Scripts.findPubKeyScriptIndex(tx, scriptPubKey)
        chan process Tuple2(tx, outIndex)
      }
    }

    // Pick a fee
    processor.chooseFee
  }
}