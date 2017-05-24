package com.lightning.wallet

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.MSat._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.R.string._

import scala.util.{Failure, Success}
import android.view.{Menu, View, ViewGroup}
import com.lightning.wallet.Utils.{app, sumIn}
import com.lightning.wallet.ln.Tools.{none, random, wrap}
import com.lightning.wallet.ln.wire.{AcceptChannel, Init, NodeAnnouncement}
import android.widget.{BaseAdapter, Button, ListView, TextView}
import com.lightning.wallet.helper.{SocketListener, ThrottledWork}
import com.lightning.wallet.lncloud.{FailoverLNCloud, LNCloudPrivateSaver}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.ln.Scripts.multiSig2of2
import concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.Utils.humanPubkey
import android.support.v4.view.MenuItemCompat

import scala.concurrent.Future
import android.os.Bundle
import android.content.DialogInterface.BUTTON_POSITIVE
import fr.acinq.bitcoin.Script
import org.bitcoinj.core.Coin
import org.bitcoinj.core.Transaction.MIN_NONDUST_OUTPUT
import org.bitcoinj.script.ScriptBuilder


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartCancel = findViewById(R.id.lnStartCancel).asInstanceOf[Button]
  lazy val lnStartInfo = findViewById(R.id.lnStartInfo).asInstanceOf[TextView]

  lazy val views = lnStartNodesList :: findViewById(R.id.lnStartDetails) :: lnStartCancel :: Nil
  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val nodeView = getString(ln_ops_start_node_view)
  private[this] val adapter = new NodesAdapter

  private[this] val privateCloudTry =
    for (data <- LNCloudPrivateSaver.tryGetObject)
      yield new FailoverLNCloud(LNParams.lnCloud, data.url)

  type AnnounceChansNumVec = Vector[AnnounceChansNum]
  private[this] val worker = new ThrottledWork[AnnounceChansNumVec] {
    def work(searchInput: String) = privateCloudTry getOrElse LNParams.lnCloud findNodes searchInput
    def process(res: AnnounceChansNumVec) = wrap(me runOnUiThread adapter.notifyDataSetChanged)(adapter.nodes = res)
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
    super.onCreate(savedState)
    wrap(initToolbar)(me setContentView R.layout.activity_ln_start)
    lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
    add(me getString ln_select_peer, Informer.LNSTATE).ui.run
    getSupportActionBar setTitle ln_ops_start
    lnStartNodesList setAdapter adapter
    react(new String)
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

    val initSockListener = new SocketListener {
      def restoreList = wrap(setListView)(app toast ln_ops_start_cancel)
      override def onDisconnect = me runOnUiThread restoreList
    }

    val initChanListener = new StateMachineListener {
      def completeFunding(cmd: CMDOpenChannel, accept: AcceptChannel): Unit = new TxProcessor {
        val scriptPubKey = Script write multiSig2of2(cmd.localParams.fundingPrivKey.publicKey, accept.fundingPubkey)
        val pay = P2WSHData(Coin valueOf cmd.fundingAmountSat, scriptPubKey)
        me runOnUiThread chooseFee

        def processTx(password: String, fee: Coin) = Future {
          val tx: fr.acinq.bitcoin.Transaction = makeTx(password, fee)
          val outIndex = Scripts.findPubKeyScriptIndex(tx, scriptPubKey)
          kit.chan process Tuple2(tx, outIndex)
        }

        def onTxFail(exc: Throwable) =
          mkForm(mkChoiceDialog(me delayUI completeFunding(cmd, accept),
            none, dialog_ok, dialog_cancel), null, errorWhenMakingTx apply exc)
      }

      override def onBecome: PartialFunction[Transition, Unit] = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          completeFunding(cmd, accept)
      }

      override def onPostProcess = {
        case theirInitMessage: Init =>
          askForFunding(next = openChannel)
          def openChannel(amountSat: Long): Unit = Future {
            val chanReserveSat = (amountSat * LNParams.reserveToFundingRatio).toLong
            val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
            val localParams = LNParams.makeLocalParams(chanReserveSat, finalPubKeyScript, System.currentTimeMillis)
            kit.chan process CMDOpenChannel(localParams, random getBytes 32, 10000L, 0L, theirInitMessage, amountSat)
          }
      }

      override def onError = {
        case channelRelated: Throwable =>
          Tools log s"Channel $channelRelated"
          kit.chan process CMDShutdown
      }
    }

    lnStartCancel setOnClickListener {
      onButtonTap(kit.chan process CMDShutdown)
    }

    kit.socket.listeners += initSockListener
    kit.chan.listeners += initChanListener
    app.TransData.value = kit
    me setPeerView position
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
    update(me getString ln_select_peer, Informer.LNSTATE).ui.run
    setVis(View.VISIBLE, View.GONE, View.INVISIBLE)
  }

  private def setPeerView(position: Int) = {
    MenuItemCompat collapseActionView searchItem
    lnStartInfo setText mkNodeView(adapter getItem position)
    update(me getString ln_notify_working, Informer.LNSTATE).ui.run
    setVis(View.GONE, View.VISIBLE, View.VISIBLE)
  }

  private def askForFunding(next: Long => Unit) = {
    val humanBalance = sumIn format withSign(app.kit.currentBalance)
    val humanCap = sumIn format withSign(LNParams.maxChannelCapacity)
    val titleTop = getString(ln_ops_start_fund_title).format(humanBalance, humanCap).html
    val content = getLayoutInflater.inflate(R.layout.frag_input_send_noaddress, null, false)
    val builder = negPosBld(dialog_cancel, dialog_next)
    me runOnUiThread showForm

    def showForm = {
      val alert = mkForm(builder, titleTop, content)
      val rateManager = new RateManager(content)

      def attempt = rateManager.result match {
        case Failure(_) => app toast dialog_sum_empty
        case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_capacity
        case Success(ms) if MIN_NONDUST_OUTPUT isGreaterThan ms => app toast dialog_sum_dusty
        case Success(ms) => rm(alert)(next apply ms.amount / satFactor)
      }

      val ok = alert getButton BUTTON_POSITIVE
      ok setOnClickListener onButtonTap(attempt)
    }
  }
}
