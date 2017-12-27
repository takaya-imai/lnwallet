package com.lightning.wallet

import spray.json._
import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.R.string._
import com.lightning.wallet.ln.Channel._
import com.lightning.wallet.Denomination._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.lnutils.ImplicitConversions._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import android.widget.{BaseAdapter, Button, ListView, TextView}
import com.lightning.wallet.lnutils.{CloudAct, PaymentInfoWrap}
import com.lightning.wallet.ln.Tools.{none, random, wrap}
import com.lightning.wallet.helper.{AES, ThrottledWork}
import fr.acinq.bitcoin.{MilliSatoshi, Script}
import android.view.{Menu, View, ViewGroup}
import scala.util.{Failure, Success}

import android.content.DialogInterface.BUTTON_POSITIVE
import com.lightning.wallet.ln.Scripts.multiSig2of2
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.script.ScriptBuilder
import scala.collection.mutable
import org.bitcoinj.core.Coin
import android.os.Bundle


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartDetailsText = findViewById(R.id.lnStartDetailsText).asInstanceOf[TextView]
  lazy val lnCancel = findViewById(R.id.lnCancel).asInstanceOf[Button]

  lazy val chansNumber = getResources getStringArray R.array.ln_ops_start_node_channels
  lazy val views = lnStartNodesList :: findViewById(R.id.lnStartDetails) :: Nil
  lazy val nodeView = getString(ln_ops_start_node_view)
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
  def notifySubTitle(subtitle: String, infoType: Int) = none

  // Adapter for nodes tx list
  class NodesAdapter extends BaseAdapter {
    def getView(nodePosition: Int, cv: View, parent: ViewGroup) = {
      val view = getLayoutInflater.inflate(R.layout.frag_single_line, null)
      val textLine = view.findViewById(R.id.textLine).asInstanceOf[TextView]

      val (announce, connections) = adapter getItem nodePosition
      val humanConnects = app.plurOrZero(chansNumber, connections)
      textLine setText nodeView.format(announce.alias, humanConnects,
        humanNode(announce.nodeId, "\u00A0"), new String).html

      view
    }

    var nodes = Vector.empty[AnnounceChansNum]
    def getItem(position: Int) = nodes(position)
    def getItemId(position: Int) = position
    def getCount = nodes.size
  }

  override def onCreate(savedState: Bundle) = {
    // Initialize this activity, method is run once
    // Set action bar, main view content, title text

    super.onCreate(savedState)
    wrap(me setSupportActionBar toolbar)(me setContentView R.layout.activity_ln_start)
    add(text = me getString ln_select_peer, tag = Informer.LNSTATE).flash.run
    animateTitle(me getString ln_ops_start)

    // Wire up list and load peers with empty query string
    lnStartNodesList setOnItemClickListener onTap(onPeerSelected)
    lnStartNodesList setAdapter adapter
    react(new String)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }

  private def onPeerSelected(pos: Int) = hideKeys {
    val (announce, connections) = adapter getItem pos
    // This channel does not receive events just yet so we need to add some custom listeners
    val freshChan = app.ChannelManager.createChannel(mutable.Set.empty, InitData apply announce)

    val socketOpenListener = new ConnectionListener {
      override def onMessage(message: LightningMessage) = freshChan process message
      override def onDisconnect(nodeId: PublicKey) = if (nodeId == announce.nodeId) freshChan process CMDShutdown
      override def onTerminalError(nodeId: PublicKey) = if (nodeId == announce.nodeId) freshChan process CMDShutdown
      override def onOperational(nodeId: PublicKey, their: Init) = if (nodeId == announce.nodeId)
        // Peer is reachable so now we ask user to provide a funding
        me runOnUiThread askForFunding(freshChan, their)
    }

    lazy val chanOpenListener = new ChannelListener {
      // Updates UI accordingly to changes in fresh channel
      // should account for user cancelling at late stages

      override def onBecome = {
        case (_, WaitFundingData(_, cmd, accept), WAIT_FOR_ACCEPT, WAIT_FOR_FUNDING) =>
          // Peer has agreed to open a channel so now we ask user for a tx feerate
          me runOnUiThread askForFeerate(freshChan, cmd, accept)

        case (_, _, _, CLOSING) =>
          // Something went wrong, back off
          // like disconnect or remote error
          me runOnUiThread cancelChannel

        case (_, wait: WaitFundingDoneData, WAIT_FUNDING_SIGNED, WAIT_FUNDING_DONE) =>
          // First we remove local listeners, then we try to save a channel to database
          ConnectionManager.listeners -= socketOpenListener
          freshChan.listeners -= this

          freshChan STORE wait
          // Error while saving will halt any further progress here
          // User may press cancel at this point but it won't affect anything
          val state = RefundingData(wait.announce, wait.commitments, wait.fundingTx).toJson.toString
          // Attempt to save a channel backup right away, in worst case it will be saved once channel becomes NORMAL if there are no tokens
          LNParams.cloud doProcess CloudAct(AES.encode(state, LNParams.cloudSecret), Seq("key" -> LNParams.cloudId.toString), "data/put")
          // Make this a fully established channel by attaching operational listeners and adding it to list
          freshChan.listeners ++= app.ChannelManager.operationalListeners
          app.ChannelManager.all +:= freshChan
          me exitTo classOf[LNOpsActivity]
      }
    }

    def cancelChannel: Unit = {
      freshChan.listeners -= chanOpenListener
      ConnectionManager.listeners -= socketOpenListener
      // Just disconnect this channel from all listeners
      whenBackPressed = anyToRunnable(super.onBackPressed)
      setVis(View.VISIBLE, View.GONE)
      app toast ln_ops_start_abort
      getSupportActionBar.show
    }

    val humanConnects = app.plurOrZero(chansNumber, connections)
    val detailsText = nodeView.format(announce.alias, humanConnects,
      "<br>" + humanNode(announce.nodeId, "\n"), new String).html

    ConnectionManager requestConnection announce
    ConnectionManager.listeners += socketOpenListener
    // We need PaymentInfoWrap here to process inner channel errors
    freshChan.listeners ++= Set(chanOpenListener, PaymentInfoWrap)
    lnCancel setOnClickListener onButtonTap(cancelChannel)
    whenBackPressed = anyToRunnable(cancelChannel)
    lnStartDetailsText setText detailsText
    setVis(View.GONE, View.VISIBLE)
    getSupportActionBar.hide
  }

  // UI utilities

  def askForFunding(chan: Channel, their: Init) = {
    val minUserCapacity = MilliSatoshi(LNParams.broadcaster.ratePerKwSat * sat2msatFactor)
    val content = getLayoutInflater.inflate(R.layout.frag_input_fiat_converter, null, false)
    val alert = mkForm(negPosBld(dialog_cancel, dialog_next), getString(ln_ops_start_fund_title).html, content)
    val rateManager = new RateManager(getString(amount_hint_newchan).format(denom withSign minUserCapacity,
      denom withSign LNParams.maxChannelCapacity, denom withSign app.kit.currentBalance), content)

    def askAttempt = rateManager.result match {
      case Failure(_) => app toast dialog_sum_empty
      case Success(ms) if ms < minUserCapacity => app toast dialog_sum_small
      case Success(ms) if ms > LNParams.maxChannelCapacity => app toast dialog_sum_big

      case Success(ms) => rm(alert) {
        val amountSat = ms.amount / sat2msatFactor
        val theirUnspendableReserveSat = (amountSat * LNParams.theirReserveToFundingRatio).toLong
        val finalPubKeyScript = ScriptBuilder.createOutputScript(app.kit.currentAddress).getProgram
        val localParams = LNParams.makeLocalParams(theirUnspendableReserveSat, finalPubKeyScript, System.currentTimeMillis)
        chan process CMDOpenChannel(localParams, random getBytes 32, LNParams.broadcaster.ratePerKwSat, 0, their, amountSat)
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

      def processTx(pass: String, feePerKb: Coin) =
        <(makeTx(pass, feePerKb): fr.acinq.bitcoin.Transaction, onTxFail) { fundTx =>
          // Convert bitcoinj tx to bitcoin-lib format and find our output order number
          val outIndex = Scripts.findPubKeyScriptIndex(fundTx, scriptPubKey)
          chan.process(fundTx -> outIndex)
        }

      def onTxFail(err: Throwable) =
        mkForm(mkChoiceDialog(me delayUI askForFeerate(chan, cmd, accept),
          none, dialog_ok, dialog_cancel), null, messageWhenMakingTx apply err)
    }
  }
}