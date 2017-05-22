package com.lightning.wallet

import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.R.string._
import android.view.{Menu, View, ViewGroup}
import android.widget.{BaseAdapter, Button, ListView, TextView}
import com.lightning.wallet.helper.{SocketListener, ThrottledWork}
import com.lightning.wallet.ln._
import com.lightning.wallet.lncloud.{FailoverLNCloud, LNCloudPrivateSaver}
import com.lightning.wallet.ln.wire.LightningMessageCodecs.AnnounceChansNum
import com.lightning.wallet.lncloud.ImplicitConversions.string2Ops
import com.lightning.wallet.ln.wire.{Init, NodeAnnouncement}
import com.lightning.wallet.Utils.humanPubkey
import android.support.v4.view.MenuItemCompat
import com.lightning.wallet.Utils.app
import android.os.Bundle


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
    def process(res: AnnounceChansNumVec) = wrap(me runOnUiThread setListView)(adapter.nodes = res)
  }

  def react(query: String) = worker onNewQuery query
  def notifySubTitle(subtitle: String, infoType: Int) = {
    add(subtitle, infoType).timer.schedule(me del infoType, 25000)
    me runOnUiThread ui
  }

  def mkNodeView(info: AnnounceChansNum) = {
    val (announce: NodeAnnouncement, quantity) = info
    val humanId = humanPubkey(announce.nodeId.toString)
    val humanChansNumber = app.plurOrZero(chansNumber, quantity)
    nodeView.format(announce.alias, humanChansNumber, humanId).html
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
    add(me getString ln_notify_working, Informer.LNSTATE).ui.run
    getSupportActionBar setTitle ln_ops_start
    lnStartNodesList setAdapter adapter
    worker onNewQuery new String
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }

  private def setListView: Unit = {
    update(me getString ln_select_peer, Informer.LNSTATE).ui.run
    setVis(View.VISIBLE, View.GONE, View.INVISIBLE)
    adapter.notifyDataSetChanged
  }

  private def setPeerView(position: Int): Unit = {
    lnStartInfo setText mkNodeView(adapter getItem position)
    update(me getString ln_notify_working, Informer.LNSTATE).ui.run
    setVis(View.GONE, View.VISIBLE, View.VISIBLE)
    MenuItemCompat collapseActionView searchItem
  }

  private def onPeerSelected(position: Int): Unit = hideKeys {
    val (announce: NodeAnnouncement, _) = adapter getItem position

    val kit: ChannelKit =
      ChannelKit apply new Channel {
        data = InitData apply announce
        state = Channel.WAIT_FOR_INIT
      }

    val initSockListener = new SocketListener {
      override def onDisconnect = me runOnUiThread restore
      def restore = wrap(setListView)(app toast ln_ops_start_cancel)
      lnStartCancel setOnClickListener onButtonTap(kit.chan process CMDShutdown)
    }

    val initChanListener = new StateMachineListener {
      override def onBecome = { case (prev, _, _, Channel.FINISHED) =>
        Tools log s"Channel opening has been interrupted at data: $prev"
        kit.socket.shutdown
      }

      override def onPostProcess = { case _: Init =>
        // This is where I should ask user for funding
      }

      override def onError = { case err =>
        Tools log s"Channel malfunction: $err"
        kit.chan process CMDShutdown
      }
    }

    kit.socket.listeners += initSockListener
    kit.chan.listeners += initChanListener
    app.TransData.value = kit
    me setPeerView position
    kit.socket.start
  }
}
