package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import scala.concurrent.duration._
import com.lightning.wallet.Utils._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.lncloud.ChannelSaver._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.lightning.wallet.helper.{SocketListener, SocketWrap}
import org.bitcoinj.core.{Coin, StoredBlock, Transaction}
import rx.lang.scala.{Observable => Obs}

import org.bitcoinj.core.listeners.NewBestBlockListener
import com.lightning.wallet.ln.crypto.Noise.KeyPair
import com.lightning.wallet.TxTracker
import fr.acinq.bitcoin.BinaryData


trait ChannelKit {
  val closing: ChannelSet
  val blockchainListener = new TxTracker with NewBestBlockListener {
    override def coinsSent(tx: Transaction, pb: Coin, nb: Coin) = for (chan <- allChannels) chan process CMDSomethingSpent(tx)
    override def notifyNewBestBlock(block: StoredBlock) = for (chan <- allChannels) chan process CMDDepth(block.getHeight)
    override def txConfirmed(tx: Transaction) = for (chan <- allChannels) chan process CMDSomethingConfirmed(tx)
  }

  app.kit.wallet addCoinsSentEventListener blockchainListener
  app.kit.blockChain addNewBestBlockListener blockchainListener
  app.kit.wallet addTransactionConfidenceEventListener blockchainListener
  for (channel <- allChannels) channel.listeners += LNParams.broadcaster
  def allChannels: ChannelSet = closing
}

case class InactiveKit(closing: ChannelSet) extends ChannelKit
case class ActiveKit(active: Channel, closing: ChannelSet) extends ChannelKit { me =>
  val keyPair = KeyPair(LNParams.nodePrivateKey.publicKey, LNParams.nodePrivateKey.toBin)
  val address = active.data.announce.addresses.head

  lazy val socket = new SocketWrap(address.getAddress, address.getPort) {
    def onReceive(dataChunk: BinaryData): Unit = handler process dataChunk
  }

  val handler: TransportHandler = new TransportHandler(keyPair, active.data.announce.nodeId, socket) {
    def feedForward(msg: BinaryData): Unit = interceptIncomingMsg(LightningMessageCodecs deserialize msg)
  }

  val reconnectSockListener = new SocketListener {
    override def onDisconnect = Obs.just(Tools log "Reconnecting a socket")
      .delay(10.seconds).subscribe(_ => socket.start, _.printStackTrace)
  }

  socket.listeners += new SocketListener {
    override def onConnect: Unit = handler.init
    override def onDisconnect = Tools log "Sock off"
  }

  handler.listeners += new StateMachineListener {
    override def onBecome: PartialFunction[Transition, Unit] = {
      case (_, _, TransportHandler.HANDSHAKE, TransportHandler.WAITING_CYPHERTEXT) =>
        Tools log s"Handle handshake phase completed, now sending Init message"
        me send Init(LNParams.globalFeatures, LNParams.localFeatures)
    }

    override def onError = {
      case transportRelated: Throwable =>
        Tools log s"Transport $transportRelated"
        active process CMDShutdown
    }
  }

  active.listeners += new StateMachineListener { self =>
    override def onBecome: PartialFunction[Transition, Unit] = {
      case (previousData, data, previousState, Channel.CLOSING) =>
        // "00" * 32 is a connection level error which will result in socket closing
        Tools log s"Closing channel from $previousState at $previousData : $data"
        me send Error("00" * 32, "Kiss all channels goodbye" getBytes "UTF-8")
        socket.listeners -= reconnectSockListener
        active.listeners -= self

      case (previousData, data, previousState, state) =>
        val messages = Helpers.extractOutgoingMessages(previousData, data)
        Tools log s"Sending $previousState -> $state messages: $messages"
        messages foreach send
    }

    override def onPostProcess = {
      case Error(_, reason: BinaryData) =>
        val decoded = new String(reason.toArray)
        Tools log s"Got remote Error: $decoded"
    }
  }

  override def allChannels: ChannelSet = closing + active
  private def interceptIncomingMsg(msg: LightningMessage) = msg match {
    case Ping(responseLength, _) => if (responseLength > 0) me send Pong("00" * responseLength)
    case Init(_, local) if !Features.areSupported(local) => active process CMDShutdown
    case _ => active process msg
  }

  def send(msg: LightningMessage) = {
    val encoded = LightningMessageCodecs serialize msg
    handler process Tuple2(TransportHandler.Send, encoded)
  }
}
