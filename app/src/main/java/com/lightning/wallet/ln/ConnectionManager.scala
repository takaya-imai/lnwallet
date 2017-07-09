package com.lightning.wallet.ln

import com.lightning.wallet.ln.wire._
import java.net.{InetSocketAddress, Socket}
import com.lightning.wallet.ln.Tools.{Bytes, none}
import scala.concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.ln.LNParams.nodePrivateKey
import com.lightning.wallet.ln.crypto.Noise.KeyPair
import fr.acinq.bitcoin.BinaryData
import scala.collection.mutable
import scala.concurrent.Future


object ConnectionManager {
  val connections = mutable.Map.empty[BinaryData, Worker]
  val listeners = mutable.Set.empty[ConnectionListener]

  protected[this] val events = new ConnectionListener {
    override def onDisconnect(id: BinaryData) = for (lst <- listeners) lst onDisconnect id
    override def onTerminalError(id: BinaryData) = for (lst <- listeners) lst onTerminalError id
    override def onOperational(id: BinaryData, their: Init) = for (lst <- listeners) lst.onOperational(id, their)
    override def onMessage(lnMessage: LightningMessage) = for (lst <- listeners) lst onMessage lnMessage
  }

  def requestConnection(announce: NodeAnnouncement) = connections get announce.nodeId match {
    case Some(work) if !work.process.isCompleted & work.savedInit == null => Tools log "Awaiting for their Init"
    case Some(work) if !work.process.isCompleted => events.onOperational(announce.nodeId, work.savedInit)
    case _ => connections(announce.nodeId) = new Worker(announce.nodeId, announce.addresses.head)
  }

  class Worker(nodeId: BinaryData, location: InetSocketAddress) { me =>
    val pair: KeyPair = KeyPair(nodePrivateKey.publicKey, nodePrivateKey.toBin)
    val handler: TransportHandler = new TransportHandler(pair, remotePubKey = nodeId) {
      def handleDecryptedIncomingData(data: BinaryData): Unit = intercept(LightningMessageCodecs deserialize data)
      def handleEncryptedOutgoingData(data: BinaryData): Unit = try socket.getOutputStream write data catch none
      def handleEnterOperationalState = me send Init(LNParams.globalFeatures, LNParams.localFeatures)
      def handleError(err: Throwable) = events onTerminalError nodeId
    }

    var savedInit: Init = _
    val BUFFER_SIZE: Int = 1024
    val socket: Socket = new Socket

    val process = Future {
      val buffer = new Bytes(BUFFER_SIZE)
      socket.connect(location, 7500)
      handler.init

      while (true) {
        val length = socket.getInputStream.read(buffer, 0, BUFFER_SIZE)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler.process(buffer take length)
      }
    }

    def send(message: LightningMessage) = {
      val raw = LightningMessageCodecs serialize message
      handler.process(TransportHandler.Send -> raw)
    }

    def intercept(message: LightningMessage) = message match {
      case Ping(length, _) if length > 0 => me send Pong("00" * length)

      case their: Init if null == savedInit =>
        // Need to remember it for later calls
        events.onOperational(nodeId, their)
        savedInit = their

      case error: Error =>
        events onMessage error
        val decoded = new String(error.data.toArray)
        Tools log s"Got remote Error: $decoded"

      case _ =>
        // Send to channels
        events onMessage message
    }

    process onComplete { _ =>
      events onDisconnect nodeId
      savedInit = null
    }
  }
}

class ConnectionListener {
  def onDisconnect(id: BinaryData): Unit = none
  def onTerminalError(id: BinaryData): Unit = none
  def onOperational(id: BinaryData, their: Init): Unit = none
  def onMessage(msg: LightningMessage): Unit = none
}