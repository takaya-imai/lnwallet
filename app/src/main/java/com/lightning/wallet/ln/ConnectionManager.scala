package com.lightning.wallet.ln

import scala.concurrent.duration._
import com.lightning.wallet.ln.wire._
import com.lightning.wallet.ln.Features._

import rx.lang.scala.{Observable => Obs}
import java.net.{InetSocketAddress, Socket}
import com.lightning.wallet.ln.Tools.{Bytes, none}

import scala.concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.ln.LNParams.nodePrivateKey
import com.lightning.wallet.ln.crypto.Noise.KeyPair
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.BinaryData
import scala.collection.mutable
import scala.concurrent.Future


object ConnectionManager {
  val pair = KeyPair(nodePrivateKey.publicKey, nodePrivateKey.toBin)
  val ourInit = Init(LNParams.globalFeatures, LNParams.localFeatures)
  val connections = mutable.Map.empty[PublicKey, Worker]
  val listeners = mutable.Set.empty[ConnectionListener]

  protected[this] val events = new ConnectionListener {
    override def onDisconnect(id: PublicKey) = for (lst <- listeners) lst.onDisconnect(id)
    override def onTerminalError(id: PublicKey) = for (lst <- listeners) lst.onTerminalError(id)
    override def onMessage(message: LightningMessage) = for (lst <- listeners) lst.onMessage(message)
    override def onOperational(id: PublicKey, their: Init) = for (lst <- listeners) lst.onOperational(id, their)
  }

  def requestConnection(ann: NodeAnnouncement) = connections get ann.nodeId match {
    case Some(w) if !w.work.isCompleted && w.savedInit != null => events.onOperational(ann.nodeId, w.savedInit)
    case _ => connections(ann.nodeId) = new Worker(nodeId = ann.nodeId, location = ann.addresses.head)
  }

  class Worker(nodeId: PublicKey, location: InetSocketAddress) {
    val handler: TransportHandler = new TransportHandler(pair, nodeId) {
      def handleDecryptedIncomingData(data: BinaryData) = intercept(LightningMessageCodecs deserialize data)
      def handleEncryptedOutgoingData(data: BinaryData) = try socket.getOutputStream write data catch none
      def handleError = { case _ => events onTerminalError nodeId }
      def handleEnterOperationalState = process(ourInit)
    }

    var savedInit: Init = _
    val socket: Socket = new Socket
    var lastPing = System.currentTimeMillis

    val work = Future {
      val buffer = new Bytes(1024)
      socket.connect(location, 7500)
      handler.init

      while (true) {
        val length = socket.getInputStream.read(buffer, 0, 1024)
        if (length < 0) throw new RuntimeException("Connection droppped")
        else handler process BinaryData(buffer take length)
      }
    }

    work onComplete { _ =>
      events onDisconnect nodeId
    }

    def intercept(message: LightningMessage) = message match {
      // Some messages need a special handling so we intercept them
      case their: Init if areSupported(their.localFeatures) =>
        events.onOperational(nodeId, their)
        savedInit = their

      case _: Init =>
        // Incompatible features
        events onTerminalError nodeId
        connections -= nodeId

      case ping: Ping if ping.pongLength > 0 =>
        handler process Pong("00" * ping.pongLength)
        lastPing = System.currentTimeMillis

      case theirMessage =>
        // Forward to all channels
        events onMessage theirMessage
    }
  }

  Obs.interval(150.seconds) foreach { _ =>
    val outdated = System.currentTimeMillis - 1000 * 150
    for (w <- connections.values if w.lastPing < outdated)
      try w.socket.close catch none
  }
}

class ConnectionListener {
  def onDisconnect(id: PublicKey): Unit = none
  def onTerminalError(id: PublicKey): Unit = none
  def onOperational(id: PublicKey, their: Init): Unit = none
  def onMessage(msg: LightningMessage): Unit = none
}