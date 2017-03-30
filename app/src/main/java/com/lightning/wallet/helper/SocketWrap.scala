package com.lightning.wallet.helper

import com.lightning.wallet.ln.Tools.{Bytes, none}
import java.net.{InetAddress, InetSocketAddress, Socket}
import concurrent.ExecutionContext.Implicits.global
import com.lightning.wallet.ln.DataTransport
import com.lightning.wallet.Utils.nullFail
import fr.acinq.bitcoin.BinaryData
import scala.concurrent.Future
import scala.util.Try


class SocketWrap(ip: InetAddress, port: Int) extends DataTransport {
  def send(data: BinaryData) = wrapOpt.foreach(_.socket.getOutputStream write data)
  def shutdown = wrapOpt.foreach(_.socket.close)
  def restart = wrapOpt = Try(new Worker)

  private var wrapOpt: Try[Worker] = nullFail
  var reactors = Set.empty[SocketReactor]

  private val proxy = new SocketReactor {
    override def onConnect = for (react <- reactors) react.onConnect
    override def onData(chunk: Bytes) = for (react <- reactors) react onData chunk
    override def onDisconnect = { case any => for (react <- reactors) react onDisconnect any }
    override def onError = { case error => for (react <- reactors) react onError error }
  }

  class Worker {
    val socket = new Socket
    private val BUFFER_SIZE = 1024
    private val buffer = new Bytes(BUFFER_SIZE)

    Future {
      socket.connect(new InetSocketAddress(ip, port), 10000)
      try proxy.onConnect catch proxy.onError

      while (true) {
        val read = socket.getInputStream.read(buffer, 0, BUFFER_SIZE)
        if (read > 0) try proxy.onData(buffer take read) catch proxy.onError
        else if (read < 0) throw new RuntimeException
      }
    } onComplete { result =>
      try socket.close catch none
      proxy onDisconnect result
    }
  }
}

class SocketReactor {
  def onConnect: Unit = none
  def onData(chunk: Bytes): Unit = none
  def onDisconnect: PartialFunction[Any, Unit] = none
  def onError: PartialFunction[Throwable, Unit] = none
}