package com.lightning.wallet

import Utils._
import R.string._
import org.bitcoinj.core._
import spray.json._
import com.lightning.wallet.lncloud.ImplicitJsonFormats._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import com.lightning.wallet.lncloud.{RatesSaver, Saver}
import org.bitcoinj.wallet.Wallet.BalanceType
import org.bitcoinj.crypto.KeyCrypterScrypt
import com.google.common.net.InetAddresses
import com.lightning.wallet.ln.Tools._
import com.google.protobuf.ByteString
import org.bitcoinj.wallet.Protos
import android.app.Application
import android.widget.Toast
import java.io.File
import java.util.concurrent.TimeUnit.MILLISECONDS

import Context.CLIPBOARD_SERVICE
import com.lightning.wallet.ln.wire.{Init, LightningMessage}
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.ln._
import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.core.listeners.NewBestBlockListener
import org.bitcoinj.net.discovery.DnsDiscovery


class WalletApp extends Application { me =>
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val params = org.bitcoinj.params.RegTestParams.get
  var walletFile, chainFile: java.io.File = _
  var kit: WalletKit = _

  lazy val plur = getString(lang) match {
    case "eng" | "esp" => (opts: Array[String], num: Long) => if (num == 1) opts(1) else opts(2)
    case "chn" | "jpn" => (phraseOptions: Array[String], num: Long) => phraseOptions(1)
    case "rus" | "ukr" => (phraseOptions: Array[String], num: Long) =>

      val reminder100 = num % 100
      val reminder10 = reminder100 % 10
      if (reminder100 > 10 & reminder100 < 20) phraseOptions(3)
      else if (reminder10 > 1 & reminder10 < 5) phraseOptions(2)
      else if (reminder10 == 1) phraseOptions(1)
      else phraseOptions(3)
  }

  // Various utilities
  def getTo(base58: String) = Address.fromBase58(params, base58)
  def toast(message: Int) = Toast.makeText(me, message, Toast.LENGTH_LONG).show
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => true case _ => false }
  def plurOrZero(opts: Array[String], number: Long) = if (number > 0) plur(opts, number) format number else opts(0)
  def clipboardManager = getSystemService(CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBuffer = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString

  def setBuffer(text: String) = wrap(me toast copied_to_clipboard) {
    clipboardManager setPrimaryClip ClipData.newPlainText(appName, text)
  }

  // Startup actions
  override def onCreate = wrap(super.onCreate) {
    chainFile = new File(getFilesDir, s"$appName.spvchain")
    walletFile = new File(getFilesDir, s"$appName.wallet")
    startupAppReference = me
  }

  object TransData {
    var value: Any = _
    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case _: WrongNetworkException => err(err_different_net)
      case _: AddressFormatException => err(err_address)
      case _: BitcoinURIParseException => err(err_uri)
      case _: ArithmeticException => err(err_neg)
      case _: Throwable => err(err_general)
    }

    def recordValue(rawText: String) = value = rawText match {
      case raw if raw startsWith "bitcoin" => new BitcoinURI(params, raw)
      case raw if raw startsWith "lnbc" => PaymentRequest read raw
      case raw if raw startsWith "lntb" => PaymentRequest read raw
      case raw => getTo(raw)
    }
  }

  object ChannelManager extends Saver {
    type ChannelDataVec = Vector[ChannelData]
    var all = tryGet map to[ChannelDataVec] getOrElse Vector.empty map createChannel
    def from(of: Vector[Channel], id: PublicKey) = of.filter(_.data.announce.nodeId == id)
    def alive: Vector[Channel] = all.filterNot(_.state == Channel.CLOSING)
    def saveChannels = save(all.map(_.data).toJson)

    val KEY = "channels"
    val chainListener = new TxTracker with NewBestBlockListener {
      override def txConfirmed(tx: Transaction) = for (chan <- alive) chan process CMDConfirmed(tx)
      override def notifyNewBestBlock(block: StoredBlock) = for (chan <- alive) chan process CMDHeight(block.getHeight)
      override def coinsSent(tx: Transaction) = for (chan <- all) chan process CMDSpent(tx, funding = false)
    }

    val socketListener = new ConnectionListener {
      override def onTerminalError(id: PublicKey) = from(alive, id).foreach(_ process CMDShutdown)
      override def onOperational(id: PublicKey, their: Init) = from(alive, id).foreach(_ process CMDOnline)
      override def onDisconnect(id: PublicKey) = from(alive, id).foreach(_ process CMDOffline)
      override def onMessage(msg: LightningMessage) = alive.foreach(_ process msg)
    }

    val reconnectListener = new ConnectionListener {
      override def onDisconnect(id: PublicKey) = from(alive, id).headOption
        .foreach(ConnectionManager requestConnection _.data.announce)
    }

    def createChannel(saved: ChannelData): Channel = new Channel {
      private def getSocket = ConnectionManager.connections get data.announce.nodeId
      def send(msg: LightningMessage) = for (socket <- getSocket) socket send msg
      listeners += LNParams.broadcaster
      process(saved)
    }
  }

  abstract class WalletKit extends AbstractKit {
    override def shutDown = if (peerGroup.isRunning) peerGroup.stop
    def currentBalance = wallet getBalance BalanceType.ESTIMATED_SPENDABLE
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def currentHeight = blockChain.getBestChainHeight

    def blockingSend(tx: Transaction) =
      // Wait for at least one peer confirmation or failure
      peerGroup.broadcastTransaction(tx, 1).broadcast.get

    def encryptWallet(pass: CharSequence) = {
      val randSalt = ByteString copyFrom KeyCrypterScrypt.randomSalt
      val scryptBuilder = Protos.ScryptParameters.newBuilder setSalt randSalt
      val crypter = new KeyCrypterScrypt(scryptBuilder.setN(65536).build)
      wallet.encrypt(crypter, crypter deriveKey pass)
    }

    def useCheckPoints(time: Long) = {
//      val pts = getAssets open "checkpoints-testnet.txt"
//      CheckpointManager.checkpoint(params, pts, store, time)
    }

    def setupAndStartDownload = {
      wallet setAcceptRiskyTransactions true
      wallet addCoinsSentEventListener Vibr.generalTracker
      wallet addCoinsReceivedEventListener Vibr.generalTracker
      wallet addTransactionConfidenceEventListener Vibr.generalTracker
      wallet.autosaveToFile(walletFile, 500, MILLISECONDS, null)
//      peerGroup addPeerDiscovery new DnsDiscovery(params)

      val pa1 = new PeerAddress(params, InetAddresses.forString("10.0.2.2"), 8333)
      peerGroup.addAddress(pa1)

      peerGroup.setUserAgent(appName, "0.01")
      peerGroup setDownloadTxDependencies 0
      peerGroup setPingIntervalMsec 10000
      peerGroup setMaxConnections 6
      peerGroup addWallet wallet
      RatesSaver.process
      startDownload
    }
  }
}

object Vibr {
  type Pattern = Array[Long]
  def vibrate(pattern: Pattern) = if (null != vib && vib.hasVibrator) vib.vibrate(pattern, -1)
  lazy val vib = app.getSystemService(Context.VIBRATOR_SERVICE).asInstanceOf[android.os.Vibrator]
  val confirmed = Array(0L, 75, 250, 75, 250)
  val processed = Array(0L, 85, 200)

  val generalTracker = new TxTracker {
    override def txConfirmed(tx: Transaction) = vibrate(confirmed)
    override def coinsReceived(tx: Transaction) = vibrate(processed)
    override def coinsSent(tx: Transaction) = vibrate(processed)
  }
}