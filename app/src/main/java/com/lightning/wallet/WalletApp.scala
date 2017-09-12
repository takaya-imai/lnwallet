package com.lightning.wallet

import Utils._
import R.string._
import org.bitcoinj.core.{Address, Block, FilteredBlock, Peer, PeerAddress, Transaction}

import scala.concurrent.duration._
import com.lightning.wallet.lncloud.ImplicitConversions._
import com.google.common.util.concurrent.Service.State.{RUNNING, STARTING}
import org.bitcoinj.uri.{BitcoinURI, BitcoinURIParseException}
import android.content.{ClipData, ClipboardManager, Context}
import org.bitcoinj.wallet.KeyChain.KeyPurpose
import rx.lang.scala.{Observable => Obs}
import com.lightning.wallet.lncloud.{ChannelWrap, Notificator, RatesSaver, Saver}
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
import com.lightning.wallet.ln.PaymentInfo._
import com.lightning.wallet.ln.wire.{Hop, Init, LightningMessage}
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.ln._
import com.lightning.wallet.ln.LNParams._
import com.lightning.wallet.ln.wire.LightningMessageCodecs._
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}
import fr.acinq.bitcoin.Crypto.PublicKey
import org.bitcoinj.core.listeners.{NewBestBlockListener, PeerConnectedEventListener}
import org.bitcoinj.net.discovery.DnsDiscovery
import org.spongycastle.crypto.params.KeyParameter


class WalletApp extends Application { me =>
  lazy val params = org.bitcoinj.params.RegTestParams.get
  lazy val prefs = getSharedPreferences("prefs", Context.MODE_PRIVATE)
  lazy val chainFile = new File(getFilesDir, s"$appName.spvchain")
  lazy val walletFile = new File(getFilesDir, s"$appName.wallet")
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

  def toast(code: Int): Unit = toast(app getString code)
  def toast(msg: CharSequence): Unit = Toast.makeText(me, msg, Toast.LENGTH_LONG).show
  def isAlive = if (null == kit) false else kit.state match { case STARTING | RUNNING => true case _ => false }
  def plurOrZero(opts: Array[String], number: Long) = if (number > 0) plur(opts, number) format number else opts(0)
  def clipboardManager = getSystemService(CLIPBOARD_SERVICE).asInstanceOf[ClipboardManager]
  def getBuffer = clipboardManager.getPrimaryClip.getItemAt(0).getText.toString
  def getTo(base58: String) = Address.fromBase58(params, base58)

  appReference = me
  override def onCreate = wrap(super.onCreate) {
    // These cannot just be lazy vals because their values may change
    denom = denoms apply prefs.getInt(AbstractKit.DENOMINATION, 0)
    fiatName = prefs.getString(AbstractKit.FIAT, strDollar)
  }

  def setBuffer(text: String) = wrap(me toast copied_to_clipboard) {
    clipboardManager setPrimaryClip ClipData.newPlainText(appName, text)
  }

  def newCrypter(pass: CharSequence) = getCrypterScrypt(KeyCrypterScrypt.randomSalt, pass)
  def getCrypterScrypt(salt: Bytes, pass: CharSequence): (KeyCrypterScrypt, KeyParameter) = {
    val crypterScryptBuilder = Protos.ScryptParameters.newBuilder.setSalt(ByteString copyFrom salt)
    val crypter = new KeyCrypterScrypt(crypterScryptBuilder.setN(65536).build)
    Tuple2(crypter, crypter deriveKey pass)
  }

  object TransData {
    var value: Any = _
    def onFail(err: Int => Unit): PartialFunction[Throwable, Unit] = {
      case _: org.bitcoinj.core.WrongNetworkException => err(err_different_net)
      case _: org.bitcoinj.core.AddressFormatException => err(err_address)
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

  object ChannelManager {
    import ConnectionManager._
    type ChannelVec = Vector[Channel]
    type OutPaymentOption = Option[OutgoingPayment]

    var all: ChannelVec = ChannelWrap.get map createChannel
    def alive: ChannelVec = all.filterNot(_.state == Channel.CLOSING)
    def from(of: ChannelVec, id: PublicKey) = of.filter(_.data.announce.nodeId == id)
    def reconnect(cv: ChannelVec) = cv.map(_.data.announce) foreach requestConnection

    val chainEventsListener = new TxTracker with BlocksListener {
      def height = for (chan <- all) chan process CMDBestHeight(broadcaster.currentHeight)
      override def coinsSent(tx: Transaction) = for (chan <- all) chan process CMDSpent(tx)
      override def txConfirmed(tx: Transaction) = for (chan <- alive) chan process CMDConfirmed(tx)
      def onBlocksDownloaded(p: Peer, b: Block, fb: FilteredBlock, left: Int) = if (left < 1) height
      override def onChainDownloadStarted(p: Peer, left: Int) = if (left < 1) height
    }

    val socketEventsListener = new ConnectionListener {
      override def onOperational(id: PublicKey, their: Init) = from(alive, id).foreach(_ process CMDOnline)
      override def onTerminalError(id: PublicKey) = from(alive, id).foreach(_ process CMDShutdown)
      override def onDisconnect(id: PublicKey) = from(alive, id).foreach(_ process CMDOffline)
      override def onMessage(msg: LightningMessage) = alive.foreach(_ process msg)
    }

    val reconnectListener = new ConnectionListener {
      override def onDisconnect(id: PublicKey) = Obs.just(Tools log s"Reconnecting socket $id")
        .delay(5.seconds).subscribe(_ => ChannelManager reconnect from(alive, id), Tools.errlog)
    }

    def createChannel(bootstrap: ChannelData) = new Channel {
      def SEND(msg: LightningMessage) = for (work <- connections get data.announce.nodeId) work send msg
      def STORE(content: HasCommitments): HasCommitments = runAnd(content)(ChannelWrap put content)
      listeners ++= Set(broadcaster, bag, ChannelWrap, Notificator)
      process(bootstrap)
    }

    // Here and in the rest of an app we use first alive channel
    def outPaymentObs(request: PaymentRequest) = alive.headOption map { chan =>
      val rsObs = cloud.connector.findRoutes(chan.data.announce.nodeId, request.nodeId)
      for (routes <- rsObs) yield outPaymentOpt(routes, request, chan)
    } getOrElse Obs.just(None)

    // Build payment if we actually have routes and channel has an id
    def outPaymentOpt(rs: Vector[PaymentRoute], request: PaymentRequest, chan: Channel) =
      Some(rs, chan.id) collectFirst { case (firstRoute +: restOfTheRoutes) \ Some(chanId) =>
        val (payloads, withFees, allExpiry) = buildPayloads(request.amount.get.amount, expiry, firstRoute)
        val onion = buildOnion(chan.data.announce.nodeId +: firstRoute.map(_.nextNodeId), payloads, request.paymentHash)
        OutgoingPayment(RoutingData(restOfTheRoutes, onion, withFees, allExpiry), NOIMAGE, request, MilliSatoshi(0), chanId, TEMP)
      }
  }

  abstract class WalletKit extends AbstractKit { self =>
    def currentBalance = wallet getBalance BalanceType.AVAILABLE_SPENDABLE
    def currentAddress = wallet currentAddress KeyPurpose.RECEIVE_FUNDS
    def currentHeight = blockChain.getBestChainHeight
    def shutDown = none

    def blockingSend(tx: Transaction) =
      // Wait for at least one peer confirmation or failure
      peerGroup.broadcastTransaction(tx, 1).broadcast.get

    def useCheckPoints(time: Long) = {
//      val pts = getAssets open "checkpoints-testnet.txt"
//      CheckpointManager.checkpoint(params, pts, store, time)
    }

    def setupAndStartDownload = {
      wallet addTransactionConfidenceEventListener ChannelManager.chainEventsListener
      wallet addCoinsSentEventListener ChannelManager.chainEventsListener
      wallet addTransactionConfidenceEventListener Vibr.generalTracker
      wallet addCoinsReceivedEventListener Vibr.generalTracker
      wallet addCoinsSentEventListener Vibr.generalTracker

      val address = InetAddresses forString cloud.connector.url
      peerGroup addAddress new PeerAddress(app.params, address, 8333)
      //peerGroup addPeerDiscovery new DnsDiscovery(params)
      peerGroup.setUserAgent(appName, "0.01")
      peerGroup setDownloadTxDependencies 0
      peerGroup setPingIntervalMsec 10000
      peerGroup setMaxConnections 6
      peerGroup addWallet wallet

      wallet setAcceptRiskyTransactions true
      wallet.autosaveToFile(walletFile, 100, MILLISECONDS, null)
      ConnectionManager.listeners += ChannelManager.socketEventsListener
      ConnectionManager.listeners += ChannelManager.reconnectListener
      startDownload(ChannelManager.chainEventsListener)
      ChannelManager reconnect ChannelManager.alive
      RatesSaver.process
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