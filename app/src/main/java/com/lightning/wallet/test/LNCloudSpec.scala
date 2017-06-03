package com.lightning.wallet.test

import com.lightning.wallet.ln._
import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.wire.{ChannelUpdate, Hop, NodeAnnouncement, UpdateFulfillHtlc}
import com.lightning.wallet.lncloud.JsonHttpUtils._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.{BinaryData, Crypto}
import fr.acinq.bitcoin.Crypto.PrivateKey
import rx.lang.scala.schedulers.IOScheduler

import scala.util.Try


class LNCloudSpec {
  def randomKey = PrivateKey(random getBytes 32)
  val (priv_a, priv_b, priv_c, priv_d, priv_e) = (randomKey, randomKey, randomKey, randomKey, randomKey)
  val (a, b, c, d, e) = (priv_a.publicKey, priv_b.publicKey, priv_c.publicKey, priv_d.publicKey, priv_e.publicKey)
  val sig = Crypto.encodeSignature(Crypto.sign(BinaryData.empty, priv_a)) :+ 1.toByte
  val defaultChannelUpdate = ChannelUpdate(sig, 0, 0, "0000", 0, 42000, 0, 0)
  val channelUpdate_ab = defaultChannelUpdate.copy(shortChannelId = 1, cltvExpiryDelta = 4, feeBaseMsat = 642000, feeProportionalMillionths = 7)
  val channelUpdate_bc = defaultChannelUpdate.copy(shortChannelId = 2, cltvExpiryDelta = 5, feeBaseMsat = 153000, feeProportionalMillionths = 4)
  val channelUpdate_cd = defaultChannelUpdate.copy(shortChannelId = 3, cltvExpiryDelta = 10, feeBaseMsat = 60000, feeProportionalMillionths = 1)
  val channelUpdate_de = defaultChannelUpdate.copy(shortChannelId = 4, cltvExpiryDelta = 7, feeBaseMsat = 766000, feeProportionalMillionths = 10)

  val preimage = BinaryData("9273f6a0a42b82d14c759e3756bd2741d51a0b3ecc5f284dbe222b59ea903942")

  object TestPaymentSpecBag extends PaymentSpecBag {
    def getRecentInfos: Vector[ExtendedPaymentInfo] = ???
    def getInfoByHash(hash: BinaryData): Try[ExtendedPaymentInfo] = Try(ExtendedPaymentInfo(null, PaymentSpec.SUCCESS, 0L))

    def putInfo(info: ExtendedPaymentInfo): Unit = ???
    def updatePaymentStatus(hash: BinaryData, status: String): Unit = ???
    def updateOutgoingPaymentSpec(spec: OutgoingPaymentSpec): Unit = ???
    def newPreimage = BinaryData(random getBytes 32)
  }

  val hops = Vector(
    Hop(a, b, channelUpdate_ab),
    Hop(b, c, channelUpdate_bc),
    Hop(c, d, channelUpdate_cd),
    Hop(d, e, channelUpdate_de))

  val chan = new Channel {

    data = InitData(NodeAnnouncement(null, 1, e, null, null, null, null))

    override def doProcess(change: Any) = change match {
      case SilentAddHtlc(spec) =>
        println(s"Channel mock got a silent spec: $spec")
        process(UpdateFulfillHtlc(null, 1, preimage))

      case _ =>
        println(s"Channel mock got something: $change")
    }

  }

  def getCloud =
    PublicDataSaver.tryGetObject.map { savedData =>
      println(s"Got LNCloudData from db: $savedData")

      new PublicPathfinder(TestPaymentSpecBag, new LNCloud("http://10.0.2.2:9002"), chan) {
        data = savedData

        override def makeOutgoingSpec(invoice: Invoice) = obsOn( {
          PaymentSpec.makeOutgoingSpec(Vector(hops), invoice, LNParams.myHtlcExpiry)
        }, IOScheduler.apply)
      }

    } getOrElse {
      val data1 = PublicData(info = None, tokens = Nil, acts = for (n <- (0 to 400).toList) yield CheckCloudAct(s"call $n"))
      println(s"Creating a new LNCloudData")

      new PublicPathfinder(TestPaymentSpecBag, new LNCloud("http://10.0.2.2:9002"), chan) {
        data = data1

        override def makeOutgoingSpec(invoice: Invoice) = obsOn( {
          PaymentSpec.makeOutgoingSpec(Vector(hops), invoice, LNParams.myHtlcExpiry)
        }, IOScheduler.apply)
      }

    }

  def allTests = {
    val cloud1 = getCloud
    println("preprocessing...")
    cloud1 process CheckCloudAct("call a")
  }
}
