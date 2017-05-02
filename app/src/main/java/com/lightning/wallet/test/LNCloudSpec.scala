package com.lightning.wallet.test

import com.lightning.wallet.ln.Tools._
import com.lightning.wallet.ln.wire.{ChannelUpdate, Hop}
import com.lightning.wallet.lncloud.DefaultLNCloudSaver._
import com.lightning.wallet.lncloud._
import fr.acinq.bitcoin.{BinaryData, Crypto}
import fr.acinq.bitcoin.Crypto.PrivateKey
import rx.lang.scala.{Observable => Obs}


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

  val hops = Vector(
    Hop(a, b, channelUpdate_ab),
    Hop(b, c, channelUpdate_bc),
    Hop(c, d, channelUpdate_cd),
    Hop(d, e, channelUpdate_de))

  def getCloud = ???
//    DefaultLNCloudSaver.tryGetObject.map { savedData =>
//      println(s"Got LNCloudData from db: $savedData")
//      val cloud = new DefaultLNCloud(PaymentSpecWrap)
//      cloud.become(savedData, LNCloud.OPERATIONAL)
//      cloud
//    } getOrElse {
//      val data = LNCloudData(info = None, tokens = Nil, acts = Nil)
//      println(s"Creating a new LNCloudData")
//      val cloud = new DefaultLNCloud(PaymentSpecWrap)
//      cloud.become(data, LNCloud.OPERATIONAL)
//      cloud
//    }

  def allTests = {

//    val a = System.currentTimeMillis()
//    val cloud1 = getCloud
//    println(cloud1)
//    DefaultLNCloudSaver.saveObject(cloud1.data)
//
//    val cloud2 = getCloud
//    println(cloud2)
//
//    cloud2 process PingCloudAct(System.currentTimeMillis.toString)
//    println(cloud2.data)
//    DefaultLNCloudSaver.saveObject(cloud2.data)
//    println(System.currentTimeMillis() - a)
  }
}
