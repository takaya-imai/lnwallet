package com.lightning.wallet.test

import java.net.InetSocketAddress

import com.lightning.wallet.ln.Announcements._
import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.PrivateKey

import scala.util.Random


class AnnouncementsSpec {
  def randomKey: PrivateKey = PrivateKey({
    val bin = Array.fill[Byte](32)(0)
    Random.nextBytes(bin)
    bin
  }, compressed = true)

  def allTests = {

    {
      println("create valid signed channel announcement")
      val (node_a, node_b, bitcoin_a, bitcoin_b) = (randomKey, randomKey, randomKey, randomKey)
      val (node_a_sig, bitcoin_a_sig) = signChannelAnnouncement(42, node_a, node_b.publicKey, bitcoin_a, bitcoin_b.publicKey, "")
      val (node_b_sig, bitcoin_b_sig) = signChannelAnnouncement(42, node_b, node_a.publicKey, bitcoin_b, bitcoin_a.publicKey, "")
      val ann = makeChannelAnnouncement(42, node_a.publicKey, node_b.publicKey, bitcoin_a.publicKey, bitcoin_b.publicKey, node_a_sig, node_b_sig, bitcoin_a_sig, bitcoin_b_sig)
      println(checkSigs(ann))
      println(!checkSigs(ann.copy(nodeId1 = randomKey.publicKey)))
    }

    val alicePk = PrivateKey(BinaryData("cc" * 32), compressed = true)

    {
      println("create valid signed node announcement")
      val ann = makeNodeAnnouncement(alicePk, "alias", (1: Byte, 2: Byte, 3: Byte), new InetSocketAddress("localhost", 9731) :: Nil, System.currentTimeMillis / 1000)
      println(checkSig(ann))
      println(!checkSig(ann.copy(timestamp = 153)))
    }

    {
      println("create valid signed channel update announcement")
      val ann = makeChannelUpdate(alicePk, randomKey.publicKey, 45561, 144, 0, 546000, 10, System.currentTimeMillis / 1000)
      println(checkSig(ann, alicePk.publicKey))
      println(!checkSig(ann, randomKey.publicKey))
    }

  }
}
