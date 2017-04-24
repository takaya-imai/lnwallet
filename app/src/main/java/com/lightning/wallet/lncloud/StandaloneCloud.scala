package com.lightning.wallet.lncloud

import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.lightning.wallet.ln.LNParams


case class StandaloneCloud(override val server: String) extends LNCloud {
  lazy val prefix: String = LNParams.extendedCloudPrivateKey.publicKey.toString take 8
  def sign(data: BinaryData): BinaryData = Crypto encodeSignature Crypto.sign(data, LNParams.extendedCloudPrivateKey.privateKey)
  def tryIfWorks(data: BinaryData) = call("sig/check", identity, "sig" -> sign(data).toString, "data" -> data.toString, "prefix" -> prefix)
}