package com.lightning.wallet.lncloud

import fr.acinq.bitcoin.{BinaryData, Crypto}
import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.ln.LNParams


case class StandaloneCloud(url: String) extends LNCloud {
  lazy val prefix: String = LNParams.extendedCloudPrivateKey.publicKey.toString take 8
  override def http(command: String): HttpRequest = HttpRequest.post(s"$url/v1/$command", true) connectTimeout 10000
  def sign(data: BinaryData): BinaryData = Crypto encodeSignature Crypto.sign(data, LNParams.extendedCloudPrivateKey.privateKey)
  def tryIfWorks(data: BinaryData) = call("sig/check", identity, "sig" -> sign(data).toString, "data" -> data.toString, "prefix" -> prefix)
}