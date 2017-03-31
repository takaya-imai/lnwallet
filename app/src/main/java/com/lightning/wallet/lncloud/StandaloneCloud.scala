package com.lightning.wallet.lncloud

import com.github.kevinsawicki.http.HttpRequest
import com.lightning.wallet.ln.Tools.Bytes
import com.lightning.wallet.ln.LNParams
import org.bitcoinj.core.Sha256Hash
import org.bitcoinj.core.Utils.HEX


case class StandaloneCloud(url: StandaloneCloudSaver.Snapshot) extends LNCloud {
  override def http(command: String): HttpRequest = HttpRequest.post(s"$url/v1/$command", true) connectTimeout 10000
  def tryIfWorks(data: Bytes) = call("sig/check", identity, "sig" -> sign(data), "data" -> HEX.encode(data), "prefix" -> prefix)
  def sign(data: Bytes) = HEX.encode(LNParams.extendedCloudPrivateKey.sign(Sha256Hash wrap data).encodeToDER)
  lazy val prefix = LNParams.extendedCloudPrivateKey.getPublicKeyAsHex take 8
}