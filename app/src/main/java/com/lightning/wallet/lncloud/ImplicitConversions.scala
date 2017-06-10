package com.lightning.wallet.lncloud

import com.lightning.wallet.Utils.app
import org.bitcoinj.core.Sha256Hash
import language.implicitConversions
import fr.acinq.bitcoin.BinaryData
import java.math.BigInteger
import android.text.Html


object ImplicitConversions {
  implicit def string2Ops(raw: String): StringOps = new StringOps(raw)
  implicit def anyToRunnable(process: => Unit): Runnable = new Runnable { def run = process }
  implicit def binaryData2Sha256hash(data: BinaryData): Sha256Hash = Sha256Hash wrap data.toArray
  implicit def bitcoinLibScript2bitcoinjScript(pubKeyScript: BinaryData): org.bitcoinj.script.Script =
    new org.bitcoinj.script.Script(pubKeyScript, System.currentTimeMillis)

  implicit def bitcoinjTx2bitcoinLibTx(bitcoinjTx: org.bitcoinj.core.Transaction): fr.acinq.bitcoin.Transaction =
    fr.acinq.bitcoin.Transaction.read(bitcoinjTx.unsafeBitcoinSerialize)

  implicit def bitcoinLibTx2bitcoinjTx(bitcoinLibTx: fr.acinq.bitcoin.Transaction): org.bitcoinj.core.Transaction =
    new org.bitcoinj.core.Transaction(app.params, fr.acinq.bitcoin.Transaction write bitcoinLibTx)
}

class StringOps(source: String) {
  def binary = BinaryData(source getBytes "UTF-8")
  def bigInteger = new BigInteger(source)
  def noCommas = source.replace(",", "")
  def html = Html fromHtml source
  def hex = binary.toString
}