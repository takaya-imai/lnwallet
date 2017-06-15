package com.lightning.wallet.test

import com.lightning.wallet.ln.{IncomingPaymentSpec, Invoice, OutgoingPaymentSpec}
import com.lightning.wallet.ln.PaymentSpec._
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi}


class PaymentSpecSpec {
  val source = new HtlcGenerationSpec

  val (payloads, _, _) = buildRoute(source.finalAmountMsat, source.currentBlockCount + source.expiryDeltaBlocks, source.hops.drop(1))
  val nodes = source.hops.map(_.nextNodeId)
  val secretsAndPacket = buildOnion(nodes, payloads, source.paymentHash)

  val pk1 = PublicKey(BinaryData("0x02eec7245d6b7d2ccb30380bfbe2a3648cd7a942653f5aa340edcea1f283686619"))
  val pk2 = PublicKey(BinaryData("0x0324653eac434488002cc06bbfb7f10fe18991e35f9fe4302dbea6d2353dc0ab1c"))

  val inv1 = Invoice(Some("Pay for meat"), pk1, MilliSatoshi(100000), "11" * 32)
  val inv2 = Invoice(None, pk2, MilliSatoshi(20000), "33" * 32)

  val incoming = IncomingPaymentSpec(inv1, "44" * 32)
  val outgoing = OutgoingPaymentSpec(inv2, None, Vector(source.hops), secretsAndPacket, 101000, 45)
}
