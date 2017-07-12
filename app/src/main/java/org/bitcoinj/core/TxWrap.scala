package org.bitcoinj.core

import com.lightning.wallet.Utils._
import scala.collection.JavaConverters._
import org.bitcoinj.wallet.WalletTransaction.Pool._
import com.lightning.wallet.{AddrData, P2WSHData}
import com.lightning.wallet.ln.~
import scala.util.Try


class TxWrap(val tx: Transaction) {
  val nativeValue = getValueSentToMe subtract getValueSentFromMe
  val marking = if (nativeValue.isPositive) sumIn else sumOut
  val isNative = !nativeValue.isZero

  private def inOuts(input: TransactionInput): Option[TransactionOutput] =
    Stream(UNSPENT, SPENT, PENDING).map(app.kit.wallet.getTransactionPool)
      .map(input.getConnectedOutput).find(_ != null)

  private def getValueSentFromMe = tx.getInputs.asScala.flatMap(inOuts).foldLeft(Coin.ZERO) {
    case accumulator ~ output if output isMine app.kit.wallet => accumulator add output.getValue
    case accumulator ~ _ => accumulator
  }

  private def getValueSentToMe = tx.getOutputs.asScala.foldLeft(Coin.ZERO) {
    case accumulator ~ out if out isMine app.kit.wallet => accumulator add out.getValue
    case accumulator ~ _ => accumulator
  }

  def payDatas = tx.getOutputs.asScala filter { out =>
    out.isMine(app.kit.wallet) == nativeValue.isPositive
  } map outputToPayData

  private def outputToPayData(out: TransactionOutput) = Try(out.getScriptPubKey) map {
    case publicKeyScript if publicKeyScript.isSentToP2WSH => P2WSHData(out.getValue, publicKeyScript)
    case publicKeyScript => AddrData(out.getValue, publicKeyScript getToAddress app.params)
  }
}