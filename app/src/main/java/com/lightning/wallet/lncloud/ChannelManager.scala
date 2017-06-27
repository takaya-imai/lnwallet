package com.lightning.wallet.lncloud

import com.lightning.wallet.ln._
import com.lightning.wallet.Utils._
import com.lightning.wallet.lncloud.ImplicitConversions._
import org.bitcoinj.core.{StoredBlock, Transaction}

import com.lightning.wallet.lncloud.ChannelSaver.ChannelSnapshot
import org.bitcoinj.core.listeners.NewBestBlockListener
import com.lightning.wallet.TxTracker
import fr.acinq.bitcoin.BinaryData


object ChannelManager {
  var all = ChannelSaver.tryGetObject getOrElse Vector.empty map fresh
  def fromNode(id: BinaryData) = all.filter(_.data.announce.nodeId == id)
  def operational = all.filterNot(_.data == Channel.CLOSING)

  val blockchainListener = new TxTracker with NewBestBlockListener {
    override def coinsSent(tx: Transaction) = for (chan <- all) chan process CMDSomethingSpent(tx)
    override def txConfirmed(tx: Transaction) = for (chan <- operational) chan process CMDSomethingConfirmed(tx)
    override def notifyNewBestBlock(block: StoredBlock) = for (chan <- operational) chan process CMDDepth(block.getHeight)
  }

  app.kit.wallet addCoinsSentEventListener blockchainListener
  app.kit.blockChain addNewBestBlockListener blockchainListener
  app.kit.wallet addTransactionConfidenceEventListener blockchainListener

  def fresh(snapshot: ChannelSnapshot) =
    new Channel match { case freshChannel =>
      val recoveredData ~ recoveredState = snapshot
      freshChannel.listeners += LNParams.broadcaster
      freshChannel.state = recoveredState
      freshChannel.data = recoveredData
      freshChannel
    }
}