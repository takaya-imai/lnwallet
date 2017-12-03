package com.lightning.wallet.test

import java.nio.ByteOrder
import fr.acinq.bitcoin.{BinaryData, Protocol}


class FeaturesSpec {
  import com.lightning.wallet.ln.Features._
  
  def allTests = {

    {
      println("'initial_routing_sync' feature")
      assert(initialRoutingSync(BinaryData("08")))
      assert(!initialRoutingSync(BinaryData("00")))
    }

    {
      println("'data_loss_protect' feature")
      assert(dataLossProtect(BinaryData("02")))
      assert(!dataLossProtect(BinaryData("00")))
    }

    {
      println("'initial_routing_sync' and 'data_loss_protect' feature")
      assert(areSupported(BinaryData("0a")) && initialRoutingSync(BinaryData("0a")) && dataLossProtect(BinaryData("0a")))
    }

    {
      println("features compatibility")
      assert(areSupported(BinaryData(Protocol.writeUInt64(1L << OPTION_DATA_LOSS_PROTECT_OPTIONAL, ByteOrder.BIG_ENDIAN))))
      assert(areSupported(BinaryData(Protocol.writeUInt64(1L << INITIAL_ROUTING_SYNC_BIT_OPTIONAL, ByteOrder.BIG_ENDIAN))))
      assert(!areSupported(BinaryData("14")))
      assert(!areSupported(BinaryData("0141")))
    }

  }
}
