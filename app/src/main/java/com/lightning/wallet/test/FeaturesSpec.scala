package com.lightning.wallet.test

import java.nio.ByteOrder
import fr.acinq.bitcoin.{BinaryData, Protocol}


class FeaturesSpec {
  import com.lightning.wallet.ln.Features._
  
  def allTests = {

    {
      println("'initial_routing_sync' feature")
      assert(initialRoutingSync(BinaryData("02")))
    }

    {
      println("features compatibility")
      assert(!areSupported(BinaryData(Protocol.writeUInt64(1L << INITIAL_ROUTING_SYNC_BIT_MANDATORY, ByteOrder.BIG_ENDIAN))))
      assert(areSupported(BinaryData(Protocol.writeUInt64(1l << INITIAL_ROUTING_SYNC_BIT_OPTIONAL, ByteOrder.BIG_ENDIAN))))
      assert(!areSupported(BinaryData("14")))
      assert(!areSupported(BinaryData("0141")))
    }

  }
}
