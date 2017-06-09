package com.lightning.wallet.test


class FeaturesSpec {
  import com.lightning.wallet.ln.Features._
  
  def allTests = {

    {
      println("'channel_public' feature")
      println(!isSet("", CHANNELS_PUBLIC_BIT))
      println(!isSet("00", CHANNELS_PUBLIC_BIT))
      println(isSet("01", CHANNELS_PUBLIC_BIT))
      println(!isSet("a602", CHANNELS_PUBLIC_BIT))
    }

    {
      println("'initial_routing_sync' feature")
      println(!isSet("", INITIAL_ROUTING_SYNC_BIT))
      println(!isSet("00", INITIAL_ROUTING_SYNC_BIT))
      println(isSet("04", INITIAL_ROUTING_SYNC_BIT))
      println(isSet("05", INITIAL_ROUTING_SYNC_BIT))
    }

    {
      println("features compatibility")
      for (i <- 0 until 16) println(areSupported(Array[Byte](i.toByte)))
      println(!areSupported("14"))
      println(!areSupported("0141"))
      println(areSupported("02af"))
    }

  }

}
