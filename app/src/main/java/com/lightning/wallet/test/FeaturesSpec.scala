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
      println("public channel:" + isSet("01", CHANNELS_PUBLIC_BIT))
      println("initial routing sync:" + isSet("01", INITIAL_ROUTING_SYNC_BIT))
    }

  }

}
