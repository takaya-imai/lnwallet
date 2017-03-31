package com.lightning.wallet.lncloud

import com.lightning.wallet.ln.JavaSerializer._
import com.lightning.wallet.ln.{Channel, ChannelData, ScheduledTx}
import com.lightning.wallet.Utils.app
import scala.collection.mutable
import scala.util.Try


trait Saver {
  type Snapshot
  val KEY: String
  def save(snap: Snapshot) = StorageWrap.put(serialize(snap), KEY)
  def get: Try[Snapshot] = StorageWrap get KEY map deserialize
}

object BlindTokensSaver extends Saver {
  // Blinding point, clear token and sig
  type ClearToken = (String, String, String)
  type Snapshot = (Set[ClearToken], List[String], BlindProgress)
  val initState: Snapshot = (Set.empty, BlindTokens.OPERATIONAL :: Nil, null)
  val KEY = "blindTokens"
}

object ChannelSaver extends Saver {
  type Snapshot = (List[String], ChannelData)
  val initState: Snapshot = (Channel.INITIALIZED :: Nil, null)
  val KEY = "channel"
}

object StandaloneCloudSaver extends Saver {
  def remove = app.db.change(Storage.killSql, KEY)
  val KEY = "standaloneCloud"
  type Snapshot = String
}

object BroadcasterSaver extends Saver {
  type Snapshot = mutable.Set[ScheduledTx]
  val KEY = "broadcaster"
}