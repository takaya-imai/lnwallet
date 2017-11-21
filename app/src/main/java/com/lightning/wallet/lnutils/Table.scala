package com.lightning.wallet.lnutils

import com.lightning.wallet.ln.Tools.{none, runAnd}
import net.sqlcipher.database.SQLiteOpenHelper
import net.sqlcipher.database.SQLiteDatabase
import android.content.Context
import android.net.Uri


object StorageTable extends Table {
  val (table, key, value) = ("storage", "value", "key")
  def updSql = s"UPDATE $table SET $value = ? WHERE $key = ?"
  def selectSql = s"SELECT * FROM $table WHERE $key = ?"
  def killSql = s"DELETE FROM $table WHERE $key = ?"

  def newSql = s"""
    INSERT OR IGNORE INTO $table
    ($key, $value) VALUES (?, ?)"""

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $key TEXT NOT NULL UNIQUE,
      $value TEXT NOT NULL
    )"""
}

object ChannelTable extends Table {
  // Order is important! New channels should always be on top
  val (table, identifier, data) = ("channel", "identifier", "data")
  def updSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  def selectAllSql = s"SELECT * FROM $table ORDER BY $id DESC"
  def killSql = s"DELETE FROM $table WHERE $identifier = ?"

  def newSql = s"""
    INSERT OR IGNORE INTO $table
    ($identifier, $data) VALUES (?, ?)"""

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $identifier TEXT NOT NULL UNIQUE,
      $data TEXT NOT NULL
    )"""
}

object PaymentInfoTable extends Table {
  import com.lightning.wallet.ln.PaymentInfo.{HIDDEN, FAILURE, WAITING}
  val names = ("payment", "hash", "incoming", "preimage", "amount", "status", "stamp", "text", "search")
  val Tuple9(table, hash, incoming, preimage, amount, status, stamp, text, search) = names

  def newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"
  def newSql = s"REPLACE INTO $table ($hash, $incoming, $preimage, $amount, $status, $text, $stamp) VALUES (?, ?, ?, ?, ?, ?, ?)"
  def searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $search MATCH ? LIMIT 24)"
  def selectRecentSql = s"SELECT * FROM $table WHERE $status <> $HIDDEN ORDER BY $id DESC LIMIT 24"
  def selectSql = s"SELECT * FROM $table WHERE $hash = ?"

  def updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"
  def updAmountSql = s"UPDATE $table SET $amount = ? WHERE $hash = ?"
  def updPreimageSql = s"UPDATE $table SET $preimage = ? WHERE $hash = ?"
  def updFailWaitingSql = s"UPDATE $table SET $status = $FAILURE WHERE $status = $WAITING"
  def createVirtualSql = s"CREATE VIRTUAL TABLE $fts$table USING $fts($search, $hash)"

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $hash STRING UNIQUE NOT NULL,
      $incoming INTEGER NOT NULL,
      $preimage STRING NOT NULL,
      $amount INTEGER NOT NULL,
      $status STRING NOT NULL,
      $stamp INTEGER NOT NULL
      $text STRING NOT NULL
    );
    CREATE INDEX idx1 ON $table ($status);
    CREATE INDEX idx2 ON $table ($hash);
    COMMIT"""
}

object RoutingDataTable extends Table {
  val (table, hash, routing) = ("outgoing", "hash", "routing")
  def newSql = s"REPLACE INTO $table ($hash, $routing) VALUES (?, ?)"
  def selectSql = s"SELECT * FROM $table WHERE $hash = ?"

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $hash STRING UNIQUE NOT NULL,
      $routing STRING NOT NULL
    );
    CREATE INDEX idx ON $table ($hash);
    COMMIT"""
}

trait Table { val (id, fts) = "_id" -> "fts4" }
class CipherOpenHelper(context: Context, version: Int, secret: String)
extends SQLiteOpenHelper(context, "lndata.db", null, version) { me =>

  SQLiteDatabase loadLibs context
  val base = getWritableDatabase(secret)
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: String*) = base.execSQL(sql, params.toArray)
  def select(sql: String, params: String*) = base.rawQuery(sql, params.toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.wallet/table/$tbl"

  def txWrap(process: => Unit) = try {
    runAnd(base.beginTransaction)(process)
    base.setTransactionSuccessful
  } finally base.endTransaction

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL PaymentInfoTable.createVirtualSql
    dbs execSQL PaymentInfoTable.createSql
    dbs execSQL RoutingDataTable.createSql
    dbs execSQL StorageTable.createSql
    dbs execSQL ChannelTable.createSql
  }
}