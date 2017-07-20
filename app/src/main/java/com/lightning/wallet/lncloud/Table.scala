package com.lightning.wallet.lncloud

import com.lightning.wallet.ln.Tools.{none, runAnd}
import net.sqlcipher.database.SQLiteOpenHelper
import net.sqlcipher.database.SQLiteDatabase
import android.content.Context
import android.net.Uri


object StorageTable extends Table {
  val (table, key, value) = ("storage", "value", "key")
  def newSql = s"INSERT OR IGNORE INTO $table ($key, $value) VALUES (?, ?)"
  def updSql = s"UPDATE $table SET $value = ? WHERE $key = ?"
  def selectSql = s"SELECT * FROM $table WHERE $key = ?"
  def killSql = s"DELETE FROM $table WHERE $key = ?"

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $key TEXT NOT NULL UNIQUE,
      $value TEXT NOT NULL
    )"""
}

object ChannelTable extends Table {
  val (table, identifier, data) = ("channel", "identifier", "data")
  def newSql = s"INSERT OR IGNORE INTO $table ($identifier, $data) VALUES (?, ?)"
  def updSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  def selectAllSql = s"SELECT * FROM $table ORDER BY $id DESC"
  def killSql = s"DELETE FROM $table WHERE $identifier = ?"

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $identifier TEXT NOT NULL UNIQUE,
      $data TEXT NOT NULL
    )"""
}

object PaymentSpecTable extends Table {
  import com.lightning.wallet.ln.PaymentSpec._
  val (table, hash, status, channel, data, search) = ("payments", "hash", "status", "channel", "data", "search")
  def searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts4$table WHERE $search MATCH ? LIMIT 50)"
  def selectRecentSql = s"SELECT * FROM $table WHERE $status <> $HIDDEN ORDER BY $id DESC LIMIT 50"
  def selectByHashSql = s"SELECT * FROM $table WHERE $hash = ? LIMIT 1"

  def updDataSql = s"UPDATE $table SET $data = ? WHERE $hash = ?"
  def updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"
  def newVirtualSql = s"INSERT INTO $fts4$table ($search, $hash) VALUES (?, ?)"
  def newSql = s"INSERT OR IGNORE INTO $table ($hash, $status, $channel, $data) VALUES (?, ?, ?, ?)"

  // Create tables
  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $hash STRING UNIQUE NOT NULL,
      $status INTEGER NOT NULL,
      $channel STRING NOT NULL,
      $data STRING NOT NULL
    );
    CREATE INDEX idx1 ON $table ($status);
    CREATE INDEX idx2 ON $table ($hash);
    COMMIT"""

  def createVirtualSql = s"""
    CREATE VIRTUAL TABLE $fts4$table
    USING fts4($search, $hash)"""
}

trait Table { val (id, fts4) = "_id" -> "fts" }
class CipherOpenHelper(context: Context, version: Int, secret: String)
extends SQLiteOpenHelper(context, "lndata.db", null, version) { me =>

  SQLiteDatabase loadLibs context
  val base = getWritableDatabase(secret)
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: String*) = base.execSQL(sql, params.toArray)
  def select(sql: String, params: String*) = base.rawQuery(sql, params.toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.wallet/table/$tbl"

  def txWrap(run: => Unit) = try {
    runAnd(base.beginTransaction)(run)
    base.setTransactionSuccessful
  } finally base.endTransaction

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL PaymentSpecTable.createVirtualSql
    dbs execSQL PaymentSpecTable.createSql
    dbs execSQL StorageTable.createSql
    dbs execSQL ChannelTable.createSql
  }
}