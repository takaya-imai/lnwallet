package com.lightning.wallet.lncloud

import com.lightning.wallet.ln.Tools.{none, runAnd}
import net.sqlcipher.database.SQLiteOpenHelper
import net.sqlcipher.database.SQLiteDatabase
import android.content.Context
import android.net.Uri


object Storage extends Table {
  val (table, value, key) = ("storage", "value", "key")
  def newSql = s"INSERT OR IGNORE INTO $table ($value, $key) VALUES (?, ?)"
  def updSql = s"UPDATE $table SET $value = ? WHERE $key = ?"
  def selectSql = s"SELECT * FROM $table WHERE $key = ?"
  def killSql = s"DELETE FROM $table WHERE $key = ?"

  def createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $key TEXT NOT NULL UNIQUE,
      $value TEXT NOT NULL
    );"""
}

object Commits extends Table {
  val strings = ("commits", "parenttxid", "punish", "privcloud", "lncloud")
  val (table, parentTxId, punishData, privateCloud, lnCloud) = strings

  def updLnCloudOnSql = s"UPDATE $table SET $lnCloud = 1 WHERE $parentTxId = ?"
  def updPrivateCloudOnSql = s"UPDATE $table SET $privateCloud = 1 WHERE $parentTxId = ?"
  def selectPrivateCloudOffSql = s"SELECT * FROM $table WHERE $privateCloud = 0 LIMIT 50"
  def selectLnCloudOffSql = s"SELECT * FROM $table WHERE $lnCloud = 0 LIMIT 50"
  def selectByParentTxIdSql = s"SELECT * FROM $table WHERE $parentTxId = ?"

  def newSql = s"""INSERT OR IGNORE INTO $table
    ($parentTxId, $punishData, $privateCloud, $lnCloud)
    VALUES (?, ?, 0, 0)"""

  def createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $parentTxId TEXT NOT NULL UNIQUE,
      $punishData TEXT NOT NULL UNIQUE,
      $privateCloud INTEGER NOT NULL,
      $lnCloud INTEGER NOT NULL
    );
    CREATE INDEX idx1 ON $table ($parentTxId);
    CREATE INDEX idx2 ON $table ($privateCloud);
    CREATE INDEX idx3 ON $table ($lnCloud);
    COMMIT"""
}

object Payments extends Table {
  val strings = ("payments", "preimage", "hash", "status", "stamp", "sum", "message", "incoming", "nodeid", "fee", "search")
  val (table, preimage, hash, status, stamp, sum, message, incoming, nodeId, fee, searchData) = strings
  val (waitHidden, waitVisible, fail, success) = (0, 1, 2, 3)

  def selectByHashSql = s"SELECT * FROM $table WHERE $preimage = ? LIMIT 1"
  def selectRecentSql = s"""SELECT * FROM $table WHERE $status = $success OR
    ($status <> $waitHidden AND $stamp > ?) ORDER BY $id DESC LIMIT 100"""

  // Hidden -> Visible -> Failed or Success
  def updFeeSql = s"UPDATE $table SET fee = ? WHERE $hash = ?"
  def updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"
  def updPreimageSql = s"UPDATE $table SET $preimage = ? WHERE $hash = ?"

  def newSql = s"""INSERT OR IGNORE INTO $table ($preimage, $hash, $status, $stamp,
    $sum, $message, $incoming, $nodeId, $fee) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"""

  // Searching by message and payment hash
  def newVirtualSql = s"INSERT INTO $fts$table ($searchData, $hash) VALUES (?, ?)"
  def createVirtualSql = s"CREATE VIRTUAL TABLE $fts$table USING fts4($searchData, $hash)"
  def searchVirtualSql = s"""SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table
    WHERE $searchData MATCH ? LIMIT 20)"""

  // Create
  def createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $preimage STRING,
      $hash STRING UNIQUE NOT NULL,
      $status INTEGER NOT NULL,
      $stamp INTEGER NOT NULL,
      $sum INTEGER NOT NULL,
      $message STRING,
      $incoming INTEGER NOT NULL,
      $nodeId STRING NOT NULL,
      $fee INTEGER
    );
    CREATE INDEX idx1 ON $table ($status, $stamp);
    CREATE INDEX idx2 ON $table ($hash);
    COMMIT"""
}

trait Table { val (id, fts) = "_id" -> "fts" }
class CipherOpenHelper(context: Context, version: Int, secret: String)
extends SQLiteOpenHelper(context, "lndata4.db", null, version)
{
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
    dbs execSQL Payments.createVirtualSql
    dbs execSQL Payments.createSql
    dbs execSQL Storage.createSql
    dbs execSQL Commits.createSql
  }
}