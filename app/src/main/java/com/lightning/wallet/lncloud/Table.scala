package com.lightning.wallet.lncloud

import com.lightning.wallet.ln.Tools.{none, runAnd}
import net.sqlcipher.database.SQLiteOpenHelper
import net.sqlcipher.database.SQLiteDatabase
import android.content.Context
import android.net.Uri


object StorageTable extends Table {
  val (table, value, key) = ("storage", "value", "key")
  def newSql = s"INSERT OR IGNORE INTO $table ($value, $key) VALUES (?, ?)"
  def updSql = s"UPDATE $table SET $value = ? WHERE $key = ?"
  def selectSql = s"SELECT * FROM $table WHERE $key = ?"
  def killSql = s"DELETE FROM $table WHERE $key = ?"

  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $key TEXT NOT NULL UNIQUE,
      $value TEXT NOT NULL
    );"""
}

object PaymentSpecTable extends Table {
  import com.lightning.wallet.ln.PaymentSpec.{SUCCESS, HIDDEN}
  val (table, data, hash, status, stamp, searchData, count) = ("payments", "data", "hash", "status", "stamp", "search", "count")
  def selectVirtualSql: String = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $searchData MATCH ? LIMIT 25)"
  def selectRecentSql = s"SELECT * FROM $table WHERE $status = $SUCCESS OR ($status <> $HIDDEN AND $stamp > ?) ORDER BY $id DESC LIMIT 100"
  def selectByHashSql = s"SELECT * FROM $table WHERE $hash = ? LIMIT 1"
  def selectCountSql = s"SELECT COUNT(*) AS $count FROM $table"

  // Hidden -> Visible -> Failed or Success
  // Data must be updated in case of route switches
  def updDataSql = s"UPDATE $table SET $data = ? WHERE $hash = ?"
  def updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"

  // Searching by message and payment hash
  def newSql = s"INSERT OR IGNORE INTO $table ($data, $hash, $status, $stamp) VALUES (?, ?, ?, ?)"
  def newVirtualSql = s"INSERT INTO $fts$table ($searchData, $hash) VALUES (?, ?)"

  // Create tables
  def createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $data STRING NOT NULL,
      $hash STRING UNIQUE NOT NULL,
      $status STRING NOT NULL,
      $stamp INTEGER NOT NULL
    );
    CREATE INDEX idx1 ON $table ($status, $stamp);
    CREATE INDEX idx2 ON $table ($hash);
    COMMIT;"""

  def createVirtualSql = s"""
    CREATE VIRTUAL TABLE $fts$table
    USING fts4($searchData, $hash);"""
}

trait Table { val (id, fts) = "_id" -> "fts" }
class CipherOpenHelper(context: Context, version: Int, secret: String)
extends SQLiteOpenHelper(context, "lndata5.db", null, version) { me =>

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
  }
}