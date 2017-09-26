package com.lightning.wallet.helper

import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import com.lightning.wallet.ln.Tools.{random, Bytes}
import org.bitcoinj.core.Utils.HEX
import javax.crypto.Cipher


object AES {
  def cipher(key: Bytes, initVector: Bytes, mode: Int) =
    Cipher getInstance "AES/CTR/NoPadding" match { case aesCipher =>
      val ivParameterSpec: IvParameterSpec = new IvParameterSpec(initVector)
      aesCipher.init(mode, new SecretKeySpec(key, "AES"), ivParameterSpec)
      aesCipher
    }

  def encCypher(key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.ENCRYPT_MODE)
  def decCypher(key: Bytes, initVector: Bytes) = cipher(key, initVector, Cipher.DECRYPT_MODE)
  def enc(data: Bytes, key: Bytes, initVector: Bytes) = encCypher(key, initVector) doFinal data
  def dec(data: Bytes, key: Bytes, initVector: Bytes) = decCypher(key, initVector) doFinal data

  def exportData(plain: String, key: Bytes): String = {
    // Takes any string and returns an encoded string with iv
    // Can be directly sent over internet connection

    val initVector = random getBytes 16
    val cipherbytes = enc(plain getBytes "UTF-8", key, initVector)
    Seq(cipherbytes, initVector) map HEX.encode mkString ":"
  }

  def importData(cipher: String, key: Bytes): String = {
    val Array(cipherbytes, iv) = cipher split ":" map HEX.decode
    val plainbytes = dec(cipherbytes, key, iv)
    new String(plainbytes, "UTF-8")
  }
}