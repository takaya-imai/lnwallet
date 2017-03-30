package com.lightning.wallet.test

import com.lightning.wallet.ln.crypto.{ChaCha20Poly1305, Poly1305}
import fr.acinq.bitcoin.BinaryData


class ChaCha20Poly1305Spec {
  def poly1305 = {
    // from https://tools.ietf.org/html/rfc7539 ch 2.5.2
    val key: BinaryData = "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b"
    val data: BinaryData = "Cryptographic Forum Research Group".getBytes("UTF-8")
    val mac = Poly1305.mac(key, data)
    println(BinaryData(mac) == BinaryData("a8061dc1305136c6c22b8baf0c0127a9"))
  }

  def poly1305two = {
    // from golang's poly1305 tests
    val key: BinaryData = "this is 32-byte key for Poly1305".getBytes()
    val data: BinaryData = "Hello world!".getBytes("UTF-8")
    val mac = Poly1305.mac(key, data)
    println(BinaryData(mac) == BinaryData("a6f745008f81c916a20dcc74eef2b2f0"))
  }

  def ietf = {
    // see https://tools.ietf.org/html/rfc7539#section-2.8.1
    val plaintext = BinaryData("4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e")
    val key = BinaryData("808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f")
    val nonce = BinaryData("070000004041424344454647")
    val aad = BinaryData("50515253c0c1c2c3c4c5c6c7")

    val (ciphertext, mac) = ChaCha20Poly1305.encrypt(key, nonce, plaintext, aad)
    println(BinaryData(ciphertext) == BinaryData("d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"))
    println(BinaryData(mac) == BinaryData("1ae10b594f09e26a7e902ecbd0600691"))

    val check = ChaCha20Poly1305.decrypt(key, nonce, ciphertext, aad, mac)
    println(BinaryData(check) == plaintext)
  }

  def allTests = {
    poly1305
    poly1305two
    ietf
  }
}
