package com.lightning.wallet.test

import com.lightning.wallet.ln.{DataTransport, StateMachine, TransportHandler}
import com.lightning.wallet.ln.crypto.Noise
import fr.acinq.bitcoin.BinaryData

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import TransportHandler.Send


class TransportHandlerSpec {
  def allTests = {
    object Initiator {
      val s = Noise.Secp256k1DHFunctions.generateKeyPair("1111111111111111111111111111111111111111111111111111111111111111")
    }

    object Responder {
      val s = Noise.Secp256k1DHFunctions.generateKeyPair("2121212121212121212121212121212121212121212121212121212121212121")
    }

    lazy val bob: TransportHandler = new TransportHandler(Initiator.s, Some(Responder.s.pub),
      data => println("Bob channel got: " + new String(data.toArray, "UTF-8")),
      new DataTransport {
        def send(data: BinaryData) = Future {
          Thread.sleep(1000)
          alice.process(data)
        }
      })

    lazy val alice: TransportHandler = new TransportHandler(Responder.s, None,
      data => println("Alice channel got: " + new String(data.toArray, "UTF-8")),
      new DataTransport {
        def send(data: BinaryData) = Future {
          Thread.sleep(1000)
          bob.process(data)
        }
      })

    bob
    bob.process((Send, BinaryData("unhandled 1" getBytes "UTF-8")))
    bob.process((Send, BinaryData("unhandled 2" getBytes "UTF-8")))
    alice
    alice.process((Send, BinaryData("unhandled 1" getBytes "UTF-8")))
    alice.process((Send, BinaryData("unhandled 2" getBytes "UTF-8")))

    Future {
      Thread.sleep(10000)
      println(bob.state)
      println(alice.state)

      bob.process((Send, BinaryData("hello1" getBytes "UTF-8")))
      Thread.sleep(500)
      bob.process((Send, BinaryData("hello2" getBytes "UTF-8")))
      Thread.sleep(500)
      bob.process((Send, BinaryData("hello3" getBytes "UTF-8")))
      Thread.sleep(500)
      bob.process((Send, BinaryData("hello4" getBytes "UTF-8")))
      Thread.sleep(500)
      bob.process((Send, BinaryData("hello5" getBytes "UTF-8")))
      Thread.sleep(500)
      bob.process((Send, BinaryData("hello10" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello20" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello30" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello40" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello50" getBytes "UTF-8")))

      Thread.sleep(5000)

      alice.process((Send, BinaryData("hi1" getBytes "UTF-8")))
      Thread.sleep(500)
      alice.process((Send, BinaryData("hi2" getBytes "UTF-8")))
      Thread.sleep(500)
      alice.process((Send, BinaryData("hi3" getBytes "UTF-8")))
      Thread.sleep(500)
      alice.process((Send, BinaryData("hi4" getBytes "UTF-8")))
      Thread.sleep(500)
      alice.process((Send, BinaryData("hi5" getBytes "UTF-8")))
      Thread.sleep(500)
      alice.process((Send, BinaryData("hi10" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi20" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi30" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi40" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi50" getBytes "UTF-8")))

      Thread.sleep(5000)

      bob.process((Send, BinaryData("hello100" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi100" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello200" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi200" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello300" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi300" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello400" getBytes "UTF-8")))
      alice.process((Send, BinaryData("hi400" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello500" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello501" getBytes "UTF-8")))
      bob.process((Send, BinaryData("hello502" getBytes "UTF-8")))
    }
  }
}
