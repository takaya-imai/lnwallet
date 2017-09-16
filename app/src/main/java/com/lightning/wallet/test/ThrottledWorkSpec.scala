package com.lightning.wallet.test

import rx.lang.scala.{Scheduler, Observable => Obs}
import com.lightning.wallet.helper.ThrottledWork
import com.lightning.wallet.lncloud.JsonHttpUtils._
import rx.lang.scala.schedulers.IOScheduler

/**
  * Created by anton on 20.05.17.
  */
class ThrottledWorkSpec {
  def allTests = {

    val worker = new ThrottledWork[String, String] {
      def work(input: String) = obsOn({
        Thread.sleep(5000)
        input * 2
      }, IOScheduler.apply)

      def process(result: String) = println(result)
      def error(err: Throwable) = err.printStackTrace
    }

    worker.addWork("t")
    worker.addWork("te")
    worker.addWork("tes")
    worker.addWork("test")
    Thread.sleep(6000)
    worker.addWork("n")
    worker.addWork("ne")
    worker.addWork("new")
  }
}
