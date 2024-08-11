package com.ivanb.part3async

import java.util.concurrent.Executors

object JVMConcurrencyIntro {

  def basicThreads(): Unit = {
    val runnable = new Runnable:

      override def run(): Unit =
        println("Waiting ...")
        Thread.sleep(2000)
        println("Running on a thread")

    val aThread = new Thread(runnable)
    aThread
      .start() // will run the runnable on a JVM thread, JVM thread == OS thread (until project loom)
    // block until thread finishes
    aThread.join()
  }

  // order of operations is never guaranteed

  def orderOfExecution(): Unit = {
    val threadHello = new Thread(() => (1 to 5).foreach(_ => println("Hello")))
    val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("Goodbye")))
    threadHello.start() // NOTE: not blocking, the two threads will start almost at the same time
    threadGoodbye.start()
  }

  def demoExecutors(): Unit = {
    val threadPool = Executors.newFixedThreadPool(4)
    threadPool.execute(() => println("something in the thread pool"))
    threadPool.execute { () =>
      {
        Thread.sleep(1000)
        println("Done after one second")
      }
    }
    threadPool.execute{
      () =>
        Thread.sleep(1000)
        println("Almost done")
        Thread.sleep(1000)
        println("Done after 2 seconds")
    }

    // NOTE: all tasks are scheduled in parallel
    threadPool.shutdown()
  }

  def main(args: Array[String]): Unit = {
    demoExecutors()
  }

}
