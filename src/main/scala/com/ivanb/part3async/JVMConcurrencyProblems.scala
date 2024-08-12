package com.ivanb.part3async

object JVMConcurrencyProblems {
  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => x = 1)
    val thread2 = new Thread(() => x = 2)

    thread1.start()
    thread2.start()
    Thread.sleep(100) // race condition
    println(x)
  }

  case class BankAccount(var amount: Int)

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
  }

  def buySafe(account: BankAccount, thing: String, price: Int) = {
    account
      .synchronized { // will not allow multiple threads to execute the code block (critical section) at the same time
        account.amount -= price
      }
  }

  def inceptionThreads(maxCount: Int, current: Int): Thread =
    new Thread(() => {
      if (current < maxCount) {
        val child = inceptionThreads(maxCount, current + 1)
        child.start()
        child.join()
      }
      println(s"Hello from thread $current")
    })

  def demoBankingProblem(): Unit = {
    (1 to 10000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buy(account, "shoes", 3000))
      val thread2 = new Thread(() => buy(account, "phone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000)
        println(s"Something went wrong, the balance is actually ${account.amount}")
    }
  }

  def main(args: Array[String]): Unit = {
    inceptionThreads(20, 0).start()
  }

}
