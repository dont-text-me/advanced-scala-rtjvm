package com.ivanb.part2afp

object LazyEval {

  lazy val x: Int = {
    println("Hello")
    42
  } // delays evaluation until it is used i.e. hello will not be printed

  def byNameMethod(n: => Int): Int = n + n + n + 1

  def retrieveMagicValue() = {
    println("Waiting...")
    Thread.sleep(1000)
    42
  }

  def demoByName(): Unit = {
    println(
      byNameMethod(retrieveMagicValue())
    ) // will wait 3 times, retrieve method is called every time
  }

  def byNeedMethod(n: => Int): Int = {
    lazy val lazyN = n // memoization - result of the computation is saved in the lazy value
    lazyN + lazyN + lazyN + 1
  }

  def demoByNeed() = {
    println(byNeedMethod(retrieveMagicValue()))
  }

  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)

  def demoFilter(): Unit = {
    val lt30 = numbers.filter(lessThan30)
    val gt20 = lt30.filter(greaterThan20)
    println(gt20)
  }

  def demoWithFilter(): Unit = {
    val lt30 = numbers.withFilter(lessThan30)
    val gt20 = lt30.withFilter(greaterThan20)
    println(gt20.map(identity))
  }

  def demoForComp(): Unit = {
    val forComp = for {
      n <- numbers if lessThan30(n) && greaterThan20(n)
    } yield n

    println(forComp)
  }
  def main(args: Array[String]): Unit = {
//    println(x)
//    println(x) // hello won't be printed, x is bound to the computed value
//
//    demoByName()
//
//    demoByNeed()
    demoFilter()
    demoWithFilter() // instead of filtering the first list THEN filtering the results, the whole list is tested against each predicate

    demoForComp() // same order as demoWithFilter()
  }

}
