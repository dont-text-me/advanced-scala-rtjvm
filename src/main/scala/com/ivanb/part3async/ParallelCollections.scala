package com.ivanb.part3async

import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.parallel.immutable.ParVector

object ParallelCollections {

  val aList = (1 to 1000000).toList
  val incrList = aList.map(_ + 1) // happens sequentially

  val parList: ParSeq[Int] = aList.par
  val parIncList = parList.map(_ + 1) // happens in parallel

  // The following can be parallelised: Seq, Vector, Array, Map, Set

  val aParVector = ParVector[Int](1, 2, 3, 4, 5, 6)

  def measure[A](expression: => A): Long = {
    val time = System.currentTimeMillis()
    expression // force evaluation
    System.currentTimeMillis() - time
  }

  def compareListTransformation(): Unit = {
    val list = (1 to 10000000).toList
    println("created a list")

    val serialTime = measure(list.map(_ + 1))
    println(s"Serial time: $serialTime")

    val parallelTime = measure(list.par.map(_ + 1))
    println(s"Parallel time: $parallelTime")
  }

  def demoUndefinedOrder(): Unit = {
    val aList = (1 to 1000).toList
    val reduction = aList.reduce(_ - _) // result depends on the order of the elements
    val parallelReduction =
      aList.par.reduce(_ - _) // order of operations is undefined, returns different results

    println(s"Sequential reduction: $reduction")
    println(s"Parallel reduction: $parallelReduction")
  }

  // for associative operations, result is deterministic
  def demoDefinedOrder(): Unit = {
    val aList = "One two three four five six".split(" ").toList
    val reduction = aList.reduce(_ + " " + _) // result depends on the order of the elements
    val parallelReduction = aList.par.reduce(_ + " " + _) // order of operations is undefined

    println(s"Sequential reduction: $reduction")
    println(s"Parallel reduction: $parallelReduction")
  }

  def demoRaceConditions(): Unit = {
    var sum = 0
    (1 to 1000).toList.par.foreach(sum += _)
    println(sum) // expected value is 500500
  }

  def main(args: Array[String]): Unit = {
    demoRaceConditions()
  }

}
