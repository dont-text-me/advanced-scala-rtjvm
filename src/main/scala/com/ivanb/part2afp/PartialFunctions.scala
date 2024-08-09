package com.ivanb.part2afp

object PartialFunctions {

  val aFunction: Int => Int = x => x + 1

  val otherFunction = (x: Int) => x match
    case 1 => 32
    case 2 => 56
    case 5 => 999

  val aPartialFunction: PartialFunction[Int, Int] =
    case 1 => 32
    case 2 => 56
    case 5 => 999

  val calCallOn37 = aPartialFunction.isDefinedAt(37)

  val liftedPF = aPartialFunction.lift
  // is now a total function returning an option, returns None for items outside the original function's domain
  val anotherPF: PartialFunction[Int, Int] =
    case 45 => 86

  val pfChain = aPartialFunction.orElse[Int, Int](anotherPF)

  case class Person(name: String, age: Int)
  
  val somePeople = List(
    Person("Alice", 24),
    Person("Bob", 20),
    Person("Claire", 19)
  )

  val ageIncr = somePeople.map {
    case Person(name, age) => Person(name, age + 1) // can use partial functions to deconstruct lists of objects
  }

  def main(args: Array[String]): Unit = {
    println(aPartialFunction(2))
    println(aPartialFunction(33))
  }
}
