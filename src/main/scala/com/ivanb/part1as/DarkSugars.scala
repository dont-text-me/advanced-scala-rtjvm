package com.ivanb
package part1as

import scala.util.Try

object DarkSugars {

  def singleArgMethod(arg: Int): Int = arg + 1

  val aMethodCall = singleArgMethod {
    //some code
    42
  }

  val aTryInstance = Try {
    throw new RuntimeException()
  }

  val anIncrementedList = List(1, 2, 3).map { x =>
    x + 1
  }

  // single abstract member pattern
  trait Action {
    def act(x: Int): Int
  }

  val anAction = new Action:
    override def act(x: Int): Int = x + 1

  val anotherAction: Action = (x: Int) => x + 1

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Printing from another thread")
  })

  val anotherThread = new Thread(() => print("Also printing from another thread"))


  // methods ending in ':'

  val aList = List(1, 2, 3)
  val aPrependedList = 0 :: aList // :: is a method on the list type, not the number type

  // methods ending in a : are *Right associative*

  val aBigList = 0 :: 1 :: 2 :: List(3, 4) // rewritten to List(3, 4).::(2)... by the compiler

  class MyStream[T] {
    infix def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  class Talker(name: String) {
    infix def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val person = new Talker("John")
  val statement = person `and then said` "I love scala"

  object `Content-Type` {
    val `application-json` = "application/JSON"
  }

  // infix types
  import scala.annotation.targetName
  @targetName("Arrow") infix class -->[A, B]

  val compositeType: -->[Int, String] = new-->[Int, String]
  val alsoCompositeType: Int --> String = new-->[Int, String]

  val myArray = Array(1, 2, 3, 4)
  myArray.update(2, 45)
  myArray(2) = 45

  // mutable fields

  class Mutable{
    private var internalMember: Int = 0
    def member: Int = internalMember //getter
    def member_=(value: Int): Unit = internalMember = value // setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42

  // varargs
  def methodWithVarargs(args: Int*): Int = args.sum

  val anotherList = List(1, 2, 3, 4)
  val callWithDynamicArgs = methodWithVarargs(anotherList*)

  def main(args: Array[String]): Unit = {

  }

}
