package com.ivanb.part2afp

import scala.annotation.targetName

object Monads {

  def listStory(): Unit = {
    val aList = List(1, 2, 3)
    val listMultiply = for {
      x <- List(1, 2, 3)
      y <- List(4, 5, 6)
    } yield x * y

    val listMultiply2 = List(1, 2, 3).flatMap(x =>
      List(4, 5, 6).map(_ * x)
    ) // for comprehensions are synthax sugar for this

    val f = (x: Int) => List(x, x + 1)
    val g = (x: Int) => List(x, 2 * x)
    val pure = (x: Int) => List(x)

    // prop 1 : left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every x, for every f

    // prop 2 : right identity
    val rightIdentity = aList.flatMap(pure) == aList

    // prop 3 : associativity
    val associativity = aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))
  }

  def optionStory(): Unit = {
    val anOption = Option(42)
    val optionString = for {
      lang <- Option("Scala")
      ver <- Option(3)
    } yield s"$lang-$ver"

    val optionString2 = Option("Scala").flatMap(lang => Option(3).map(ver => s"$lang-$ver"))

    val f = (x: Int) => Option(x + 1)
    val g = (x: Int) => Option(2 * x)
    val pure = (x: Int) => Option(x)

    // prop 1 : left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every x, for every f

    // prop 2 : right identity
    val rightIdentity = anOption.flatMap(pure) == anOption

    // prop 3 : associativity
    val associativity = anOption.flatMap(f).flatMap(g) == anOption.flatMap(x => f(x).flatMap(g))
  }

  /** Is this a monad? Yes! This **describes** a computation without performing it
    */
  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] =
      new IO(() => f(unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO(() => f(unsafeRun()).unsafeRun())
  }

  object IO {
    @targetName("pure")
    def apply[A](value: => A): IO[A] = new IO(() => value)
  }

  def possibleMonadStory(): Unit = {
    val aMonadInstance = IO(42)

    val forComp = for {
      lang <- IO("Scala")
      ver <- IO(3)
    } yield s"$lang-$ver"

    val sameAsForComp =
      IO("Scala").flatMap(lang => IO(3).map(ver => s"$lang-$ver"))

    val f = (x: Int) => IO(x + 1)
    val g = (x: Int) => IO(2 * x)
    val pure = (x: Int) => IO(x)

    // prop 1 : left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every x, for every f

    // prop 2 : right identity
    val rightIdentity = aMonadInstance.flatMap(pure) == aMonadInstance

    // prop 3 : associativity
    val associativity =
      aMonadInstance.flatMap(f).flatMap(g) == aMonadInstance.flatMap(x => f(x).flatMap(g))

    println(leftIdentity)
    println(rightIdentity)
    println(associativity)
    // all three return false because the monad wraps a function.
    // Comparing functions is impossible (always false), therefore, all 3 are false negatives

    val leftIdentity2 = pure(42).flatMap(f).unsafeRun() == f(42).unsafeRun()
    val rightIdentity2 = aMonadInstance.flatMap(pure).unsafeRun() == aMonadInstance.unsafeRun()
    val associativity2 = aMonadInstance.flatMap(f).flatMap(g).unsafeRun() == aMonadInstance
      .flatMap(x => f(x).flatMap(g))
      .unsafeRun()

    println(leftIdentity2)
    println(rightIdentity2)
    println(associativity2)
    // all 3 are true, the actual test.
    val fs = (x: Int) =>
      IO {
        println("incrementing")
        x + 1
      }
    val gs = (x: Int) =>
      IO {
        println("doubling")
        x * 2
      }

    val associativity3 = aMonadInstance.flatMap(fs).flatMap(gs).unsafeRun() == aMonadInstance
      .flatMap(x => fs(x).flatMap(gs))
      .unsafeRun()
  }

  def possiblyMonadExample(): Unit = {
    val aPossiblyMonad = IO {
      println(
        "Printing stuff"
      ) // NOTE: this will not be ran. The monad describes the computation without performing it
      // more computations
      42
    }

    val anotherPM = IO {
      println("Printing stuff here too")
      // more computations
      "Scala"
    }

    val aForComp = for {
      num <- aPossiblyMonad
      lang <- anotherPM
    } yield s"$num-$lang" // this also doesnt perform any computation, just describes a combination of the two computations

  }

  def main(args: Array[String]): Unit = {
    possibleMonadStory()
  }
}
