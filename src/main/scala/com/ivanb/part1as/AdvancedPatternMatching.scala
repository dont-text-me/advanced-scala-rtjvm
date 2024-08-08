package com.ivanb.part1as

object AdvancedPatternMatching {

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      if (age < 21) Some("Minor") else Some("Adult")
  }

  val ivan = new Person("ivan", 23)
  val ivanPM = ivan match { // Person.unapply(ivan)
    case Person(n, a) => s"Hi there, i'm $n"
  }

  val legalStatus = ivan.age match {
    case Person(status) => s"The legal drinking status is $status"
  }

  object Even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object Odd {
    def unapply(arg: Int): Boolean = arg % 2 != 0
  }

  object SingleDigits {
    def unapply(arg: Int): Boolean = arg.abs < 10
  }

  val n: Int = 55

  val mathProperty = n match {
    case Even() => "Even"
    case Odd() => "Odd"
    case SingleDigits() => "Single digit"
    case _ => "No special property"
  }

  infix case class Or[A, B](a: A, b: B)

  val anEither = Or(2, "two")

  val humanDescription = anEither match {
    case number Or string => s"$number is written as $string"
  }

  val aList = List(1, 2, 3)
  val listPM = aList match {
    case 1 :: rest => "Starts with 1"
    case _ => "Doesnt start with 1"
  }

  val vararg = aList match {
    case List(1, _*) => "List starting with 1"
    case _ => "Some other list"
  }

  abstract class MyList[A] {
    def head: A = throw new NoSuchElementException()

    def tail: MyList[A] = throw new NoSuchElementException()
  }

  case class Empty[A]() extends MyList[A]

  case class Cons[A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty())))

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty()) Some(Seq.empty)
      else unapplySeq(list.tail).map(restOfSeq => list.head +: restOfSeq)
  }

  val varargCustom = myList match {
    case MyList(1, _*) => "List starting with 1"
    case _ => "some other list"
  }

  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T //needs to be present for pattern matching, checked via reflection
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = person.name
    }
  }

  val weirdPersonPM = ivan match{
    case PersonWrapper(name) => s"The name is $name"
  }

  def main(args: Array[String]): Unit = {
    println(ivanPM)
    println(legalStatus)
    println(mathProperty)
  }

}
