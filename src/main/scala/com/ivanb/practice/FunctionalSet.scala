package com.ivanb.practice

import scala.annotation.targetName


abstract class FSet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean

  def apply(elem: A): Boolean = contains(elem)


  infix def +(elem: A): FSet[A]

  infix def ++(anotherSet: FSet[A]): FSet[A]

  def map[B](f: A => B): FSet[B]

  def flatMap[B](f: A => FSet[B]): FSet[B]

  def filter(predicate: A => Boolean): FSet[A]

  def foreach(f: A => Unit): Unit

  @targetName("remove") def -(elem: A): FSet[A]

  @targetName("diff") infix def --(anotherSet: FSet[A]): FSet[A]

  @targetName("intersect") infix def &(anotherSet: FSet[A]): FSet[A]

  def toList: List[A]

  // negation : all elements of type A except the elements in this set
  def unary_! : FSet[A] = new PBSet[A](x => !contains(x))
}

/** Property based set */
class PBSet[A](property: A => Boolean) extends FSet[A] {

  override def contains(elem: A): Boolean = property(elem)

  override infix def +(elem: A): FSet[A] = new PBSet[A](x => property(x) || x == elem)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = new PBSet[A](elem => property(elem) || anotherSet(elem))

  override def map[B](f: A => B): FSet[B] = failWithMessage

  override def flatMap[B](f: A => FSet[B]): FSet[B] = failWithMessage

  override def filter(predicate: A => Boolean): FSet[A] = new PBSet[A](elem => property(elem) && predicate(elem))

  override def foreach(f: A => Unit): Unit = failWithMessage

  @targetName("remove")
  override def -(elem: A): FSet[A] = filter(x => x != elem)

  @targetName("diff")
  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(x => !anotherSet(x))

  @targetName("intersect")
  override infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)

  override def toList: List[A] = ???

  override def unary_! : FSet[A] = new PBSet[A](x => !property(x))

  private def failWithMessage = throw new RuntimeException("Not sure if the set is iterable")
}


case class Empty[A]() extends FSet[A] {
  override def filter(predicate: A => Boolean): FSet[A] = this

  override def contains(elem: A): Boolean = false

  override infix def +(elem: A): FSet[A] = Cons(elem, this)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet

  override def map[B](f: A => B): FSet[B] = Empty()

  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()

  override def foreach(f: A => Unit): Unit = ()

  override def toList: List[A] = List()

  @targetName("remove")
  override def -(elem: A): FSet[A] = this

  @targetName("diff")
  override infix def --(anotherSet: FSet[A]): FSet[A] = this

  @targetName("intersect")
  override infix def &(anotherSet: FSet[A]): FSet[A] = this
}

case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {

  override def contains(elem: A): Boolean = if (head == elem) true else tail.contains(elem)

  override infix def +(elem: A): FSet[A] = if contains(elem) then this else Cons(elem, this)

  override infix def ++(anotherSet: FSet[A]): FSet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => FSet[B]): FSet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): FSet[A] = if predicate(head) then tail.filter(predicate) + head else tail.filter(predicate)


  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override def toList: List[A] = head :: tail.toList

  @targetName("remove")
  override def -(elem: A): FSet[A] = if head == elem then tail else tail - elem + head

  @targetName("diff")
  override infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)

  @targetName("intersect")
  override infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)
}

object FSet {
  def apply[A](values: A*): FSet[A] = values.foldRight[FSet[A]](Empty())((x, acc) => acc + x)
}

object FunctionalSet {

  def main(args: Array[String]): Unit = {
    val numbers = FSet(1, 2, 3, 4, 5)
    println(numbers.contains(5))
    println(numbers(6))
    println((numbers + 10).contains(10))
    println(numbers.map(_ * 2).contains(10))
    println(numbers.map(_ % 2).contains(1))
    println(numbers.flatMap(x => FSet(x, x + 1)).contains(7))
    println(numbers.flatMap(x => FSet(x, x + 1)).toList)

    println((FSet(1, 2, 3) -- FSet(2, 3, 4)).toList)

    val nats = new PBSet[Int](_ => true) // set of all integers

    println(!nats.contains(0))
    println((!nats + 1 + 2 + 3).contains(3))
  }

}
