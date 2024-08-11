package com.ivanb.practice

import scala.annotation.targetName

abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  @targetName("prepend")
  def #::(element: A): LzList[A]
  @targetName("concat")
  def ++(another: LzList[A]): LzList[A]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A]
  def takeAsList(n: Int): List[A]
  def toList: List[A] // potentially undefined for infinite/very large lists
}

case class LZEmpty[A]() extends LzList[A] {

  override def isEmpty: Boolean = true

  override def head: A = throw new NoSuchElementException()

  override def tail: LzList[A] = throw new NoSuchElementException()

  @targetName("prepend")
  override def #::(element: A): LzList[A] = LzCons[A](element, this)

  @targetName("concat")
  override def ++(another: LzList[A]): LzList[A] = another

  override def foreach(f: A => Unit): Unit = ()

  override def map[B](f: A => B): LzList[B] = LZEmpty()

  override def flatMap[B](f: A => LzList[B]): LzList[B] = LZEmpty()

  override def filter(predicate: A => Boolean): LzList[A] = LZEmpty()

  override def take(n: Int): LzList[A] = this

  override def takeAsList(n: Int): List[A] = List()

  override def toList: List[A] = List()
}

class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {

  override def isEmpty: Boolean = false

  @targetName("prepend")
  override def #::(element: A): LzList[A] = new LzCons(element, this)

  @targetName("concat")
  override def ++(another: LzList[A]): LzList[A] = new LzCons(head, tail ++ another)

  override def foreach(f: A => Unit): Unit =
    f(head)
    tail.foreach(f)

  override def map[B](f: A => B): LzList[B] = f(head) #:: tail.map(f)

  override def flatMap[B](f: A => LzList[B]): LzList[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): LzList[A] =
    if predicate(head) then new LzCons(head, tail.filter(predicate)) // lazy evaluation is preserved
    else tail.filter(predicate)

  override def take(n: Int): LzList[A] = n match
    case 0 => LZEmpty()
    case x => new LzCons(head, tail.take(x - 1))

  override def takeAsList(n: Int): List[A] = n match
    case 0 => List()
    case x => head :: tail.takeAsList(n - 1)

  override def toList: List[A] = head :: tail.toList

  override lazy val head: A = hd

  override lazy val tail: LzList[A] = tl
}

object LzList {
  def generate[A](start: A)(generator: A => A): LzList[A] = new LzCons[A](
    start,
    LzList.generate(generator(start))(generator)
  )
  def from[A](list: List[A]): LzList[A] = list.foldRight[LzList[A]](LZEmpty[A]())(_ #:: _)
}

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val natsGen = LzList.generate(1)(n => n + 1).filter(_ % 2 == 0).takeAsList(1)
    val natsFrom = LzList.from((1 to 30).toList).map(_ + 1).take(5).toList
    println(natsGen)
  }
}
