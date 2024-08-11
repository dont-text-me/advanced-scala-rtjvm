package com.ivanb.practice

import scala.annotation.{tailrec, targetName}

abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  @targetName("prepend")
  def #::(element: A): LzList[A]
  @targetName("concat")
  infix def ++(another: => LzList[A]): LzList[A]

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
  override infix def ++(another: => LzList[A]): LzList[A] = another

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
  override infix def ++(another: => LzList[A]): LzList[A] = new LzCons(head, tail ++ another)

  override def foreach(f: A => Unit): Unit =
    @tailrec
    def foreachTailrec(l: LzList[A]): Unit = if l.isEmpty then ()
    else
      f(l.head)
      foreachTailrec(l.tail)

    foreachTailrec(this)
  override def map[B](f: A => B): LzList[B] = new LzCons(f(head), tail.map(f))

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

  override def toList: List[A] = {
    @tailrec
    def toListTailRec(list: LzList[A], acc: List[A]): List[A] = list match {
      case l if l.isEmpty => acc.reverse
      case l              => toListTailRec(l.tail, l.head :: acc)
    }
    toListTailRec(this, List())
  }

  override lazy val head: A = hd

  override lazy val tail: LzList[A] = tl
}

object LzList {
  def generate[A](start: A)(generator: A => A): LzList[A] = new LzCons[A](
    start,
    LzList.generate(generator(start))(generator)
  )
  def from[A](list: List[A]): LzList[A] = list.foldRight[LzList[A]](LZEmpty[A]())(_ #:: _)

  def apply[A](values: A*) = LzList.from(values.toList)
}

object LzListPlayground {
  def main(args: Array[String]): Unit = {
    val nats = LzList.generate(0)(_ + 1)
    println(nats.head)
    println(nats.tail.head)

    val first50k = nats.take(50000)
    val first50kList = first50k.toList

//    println(nats.map(_*2).takeAsList(10))
//    println(nats.flatMap(x => LzList(x, x + 1)).takeAsList(10))
//    println(nats.filter( _ < 10).takeAsList(9))

    val combinationsLazy: LzList[String] = for {
      number <- LzList(1, 2, 3)
      string <- LzList("Black", "White")
    } yield s"$number-$string"

    val nums = LzList.generate(0)(_ + 1)
    val numsInc = LzList.generate(1)(_ + 1)

    def fibs: LzList[BigInt] =
      def fib(first: BigInt, second: BigInt): LzList[BigInt] =
        new LzCons[BigInt](first, fib(second, first + second))
      fib(1, 2)

    println(fibs.takeAsList(10))

    def isPrime(x: Int): Boolean = x match
      case 0 => false
      case 1 => true
      case 2 => true
      case 3 => true
      case _ => (2 to math.ceil(math.sqrt(x)).toInt).toList.forall(it => x % it != 0)

    val primes = nats.filter(isPrime).takeAsList(20)
    println(primes)

    def primesErastosthenes: LzList[Int] =
      @tailrec
      def sieve(numbers: LzList[Int]): LzList[Int] = numbers match
        case xs if xs.isEmpty        => xs
        case xs if !isPrime(xs.head) => sieve(xs.tail)
        case xs                      => new LzCons(xs.head, xs.tail.filter(_ % xs.head != 0))

      sieve(LzList.generate(2)(_ + 1))

    println(primesErastosthenes.takeAsList(20))
  }
}
