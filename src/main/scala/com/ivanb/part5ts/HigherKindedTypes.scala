package com.ivanb.part5ts

import scala.util.Try

object HigherKindedTypes {
  class HigherKindedType[F[_]] // higher-kinded type
  class HigherKindedType2[F[_], G[_], A]
  val hkExample = new HigherKindedType[List]
  val hkExample2 = new HigherKindedType2[List, Option, Int]

  // can use hkts for methods as well
  // useful for abstract libraries e.g. Cats
  // example: Functor

  val aList = List(1, 2, 3)
  val anOption = Option(2)
  val aTry = Try(2)

  val anIncrList = aList.map(_ + 1)
  val anIncrOption = anOption.map(_ + 1)
  val anIncrTry = aTry.map(_ + 1)

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  // etc...

  trait Functor[F[_]]:
    def map[A, B](fa: F[A])(f: A => B): F[B]

  given optionFunctor: Functor[Option] with
    override def map[A, B](option: Option[A])(f: A => B): Option[B] = option map f

  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  extension [F[_], A](container: F[A])(using functor: Functor[F])
    def map[B](f: A => B): F[B] = functor.map(container)(f)

  def do10xv2[F[_]: Functor /*a Functor[F] must be in scope*/ ](container: F[Int]): F[Int] =
    container.map(_ * 10)

  // Exercise -----------------------------------------------------------------------

  def combineList[A, B](listA: List[A], listB: List[B]): List[(A, B)] =
    for {
      a <- listA
      b <- listB
    } yield (a, b)

  def combineOption[A, B](optA: Option[A], optB: Option[B]): Option[(A, B)] =
    for {
      a <- optA
      b <- optB
    } yield (a, b)

  // etc...

  trait Monad[F[_]] extends Functor[F]:
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  given listFlatMapper: Monad[List] with
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f

  extension [F[_], A](container: F[A])(using fm: Monad[F])
    def flatMap[B](f: A => F[B]): F[B] = fm.flatMap(container)(f)

  def combineGeneral[F[_]: Monad, A, B](ca: F[A], cb: F[B]): F[(A, B)] =
    for {
      a <- ca
      b <- cb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(10)))
    println(combineGeneral(List(1, 2, 3), List("one", "two", "three")))

  }
}
