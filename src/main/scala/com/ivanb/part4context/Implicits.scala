package com.ivanb.part4context

import scala.language.implicitConversions

object Implicits {
  trait Semigroup[A]:
    def combine(x: A, y: A): A

  def combineAll[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
    list.reduce(semigroup.combine)

  implicit val intSemiGroup: Semigroup[Int] = (x, y) => x + y

  val sumOf10 = combineAll((1 to 10).toList)

  // scala 2 -> scala 3:
  //  implicit arg -> using
  //  implicit val -> given
  //  implicit class -> extension

  extension (number: Int) def isEven = number % 2 == 0

  val is23Even = 23.isEven

  implicit class MyRichInteger(number: Int):
    def isEven2 = number % 2 == 0

  val is23Even2 = 23.isEven2 // compiler converts Int to MyRichInteger

  case class Person(name: String):
    def greet(): String = s"Hi, i am $name"

  implicit def string2Person(x: String): Person = Person(x) // implicit conversion (dangerous)
  // implicit def => synthesize new implicit values
  implicit def semigroupOfOption[A](implicit semigroup: Semigroup[A]): Semigroup[Option[A]] =
    (x, y) => {
      for {
        vx <- x
        vy <- y
      } yield semigroup.combine(vx, vy)
    }
  def main(args: Array[String]): Unit = {
    println(sumOf10)
    println(combineAll(List[Option[Int]](Some(2), Some(3))))
  }

}
