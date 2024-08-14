package com.ivanb.part4context

import com.ivanb.part4context.ExtensionMethods.GroupedExtensions.{combineAll, ends}

object ExtensionMethods {

  case class Person(name: String):
    def greet: String = s"Hi, my name is $name"

  extension (string: String) def greetAsPerson: String = Person(string).greet

  trait Combinator[A]:
    def combine(x: A, y: A): A
    def combineAll(vals: A*): A

  val greeting = "Ivan".greetAsPerson
  val aList = List(1, 2, 3, 4)
  val (first, last) = aList.ends

  given intCombinator: Combinator[Int] with {

    override def combine(x: Int, y: Int): Int = x + y

    override def combineAll(vals: Int*): Int = vals.sum
  }

  val firstSum = aList.combineAll // available because a given combinator exists

  object GroupedExtensions {
    extension [A](list: List[A])
      def ends: (A, A) = (list.head, list.last)
      def combineAll(using combinator: Combinator[A]): A = list.reduce(combinator.combine)
  }

  val firstLast2 = ends(aList)

  /** Exercises
    *
    *   - \1. Add an isPrime method to the Int type
    *   - 2. Add extensions to Tree
    *     - map(f: A => B): Tree[B]
    *     - forall(p: A => Boolean): Boolean
    *     - sum => Sum of all elements of the tree (only for ints)
    */

  sealed abstract class Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  extension (int: Int)
    def isPrime: Boolean = int match {
      case 0 => false
      case 1 => true
      case 2 => true
      case 3 => true
      case n => (2 to math.ceil(math.sqrt(n)).toInt).forall(n % _ != 0)
    }

  extension [A](tree: Tree[A])
    def map[B](f: A => B): Tree[B] = tree match
      case Leaf(n)                => Leaf(f(n))
      case Branch(n, left, right) => Branch(f(n), left.map(f), right.map(f))

    def forAll(predicate: A => Boolean): Boolean = tree match
      case Leaf(n) => predicate(n)
      case Branch(n, left, right) =>
        predicate(n) && left.forAll(predicate) && right.forAll(predicate)

    def sum(using combinator: Combinator[A]): A = tree match
      case Leaf(n)                => n
      case Branch(n, left, right) => combinator.combineAll(n, left.sum, right.sum)

  def main(args: Array[String]): Unit = {
    val myTree: Tree[Int] = Branch(5, Branch(10, Leaf(3), Leaf(2)), Branch(21, Leaf(17), Leaf(9)))
    println(myTree.map(_ + 1))
  }
}
