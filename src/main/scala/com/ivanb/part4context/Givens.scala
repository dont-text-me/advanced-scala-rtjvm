package com.ivanb.part4context

object Givens {

  val aList = List(4, 2, 3, 1)
  val anOrderedList = aList.sorted

  // given descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  // ^ signals to the compiler to automatically inject this into any place where an Ordering[Int] is required
  // val anInverseOrderedList = aList.sorted(descendingOrdering)

  case class Person(name: String, age: Int)

  val people = List(
    Person("Alice", 29),
    Person("Bob", 32),
    Person("Dan", 11),
    Person("Claire", 65)
  )
  given personOrdering: Ordering[Person] = (x: Person, y: Person) => x.name compareTo y.name

  val sortedPeople = people.sorted // (personOrdering)

  object PersonAltSyntax {
    given personOrdering: Ordering[Person] with {
      override def compare(x: Person, y: Person): Int = x.name compareTo y.name
    }
  }

  trait Combinator[A] {
    def combine(x: A, y: A): A
  }

  def combineAll[A](
      list: List[A]
  )(using combinator: Combinator[A]): A = // require the presence of a given combinator
    list.reduce(combinator.combine)

  given intCombinator: Combinator[Int] = (x: Int, y: Int) => x + y
  val firstSum = combineAll(List(1, 2, 3, 4))

  // context bound
  def combineInGroupsOf3[A](list: List[A])(using combinator: Combinator[A]): List[A] =
    list.grouped(3).map( /*combinator.*/ combineAll).toList

  def combineInGroupsOf3V2[A: Combinator](
      list: List[A]
  ): List[A] = // A : Combinator => there is a given Combinator[A] in scope
    list.grouped(3).map(combineAll).toList

  // synthesize new given instance based on existing ones
  given listOrdering(using intOrdering: Ordering[Int]): Ordering[List[Int]] with {
    override def compare(x: List[Int], y: List[Int]): Int = x.sum - y.sum
  }

  val listOfLists = List(List(1, 2), List(1, 1), List(3, 4, 5))
  val nestedListOrdered = listOfLists.sorted

  given listOrderingBasedOnCombinator[A](using
      ordering: Ordering[A]
  )(using combinator: Combinator[A]): Ordering[List[A]] = (x: List[A], y: List[A]) =>
    ordering.compare(combineAll(x), combineAll(y))

  // pass a regular value in place of a given

  val myCombinator: Combinator[Int] = (x: Int, y: Int) => x * y
  val listProduct = combineAll(List(1, 2, 3, 4))(using myCombinator)

  /** Exercises:
    *
    *   - 1 - create a given for ordering Option[A] if you can order A
    *
    *   - 2 - create a summoning method that fetches the given value of your particular
    */

  def fetchGiven[A](using aGiven: A): A = aGiven // implemented as summon in the stdlib

  given optionOrdering[A: Ordering]: Ordering[Option[A]] =
    (x: Option[A], y: Option[A]) =>
      (x, y) match {
        case (Some(_), None)          => 1
        case (None, None)             => 0
        case (None, Some(_))          => -1
        case (Some(val1), Some(val2)) => fetchGiven[Ordering[A]].compare(val1, val2)
      }

  val options = List(Option(1), Option(5), Option(-3), None)
  val sortedOptions = options.sorted

  def main(args: Array[String]): Unit = {
    println(anOrderedList)
    println(sortedPeople)
    println(firstSum)
    println(sortedOptions)
  }

}
