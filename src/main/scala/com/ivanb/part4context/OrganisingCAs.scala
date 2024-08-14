package com.ivanb.part4context

object OrganisingCAs {

  // compiler fetches givens/extensions (in this order of priority, stops searching as soon as something is found)
  // 1 - local scope
  given reverseOrdering: Ordering[Int] = (x: Int, y: Int) => y - x

  // --------------------------------------------------------------

  // 2 - imported scope
  case class Person(name: String, age: Int)
  val people = List(
    Person("Alice", 29),
    Person("Bob", 32),
    Person("Dan", 11),
    Person("Claire", 65)
  )

  object PersonGivens {
    given ageOrdering: Ordering[Person] = (x: Person, y: Person) => y.age - x.age
  }
  // import explicitly
  //  import PersonGivens.ageOrdering

  // search for a given instead of referencing by name
  // import PersonGivens.given Ordering[Person]

  // import ALL givens
  import PersonGivens.given
  // NOTE: import * does not import givens
  val sortedPeople = people.sorted
  // --------------------------------------------------------------

  // 3 - companions of all types involved in method signature
  object Person {
    given byNameOrdering: Ordering[Person] = (x: Person, y: Person) => x.name compareTo y.name
  }
  val sortedPeople2 = people.sorted

  /** Exercise: create given instances for Ordering[Purchase]
    *
    *   - ordering by total price, descending = 50% of the code base
    *     - this is the "dominant" given, so it goes into the companion object
    *   - ordering by unit count, descending = 25% of the code base
    *     - goes into a different object to be imported when needed
    *   - ordering by unit price, ascending = 25% of the code base
    *     - same as above
    */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    extension (purchase: Purchase) def totalPrice: Double = purchase.unitPrice * purchase.nUnits

    given totalPriceOrdering: Ordering[Purchase] = (x: Purchase, y: Purchase) =>
      y.totalPrice compareTo x.totalPrice
  }

  object PurchaseCountOrdering {
    given unitCountOrdering: Ordering[Purchase] = (x: Purchase, y: Purchase) =>
      y.nUnits compareTo x.nUnits
  }

  object PurchaseUnitPriceOrdering {
    given unitPriceOrdering: Ordering[Purchase] = (x: Purchase, y: Purchase) =>
      x.unitPrice compareTo y.unitPrice
  }

  def main(args: Array[String]): Unit = {
    println(sortedPeople2)
  }

}
