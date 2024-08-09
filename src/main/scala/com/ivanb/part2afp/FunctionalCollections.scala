package com.ivanb.part2afp

object FunctionalCollections {
  val aSet: Set[String] = Set("I", "Love", "Scala")
  val setContainsScala = aSet("Scala")

  // sequences are functions of type PartialFunction[Int, A] index => element
  // (not defined for indexes beyond the seq size)
  val aSeq: Seq[Int] = Seq(1, 2, 3, 4)
  val anElement = aSeq(2)

  // Map[K, V] extends PartialFunction[K, V]
  val aPhoneBook = Map(
    "Alice" -> 12345,
    "Bob" -> 56789
  )

  val alicesPhone = aPhoneBook("Alice")

}
