package com.ivanb.part5ts

object Variance {
  class Animal
  class Dog(name: String) extends Animal

  // if Dog extends Animal, should List[Dog] extend List[Animal]?

  // for List, yes - List is COVARIANT

  val lassie = new Dog("Lassie")
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")
  val anAnimal: Animal = lassie // ok - Dog <: Animal
  val myDogs: List[Animal] = List(lassie, hachi, laika)

  class MyList[+A] // MyList is COVARIANT in A, relationship between List[A] and List[B] is the same as between A and B
  val aListOfAnimals: MyList[Animal] = new MyList[Dog]

  // if the answer is no, the type is INVARIANT
  trait Semigroup[A]: // no marker = INVARIANT
    def combine(x: A, y: A): A

  // java generics are invariant
  // val javaList : util.ArrayList[Animal] = new util.ArrayList[Dog]() - type mismatch

  // if the answer is "absolutely not", the answer is CONTRAVARIANT

  trait Vet[-A]: // contravariant
    def heal(animal: A): Boolean

  // if Dog <: Animal, then Vet[Animal] <: Vet[Dog] i.e. opposite of covariance
  val myVet: Vet[Dog] = new Vet[Animal]:
    override def heal(animal: Animal): Boolean =
      println("Healing...")
      true // ok

  val healLaika = myVet.heal(laika) // ok

  // rules of thumb:
  // if the type PRODUCES or RETRIEVES a value (e.g. list), then it should be COVARIANT
  // if the type ACTS ON or CONSUMES a value (e.g. a vet), then it should be CONTRAVARIANT
  // otherwise, INVARIANT

  class RandomGenerator[+A]
  class MyOption[+A]
  class JSONSerializer[-A]
  trait MyFunction[-A, +B]

  abstract class LList[+A]:
    def head: A
    def tail: LList[A]

  case object EmptyList extends LList[Nothing]: // Nothing <: A therefore LList[Nothing] <: LList[A]
    override def head: Nothing = throw new NoSuchElementException()
    override def tail: Nothing = throw new NoSuchElementException()

  case class Cons[+A](override val head: A, override val tail: LList[A]) extends LList[A]

  val aList: LList[Int] = EmptyList
  val otherList: LList[String] = EmptyList

  def main(args: Array[String]): Unit = {}

}
