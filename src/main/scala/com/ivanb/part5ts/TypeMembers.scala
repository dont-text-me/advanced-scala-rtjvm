package com.ivanb.part5ts

object TypeMembers {
  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollective:
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal // abstract type member with a type bound
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalAlias = Cat // type Alias
    type NestedOption = List[Option[Option[Int]]]

  val ac = new AnimalCollective // every instance has its own type definition
  val anAnimal: ac.AnimalType = ???

  // val cat: ac.BoundedAnimal = new Cat // not ok, from the compiler POV, BoundedAnimal might be Dog
  val aDog: ac.SuperBoundedAnimal = new Dog // ok
  val aCat: ac.AnimalAlias = new Cat // ok

  // establish relationships between types
  // alternative to generics

  class LList[T]:
    def add(element: T): LList[T] = ???
  class MyList:
    type T
    def add(element: T): MyList = ???

  // .type
  type CatType = aCat.type

  class MoreConcreteAnimalColective extends AnimalCollective:
    override type AnimalType = Dog

}
