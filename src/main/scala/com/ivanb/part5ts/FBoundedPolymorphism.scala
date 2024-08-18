package com.ivanb.part5ts

object FBoundedPolymorphism {
  object Problem:
    trait Animal:
      def breed: List[Animal]

    class Cat extends Animal:
      override def breed: List[Animal] = List(new Cat, new Dog /*<- problem*/ )

    class Dog extends Animal:
      override def breed: List[Animal] = List(new Dog, new Dog, new Dog)

    // losing type safety

  object NaiveSolution:
    trait Animal:
      def breed: List[Animal]

    class Cat extends Animal:
      override def breed: List[Cat] = List(new Cat, new Cat)

    class Dog extends Animal:
      override def breed: List[Dog] = List(new Dog, new Dog, new Dog)

    // must write the proper type signatures

  object FBP:
    trait Animal[A <: Animal[A]]: // recursive type a.k.a F-bounded polymorphism
      def breed: List[Animal[A]]

    class Cat extends Animal[Cat]:
      override def breed: List[Animal[Cat]] = List(new Cat, new Cat)

    class Dog extends Animal[Dog]:
      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)

    // example of FBP going wrong
    class Crocodile extends Animal[Dog]:
      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)

  // some ORM libraries use FBP
  trait Entity[E <: Entity[E]]

  // the Java sorting library uses FBP
  class Person extends Comparable[Person]:
    override def compareTo(o: Person): Int = ???

  // FBP + self types

  object FBPSelf:
    trait Animal[A <: Animal[A]]:
      self: A =>
      def breed: List[Animal[A]]

    class Cat extends Animal[Cat]:
      override def breed: List[Animal[Cat]] = List(new Cat, new Cat)

    class Dog extends Animal[Dog]:
      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)

    // does not compile, Crocodile !<: Dog
//    class Crocodile extends Animal[Dog]:
//      override def breed: List[Animal[Dog]] = List(new Dog, new Dog)

    trait Fish extends Animal[Fish]
    class Cod extends Fish:
      override def breed: List[Animal[Fish]] = List(new Cod, new Cod)

    class Shark extends Fish:
      override def breed: List[Animal[Fish]] = List(new Cod) // ok, compiler doesnt complain

    // solution lvl2
    trait FishV2[A <: FishV2[A]] extends Animal[FishV2[A]]:
      self: A =>

    class Tuna extends FishV2[Tuna]:
      override def breed: List[Animal[FishV2[Tuna]]] = List(new Tuna)

//    class Swordfish extends FishV2[Swordfish]:
//      override def breed: List[Animal[FishV2[Swordfish]]] = List(new Tuna) // not ok

}
