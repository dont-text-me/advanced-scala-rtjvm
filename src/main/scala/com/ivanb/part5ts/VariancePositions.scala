package com.ivanb.part5ts

object VariancePositions {

  class Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  class Cage[A <: Animal] // A must be a subtype of animal
  // val aCage = new Cage[String] // not OK, string is not a subtype of animal
  val aCage = new Cage[Dog]

  class WeirdContainer[A >: Animal] // A must be a supertype of animal

  // variance positions

  // class Vet[-T](val favouriteAnimal: T) doesnt work, type of val fields are in COVARIANT position

  /*
   * val garfield = new Cat
    val theVet : Vet[Animal] = new Vet[Animal](garfield)
    val aDogVet: Vet[Dog] = theVet // ok, theVet is Vet[Animal]
    val aDog: Dog = aDogVet.favouriteAnimal // must be a Dog - type confilct
   * */
  // types of var fields are in COVARIANT AND CONTRAVARIANT positions
  // class MutableOption[+T](var contents: T) doesnt work

  // types of method arguments are in CONTRAVARIANT position
  //  class MyList[+T]:
  //    def add(element: T): MyList[T] = ???

  class Vet[-T]:
    def heal(animal: T): Boolean = true // ok, T is only consumed

  // method return types are in COVARIANT position
  //  abstract class Vet2[-T]:
  //    def rescueAnimal(): T

  // solving vairance positions problems

  abstract class LList[+A]:
    def head: A
    def tail: LList[A]
    def add[B >: A](element: B): LList[B] // widening the type

  class Vehicle
  class Car extends Vehicle
  class Supercar extends Car
  class RepairShop[-A <: Vehicle]:
    def repair[B <: A](vehicle: B): B = vehicle // narrowing the type

  val myRepairShop: RepairShop[Car] = new RepairShop[Vehicle]
  val myCar = new Car
  val repairedCar = myRepairShop.repair(myCar) // ok, returns a car

  val mySuperCar = new Supercar
  val repairedSuperCar = myRepairShop.repair(mySuperCar)

}
