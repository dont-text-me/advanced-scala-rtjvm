package com.ivanb.part5ts
import scala.reflect.Selectable.reflectiveSelectable
object StructuralTypes {
  // compile-time duck typing
  type SoundMaker = { // structural type
    def makeSound(): Unit
  }

  class Dog:
    def makeSound(): Unit = println("bark")

  class Car:
    def makeSound(): Unit = println("beep")

  val dog: SoundMaker = new Dog // ok, dog has AT LEAST the structure of SoundMaker
  val car: SoundMaker = new Car
  // duck typing

  // type refinements

  abstract class Animal:
    def eat(): String

  type WalkingAnimal = Animal {
    def walk(): Int // refined type, extends Animal with a structural type
  }

  type JavaCloseable = java.io.Closeable
  class CustomCloseable:
    def close(): Unit = println("closing...")
    def closeSilently(): Unit = println("closing quietly....")

//  def closeResource(closeable: JavaCloseable | CustomCloseable): Unit =
//    closeable.close() // not ok

  type UnifiedCloseable = {
    def close(): Unit
  }

  def closeResources(closeable: UnifiedCloseable): Unit = closeable.close()

  val javaCloseable = new JavaCloseable {
    override def close(): Unit = println("closing java resource")
  }
  val customCloseable = new CustomCloseable {
    override def close(): Unit = println("closing custom resource")
  }

  def closeResourceV2(closeable: { def close(): Unit }): Unit = closeable.close()

  def main(args: Array[String]): Unit = {
    dog.makeSound() // done via reflection - slow
    car.makeSound()

    closeResources(javaCloseable)
    closeResources(customCloseable)
  }

}
