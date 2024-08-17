package com.ivanb.part5ts

object LiteralUnionIntersectionTypes {

  val aNumber = 3
  val three: 3 = 3

  def passNumber(n: Int): Unit = println(n)
  passNumber(45)
  passNumber(three) // ok, 3 <: Int

  def passStrict(n: 3): Unit = println(n)
  // passStrict(45) // not ok
  passStrict(three)

  val pi: 3.14 = 3.14
  val truth: true = true
  val faveLang: "Scala" = "Scala"

  // union types

  val truthOr42: Boolean | Int = 43

  def ambivalentMethod(arg: String | Int): String = arg match
    case _: String => "A string"
    case _: Int    => "A number"

  // type inference chooses the lowest common ancestor (Any) of the 2 types instead of String | Int
  val stringOrInt = if (43 > 0) "a string" else 45
  val stringOrInt2: String | Int = if (43 > 0) "a string" else 45

  type Maybe[T] = T | Null // Null type not null value

  def handleMaybe(someValue: Maybe[String]): Int = someValue match
    case null      => 0
    case x: String => x.length // flow typing

  type ErrorOr[T] = T | "error"
//  def handleREsource(arg: ErrorOr[Int]): Unit =
//    if arg != "error" then println(arg + 1) // flow typing doesnt work here
//    else println("Error!")

  // 3 - intersection types

  class Animal
  trait Carnivore
  class Crocodile extends Animal with Carnivore

  val carnivoreAnimal: Animal & Carnivore = new Crocodile // ok

  trait Gadget:
    def use(): Unit

  trait Camera extends Gadget:
    def takePicture() = println("smile!")
    override def use() = println("snap")

  trait Phone extends Gadget:
    def makeCall() = println("calling...")
    override def use() = println("ring")

  def useSmartphone(sp: Camera & Phone): Unit =
    sp.takePicture()
    sp.makeCall()
    sp.use() // Phone::Use, the rightmost type is used

  class SmartPhone extends Camera with Phone // diamond problem
  // intersection types + covariance
  trait HostConfig
  trait HostController:
    def get: Option[HostConfig]

  trait PortConfig
  trait PortController:
    def get: Option[PortConfig]

  def getConfigs(
      controller: HostController & PortController
  ): Option[
    PortConfig & HostConfig
  ] = // eqivalent to Option[PortConfig] & Option[HostConfig] (because Option is covariant)
    controller.get

  def main(args: Array[String]): Unit = {
    val maybeString: Maybe[String] = "Hello"
    val maybeString2: Maybe[String] = null
    println(handleMaybe(maybeString))
    println(handleMaybe(maybeString2))

    useSmartphone(new SmartPhone)
  }

}
