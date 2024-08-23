package com.ivanb.part5ts

object AdvancedInheritance {

  // composite types can be used on their own
  trait Writer[T]:
    def write(value: T): Unit

  trait Stream[T]:
    def foreach(f: T => Unit): Unit

  trait Closeable:
    def close(status: Int): Unit

  def processStream[T](stream: Writer[T] & Stream[T] & Closeable): Unit =
    stream.foreach(println)
    stream.close(0)

  // diamond problem

  trait Animal:
    def name: String

  trait Lion extends Animal:
    override def name: String = "Lion"

  trait Tiger extends Animal:
    override def name: String = "Tiger"

  class Liger extends Tiger with Lion
  // class Liger extends Animal with {override def name = "Tiger"}
  // with {override def name = "Lion"} - last override always gets picked

  def demoLiger(): Unit = {
    val liger = new Liger
    println(liger.name)
  }

  // 3 - the super problem

  trait Cold:
    def print() = println("cold")

  trait Green extends Cold:
    override def print(): Unit =
      println("green")
      super.print()

  trait Blue extends Cold:
    override def print(): Unit =
      println("blue")
      super.print()

  class Red:
    def print() = println("red")

  class White extends Red with Green with Blue:
    override def print(): Unit =
      println("white")
      super.print()

  def demoColorInheritance(): Unit =
    val white = new White
    white.print()

  def main(args: Array[String]): Unit = {
    demoLiger()
    demoColorInheritance()
  }

}
