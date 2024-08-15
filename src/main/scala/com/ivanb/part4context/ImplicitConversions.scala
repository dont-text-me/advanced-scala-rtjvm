package com.ivanb.part4context
import scala.language.implicitConversions
object ImplicitConversions {
  case class Person(name: String) {
    def greet(): String = s"Hi, i am $name"
  }

  val ivan = Person("Ivan")
  val ivanGreeting = ivan.greet()

  given string2Person: Conversion[String, Person] with {
    override def apply(x: String): Person = Person(x)
  }

  val ivanGreeting2 = "ivan".greet()

  def processPerson(person: Person): String =
    if person.name.startsWith("J") then "OK" else "NOT OK"

  val isJaneOK = processPerson("Jane") // compiler rewrites to ProcessPerson("Jane")

  def main(args: Array[String]): Unit = {
    println(isJaneOK)
  }

}
