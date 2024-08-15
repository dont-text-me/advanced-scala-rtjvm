package com.ivanb.part4context

object Typeclasses {

  // small library to serialize data to HTML

  // v1 - OO way
  trait HTMLWritable:
    def toHtml: String

  case class User(name: String, age: Int, email: String) extends HTMLWritable:
    override def toHtml: String = s"<div>$name ($age y.o.) <a href=$email/></div>"

  val bob = User("Bob", 43, "bob@example.com")
  val bob2Html = bob.toHtml

  // v2 - pattern matching
  object HTMLSerializerPM
  def serializeToHtml(value: Any) = value match
    case User(name, age, email) => s"<div>$name ($age y.o.) <a href=$email/></div>"
    case _ => throw new IllegalArgumentException("data structure not supported")

  // v3 - type class
  // part 1 - type class definition
  trait HTMLSerializer[T]:
    def serialize(value: T): String

  // part 2 - type class instances
  given userSerializer: HTMLSerializer[User] =
    case User(name, age, email) => s"<div>$name ($age y.o.) <a href=$email/></div>"

  val bob2Html2 = userSerializer.serialize(bob)

  import java.util.Date
  given dateSerializer: HTMLSerializer[Date] = (d: Date) => s"<div>${d.toString}</div>"

  object PartialSerializer:
    given partialUserSerializer: HTMLSerializer[User] =
      case User(name, _, _) => s"<div>$name</div>"

  // part 3 - using the type class (user-facing API)

  object HTMLSerializer:
    def serialize[T](value: T)(using serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](using serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer

  val bob2Html3 = HTMLSerializer.serialize(bob)
  val bob2Html4 = HTMLSerializer[User].serialize(bob)

  // part 4 - extension methods
  object HTMLSynthax:
    extension [T](value: T)
      def toHTML(using serializer: HTMLSerializer[T]): String = serializer.serialize(value)

  import HTMLSynthax.*
  val bob2Html5 = bob.toHTML

  def main(args: Array[String]): Unit = {
    println(bob2Html)
    println(bob2Html2)
    println(bob2Html3)
    println(bob2Html4)
    println(bob2Html5)
  }
}
