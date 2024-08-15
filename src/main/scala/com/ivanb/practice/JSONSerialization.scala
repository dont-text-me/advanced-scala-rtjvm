package com.ivanb.practice

import java.util.Date
import scala.concurrent.duration.*

object JSONSerialization {

  case class User(name: String, email: String, age: Int)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  // 1 - intermediate data: numbers, strings, lists, dates, objects
  // 2 - type class to convert data to intermediate data
  // 3 - serialize to JSON

  // part 1 - intermediate data
  sealed trait JSONValue:
    def stringify: String

  final case class JSONString(value: String) extends JSONValue:
    override def stringify: String = s"\"$value\""

  final case class JSONNumber(value: Int) extends JSONValue:
    override def stringify: String = value.toString

  final case class JSONArray(values: List[JSONValue]) extends JSONValue:
    override def stringify: String = values.map(_.stringify).mkString("[\n", ",\n", "\n]")

  final case class JSONObject(map: Map[String, JSONValue]) extends JSONValue:
    override def stringify: String =
      map.map((k, v) => s"\"$k\" : ${v.stringify}").mkString("{\n", ",\n", "\n}")

  val data = JSONObject(
    Map(
      "user" -> JSONString("Ivan"),
      "posts" -> JSONArray(
        List(
          JSONString("Scala is cool"),
          JSONNumber(42)
        )
      )
    )
  )

  // part 2 - type class pattern

  trait JSONConverter[A]:
    def convert(value: A): JSONValue

  given stringConverter: JSONConverter[String] = x => JSONString(x)
  given intConverter: JSONConverter[Int] = x => JSONNumber(x)
  given dateConverter: JSONConverter[Date] = x => JSONString(x.toString)
  given userConverter: JSONConverter[User] = { case User(name, email, age) =>
    JSONObject(
      Map(
        "name" -> JSONConverter[String].convert(name),
        "email" -> JSONConverter[String].convert(email),
        "age" -> JSONConverter[Int].convert(age)
      )
    )
  }

  given postConverter: JSONConverter[Post] = { case Post(content, createdAt) =>
    JSONObject(
      Map(
        "content" -> JSONConverter[String].convert(content),
        "createdAt" -> JSONConverter[String].convert(createdAt.toString)
      )
    )
  }
  given feedConverter: JSONConverter[Feed] = { case Feed(user, posts) =>
    JSONObject(
      Map(
        "user" -> userConverter.convert(user),
        "posts" -> JSONArray(posts.map(x => postConverter.convert(x)))
      )
    )
  }

  object JSONConverter:
    def convert[T](value: T)(using converter: JSONConverter[T]): JSONValue =
      converter.convert(value)
    def apply[T](using converter: JSONConverter[T]): JSONConverter[T] = converter

  val yesterday = new Date(System.currentTimeMillis() - 2.days.toMillis)
  val today = new Date(System.currentTimeMillis())
  val user = User("John", "john@example.com", 34)
  val feed = Feed(
    user,
    List(
      Post("My first post!", yesterday),
      Post("My second post!", today)
    )
  )

  object JSONSynthax:
    extension [T](value: T)
      def toIntermediate(using converter: JSONConverter[T]): JSONValue = converter.convert(value)
      def toJson(using converter: JSONConverter[T]): String = converter.convert(value).stringify

  def main(args: Array[String]): Unit = {
    import JSONSynthax.*
    println(feed.toIntermediate.stringify)
    println(feed.toJson)
  }

}
