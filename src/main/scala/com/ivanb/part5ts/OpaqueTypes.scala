package com.ivanb.part5ts

object OpaqueTypes {

  object SocialNetwork:
    // some data structures = "domain"
    opaque type Name = String

    object Name:
      def apply(s: String): Name = s

    extension (name: Name) def length: Int = name.length // use String API

    // inside the object, Name and String can be used interchangeably
    def addFriend(person1: Name, person2: Name): Boolean =
      person1.length == person2.length // string methods available

  // outside SocialNetwork, Name and String are NOT related

  import SocialNetwork.*
  // val name: Name = "some string" //not ok

  // why: for when you dont need or want to have access to the entire String API for the Name type

  object Graphics:
    opaque type Color = Int // in hex
    opaque type ColorFilter <: Color = Int

    val red: Color = 0xff000000
    val green: Color = 0x00ff0000
    val blue: Color = 0x0000ff00
    val halfTransparency: ColorFilter = 0x88

  import Graphics.*

  case class OverlayFilter(c: Color)

  val fadeLayer = OverlayFilter(halfTransparency) // ok

  // creating and accessing the APIs of opaque types
  // 1 - companion objects
  val aName: Name = Name("Ivan")
  val nameLength = aName.length // ok
}
