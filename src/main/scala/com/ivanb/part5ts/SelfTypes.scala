package com.ivanb.part5ts

object SelfTypes {

  trait Instrumentalist:
    def play(): Unit

  trait Singer:
    self: Instrumentalist => // self-type: whoever implements Singer must also implement Instrumentalist
    // ^ name can be anything, usually "self"
    def sing(): Unit

  class LeadSinger extends Singer with Instrumentalist: // ok
    override def sing(): Unit = ???
    override def play(): Unit = ???

//  class Vocalist extends Singer: // not ok
//    override def sing(): Unit = ???

  val singerAndInstr = new Singer with Instrumentalist:
    override def sing(): Unit = ???
    override def play(): Unit = ???

  class Guitarist extends Instrumentalist:
    override def play(): Unit = println("some guitar solo")

  val aGuitarist = new Guitarist with Singer: // ok, Guitarist <: Instrumentalist
    override def sing(): Unit = println("singing...")

  // self types vs inheritance

  class A
  class B extends A // B "is an" "A"

  trait T
  trait S { self: T => } // S "requires a" T

  // self-types for DI = "cake pattern"

  abstract class Component
  class ComponentA extends Component
  class ComponentB extends Component
  class DependentComponent(val component: Component) // regular DI

  // cake pattern

  trait ComponentLayer1:
    def actionLayer1(x: Int): String

  trait ComponentLayer2:
    self: ComponentLayer1 =>
    def actionLayer2(x: String): Int

  trait Application:
    self: ComponentLayer1 & ComponentLayer2 =>
  // main API here

  trait Picture extends ComponentLayer1
  trait Stats extends ComponentLayer1

  trait ProfilePage extends ComponentLayer2 with Picture // extends Layer 2 API and Picture API
  trait Analytics extends ComponentLayer2 with Stats

  trait AnalyticsApp extends Application with Analytics

  // self-types: preserve the "this" instance

  class SingerWithInnerClass:
    self => // self-type with no type requirement, self == this
    class Voice:
      def sing() = this.toString // this == the voice, can use "self" to refer to the outer instance

  // not ok, cyclical inheritance does not work
//  class X extends Y
//  class Y extends X

  trait X { self: Y => }
  trait Y { self: X => }
  // ok

}
