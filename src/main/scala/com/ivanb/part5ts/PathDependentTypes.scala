package com.ivanb.part5ts

object PathDependentTypes {
  class Outer:
    class Inner
    object InnerObject
    type InnerType
    def process(arg: Inner): Unit = println(arg)
    def processGeneral(arg: Outer#Inner /*the parent type of all Inner types*/ ) = println(arg)

  val outer = new Outer
  val inner = new outer.Inner // outer.Inner is a separate TYPE = path-dependent type

  val outerA = new Outer
  val outerB = new Outer
  // val inner2 : outerA.Inner = new outerB.Inner // not ok, every instance has its own type
  val innerA = new outerA.Inner
  val innerB = new outerB.Inner

  // outerA.process(innerB) // not ok, same as above
  outer.process(inner) // ok

  outerA.processGeneral(innerB)

  trait Record:
    type Key
    def defaultValue: Key

  class StringRecord extends Record:
    override type Key = String
    override def defaultValue = ""

  class IntRecord extends Record:
    override type Key = Int
    override def defaultValue = 0

  def getDefaultIdentifier(record: Record): record.Key =
    record.defaultValue // no type arguments needed

  val aString: String = getDefaultIdentifier(new StringRecord) // ok
  val anInt: Int = getDefaultIdentifier(new IntRecord)

  val getIdentifierFunc: Record => Record#Key = getDefaultIdentifier

}
