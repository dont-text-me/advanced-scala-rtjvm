package com.ivanb.part2afp

object CurryingPAFs {

  val superAdder: Int => Int => Int = x => y => x + y

  val add3 = superAdder(3)

  val eight = add3(5)

  def curriedAdder(x: Int)(y: Int): Int = x + y

  // converting a method to functions
  val add4 = curriedAdder(4) // eta-expansion
  val nine = add4(5)

  def increment(x: Int): Int = x + 1

  val aList = List(1, 2, 3)
  val anIncrementedList = aList.map(increment) // eta-expansion

  def concatenator(a: String, b: String, c: String): String = a + b + c

  val insertName =
    concatenator(
      "Hello, my name is ",
      _: String,
      " How are you?"
    ) // underscore is the argument to the new lambda

  val ivansGreeting = insertName("Ivan")

  val simpleAddFunction = (x: Int, y: Int) => x + y

  def simpleAddMethod(x: Int, y: Int) = x + y

  def curriedMethod(x: Int)(y: Int) = x + y

  val add7_1 = simpleAddMethod(_: Int, 7)
  val add7_2 = simpleAddMethod(7, _: Int)
  val add7_3 = simpleAddFunction(7, _: Int)
  val add7_4 = curriedMethod(7)
  val add7_5 = (x: Int) => simpleAddFunction(7, x)
  val add7_6 = simpleAddFunction.curried(7)


  def formatDouble(format: String)(number: Double) = format.format(number)

  val nums = List(Math.PI, Math.E, Math.TAU)
  val highPrecision = nums.map(formatDouble("%8.6f"))
  val lowPrecision = nums.map(formatDouble("%4.2f"))

  // methods vs functions, by-name vs 0-lambdas

  def byName(n: => Int) = n + 1

  def byLambda(f: () => Int) = f() + 1

  def method: Int = 42

  def parenMethod(): Int = 42

  byName(42)
  byName(method) // NOT eta-expanded, method is invoked
  byName(parenMethod())
  // byName(parenMethod) // not ok

  byName((() => 42)()) // ok
  // byName(() => 42) // not ok

  // byLambda(23) // not ok
  // byLambda(method) // eta-expansion is not possible (because there is no argument list)
  byLambda(parenMethod) // ok
  byLambda(() => 42) // ok
  byLambda(() => parenMethod()) // ok

}
