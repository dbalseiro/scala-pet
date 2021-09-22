package com.stackbuilders.scalapet
package exercises

object FunctionalProgrammingBasic extends App {
  val concat = (a: String, b: String) => a + b
  println(concat("Diego", "Capo"))

  val curried = (a: Int) => (b: Int) => a + b
  println(curried(2)(3))

  val numbers = List(1, 2, 3)
  val chars = List('a', 'b', 'c')
  println(chars.flatMap(c => numbers.map(n => s"$c$n")))
}
