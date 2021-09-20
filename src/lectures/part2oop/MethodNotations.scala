package com.stackbuilders.scalapet
package lectures.part2oop

object MethodNotations extends App {
  class Person(name: String, val favoriteMovie: String) {
    def >>=(movie: String): Boolean = favoriteMovie == movie
    def unary_! : Person = new Person("Diego", favoriteMovie)
    def apply() = s"Hello, my name is ${name} and I like ${favoriteMovie}"
  }

  val mary = new Person("Mary", "Inception")
  println(mary >>= "Inception")
  println((!mary)())
}
