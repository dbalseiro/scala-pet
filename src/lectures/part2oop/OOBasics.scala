package com.stackbuilders.scalapet
package lectures.part2oop

object OOBasics extends App {
  var person = new Person() //("Diego", 43)
  println(person.age)
  println(person.x)
  println(person.greet("Pepe"))
  println(person.greet())
}

class Person (name: String, val age: Int) {
  val x = 2

  def greet(name: String): String =
    s"Hello $name, my name is ${this.name}"

  def greet() = s"My name is $name"

  def this() = this("anon", 0)
}