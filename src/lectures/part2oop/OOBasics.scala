package com.stackbuilders.scalapet
package lectures.part2oop

object OOBasics extends App {
  var person = new Person() //("Diego", 43)
  println(person.age)
  println(person.x)
  println(person.greet("Pepe"))
  println(person.greet())

  val c = new Counter
  c.inc.print
  c.inc.inc.print
  c.inc(3).print
}

class Person (name: String, val age: Int) {
  val x = 2

  def greet(name: String): String =
    s"Hello $name, my name is ${this.name}"

  def greet() = s"My name is $name"

  def this() = this("anon", 0)
}

class Counter (count: Int = 0) {
  def inc() = new Counter(count + 1)

  def dec() = new Counter(count + 2)

  def inc(n: Int): Counter =
    if (n <= 0) this
    else this.inc.inc(n - 1)

  def dec(n: Int): Counter =
    if (n <= 0) this
    else this.dec.dec(n - 1)

  def print() = println(count)
}
