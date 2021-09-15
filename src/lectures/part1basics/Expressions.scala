package com.stackbuilders.scalapet
package lectures.part1basics

object Expressions extends App {
  val x = 1 + 2
  println(x)

  val cond = true
  val e = if (cond) 5 else 3
  println(e)

  val cb = {
    val y = 2
    val z = y + 1
    if (z > 2) "hello" else "bye"
  }

  println(cb)
}