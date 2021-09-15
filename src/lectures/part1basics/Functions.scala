package com.stackbuilders.scalapet
package lectures.part1basics

object Functions extends App {
  def repeat(a: String, n: Int): String = {
    if (n <= 0) "" else a + repeat(a, n - 1)
  }
  println(repeat("hello", 3))
}
