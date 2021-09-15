package com.stackbuilders.scalapet
package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  def factorial (n: Int): BigInt = {
    @tailrec
    def go (base: BigInt, acc: BigInt): BigInt =
      if (base <= 1) acc else go (base-1, base*acc)
    go (n, 1)
  }

  println(factorial(15))

  def fibonacci(n: Int): BigInt = {
    @tailrec
    def go(base: BigInt, prev1: BigInt, prev2: BigInt): BigInt =
      if (base <= 1) prev2 + prev1
      else go(base - 1, prev2, prev1 + prev2)

    go(n, 1, 1)
  }

  println(fibonacci(5))
}
