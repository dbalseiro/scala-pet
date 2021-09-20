package com.stackbuilders.scalapet
package exercises

import scala.annotation.tailrec

class MyList(node: Node = null) {
  /* SINGLE LINKED LIST */
  def head: Int = node.h

  def tail: MyList = if (isEmpty) new MyList else node.t

  def isEmpty: Boolean = node == null

  def add(elem: Int): MyList = new MyList(new Node(elem, this))

  def length: Int = if (this.isEmpty) 0 else 1 + this.tail.length

  override def toString: String = {
    @tailrec
    def go(acc: String, l: MyList): String =
      if (l.isEmpty) acc + "[]"
      else go(acc + l.head + ":", l.tail)

    go("", this)
  }
}

class Node (val h: Int, val t: MyList)

object Foo extends App {
  val l = new MyList
  val ll = l.add(1).add(2)
  println(ll.toString)
  println(ll.head)
  println(ll.tail)
  println(ll.length)
  println(l.isEmpty)

  val bar = new MyList(new Node(42, new MyList))
  println(bar.length)
  println(bar.toString)
  println(bar.add(2).add(3).toString)
  println(bar.isEmpty)
}