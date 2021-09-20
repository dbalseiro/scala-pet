package com.stackbuilders.scalapet
package exercises

trait MyPredicate[-T] {
  def apply(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def apply(elem: A): B
}

abstract class MyListF[+A] {
  def head: A
  def tail: MyListF[A]
  def isEmpty: Boolean
  def add[B >: A] (elem: B): MyListF[B]
  def length: Int
  protected def printElems: String
  override def toString: String = printElems

  def filter(predicate: MyPredicate[A]): MyListF[A]
  def map[B](transformer: MyTransformer[A, B]): MyListF[B]
  def flatMap[B](transformer: MyTransformer[A, MyListF[B]]): MyListF[B]

  def ++[B>:A](l: MyListF[B]):MyListF[B]
}

object MyListF {
  def apply[A](elem: A): MyListF[A] = new Cons(elem, Empty)
}

case object Empty extends MyListF[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyListF[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing] (elem: B): MyListF[B] = new Cons(elem, Empty)
  override def length: Int = 0
  override def printElems: String = "[]"

  override def filter(predicate: MyPredicate[Nothing]): MyListF[Nothing] = Empty
  override def map[B](transformer: MyTransformer[Nothing, B]): MyListF[B] = Empty
  override def flatMap[B](transformer: MyTransformer[Nothing, MyListF[B]]): MyListF[B] = Empty

  override def ++[B >: Nothing](l: MyListF[B]): MyListF[B] = l
}

case class Cons[+A] (h: A, t: MyListF[A]) extends MyListF[A] {
  override def head: A = h
  override def tail: MyListF[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](elem: B): MyListF[B] = Cons(elem, this)
  override def length: Int = 1 + t.length
  override def printElems: String = h + ":" + t.toString

  override def filter(predicate: MyPredicate[A]): MyListF[A] =
    if (predicate(head)) Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def map[B](transformer: MyTransformer[A, B]): MyListF[B] =
    Cons(transformer(head), tail.map(transformer))

  override def flatMap[B](transformer: MyTransformer[A, MyListF[B]]): MyListF[B] =
    transformer(head) ++ tail.flatMap(transformer)

  override def ++[B >: A](l: MyListF[B]): MyListF[B] = Cons(head, tail ++ l)
}

object MyListFTest extends App {
  val l: MyListF[Int] = Empty
  val ll = l.add(2).add(1)
  println(ll.toString)
  println(ll.head)
  println(ll.tail)
  println(ll.length)
  println(l.isEmpty)

  println {
    ll
      .flatMap(new MyTransformer[Int, MyListF[Int]] {
        def apply(elem: Int): MyListF[Int] = MyListF(elem).add(elem+1)
      })
      .filter(new MyPredicate[Int] {
        def apply(elem: Int): Boolean = elem > 1
      })
      .map(new MyTransformer[Int, String] {
        def apply(n: Int): String = s"This is $n"
      })
      .toString
  }


  val bar: MyListF[String] = new Cons("lol", Empty)
  println(bar.length)
  println(bar.toString)
  println(bar.add(2).add(3).toString)
  println(bar.isEmpty)
}
