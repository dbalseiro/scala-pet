package com.stackbuilders.scalapet
package exercises

abstract class MyListF[+A] {
  def head: A
  def tail: MyListF[A]
  def isEmpty: Boolean
  def add[B >: A] (elem: B): MyListF[B]
  def length: Int

  def foreach(action: A => Unit): Unit
  def sort(criteria: (A, A) => Int): MyListF[A]
  def zipWith[B, C](that: MyListF[B], zipper: (A, B) => C): MyListF[C]

  protected def printElems: String
  override def toString: String = printElems

  def filter(predicate: A => Boolean): MyListF[A]
  def map[B](transformer: A => B): MyListF[B]
  def flatMap[B](transformer: A => MyListF[B]): MyListF[B]
  def fold[B](acc: B, reducer: (A, B) => B): B

  def ++[B>:A](l: MyListF[B]):MyListF[B]
  def reverse(): MyListF[A]
}

object MyListF {
  def singleton[A](elem: A): MyListF[A] = new Cons(elem, Empty)

  def uncurry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def curry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f (a) (b)

  def andThen[A,B,C](f: A => B, g: B => C): A => C = (x: A) => g(f(x))
  def compose[A,B,C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))
}

case object Empty extends MyListF[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyListF[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def add[B >: Nothing] (elem: B): MyListF[B] = new Cons(elem, Empty)
  override def length: Int = 0
  override def printElems: String = "[]"

  override def filter(predicate: Nothing => Boolean): MyListF[Nothing] = Empty
  override def map[B](transformer: Nothing => B): MyListF[B] = Empty
  override def flatMap[B](transformer: Nothing => MyListF[B]): MyListF[B] = Empty

  override def ++[B >: Nothing](l: MyListF[B]): MyListF[B] = l
  override def reverse(): MyListF[Nothing] = Empty

  override def foreach(action: Nothing => Unit): Unit = ()
  override def sort(criteria: (Nothing, Nothing) => Int): MyListF[Nothing] = Empty
  override def zipWith[B, C](that: MyListF[B], zipper: (Nothing, B) => C): MyListF[C] = Empty
  override def fold[B](acc: B, reducer: (Nothing, B) => B): B = acc
}

case class Cons[+A] (h: A, t: MyListF[A]) extends MyListF[A] {
  override def head: A = h
  override def tail: MyListF[A] = t
  override def isEmpty: Boolean = false
  override def add[B >: A](elem: B): MyListF[B] = Cons(elem, this)
  override def length: Int = 1 + t.length
  override def printElems: String = h + ":" + t.toString

  override def filter(predicate: A => Boolean): MyListF[A] =
    if (predicate(head)) Cons(head, tail.filter(predicate))
    else tail.filter(predicate)

  override def map[B](transformer: A => B): MyListF[B] =
    Cons(transformer(head), tail.map(transformer))

  override def flatMap[B](transformer: A => MyListF[B]): MyListF[B] =
    transformer(head) ++ tail.flatMap(transformer)

  override def ++[B >: A](l: MyListF[B]): MyListF[B] = Cons(head, tail ++ l)
  override def reverse(): MyListF[A] = tail.reverse() ++ MyListF.singleton(head)

  override def foreach(action: A => Unit): Unit = {
    action(head)
    tail.foreach(action)
  }

  override def sort(criteria: (A, A) => Int): MyListF[A] = {
    val comparator = MyListF.uncurry(criteria)(head)
    val left = tail.filter(comparator(_) > 0).sort(criteria)
    val right = tail.filter(comparator(_) <= 0).sort(criteria)
    left ++ MyListF.singleton(head) ++ right
  }

  override def zipWith[B, C](that: MyListF[B], zipper: (A, B) => C): MyListF[C] =
    if (that.isEmpty) Empty
    else Cons(zipper(this.head, that.head), this.tail.zipWith(that.tail, zipper))

  override def fold[B](acc: B, reducer: (A, B) => B): B =
    tail.fold(reducer(head, acc), reducer)
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
    ll.flatMap(elem => MyListF.singleton(elem).add(elem+1))
      .filter(_ > 1)
      .map("This is " + _)
      .toString
  }

  val bar: MyListF[String] = new Cons("lol", Empty)
  println(bar.length)
  println(bar.toString)
  println(bar.add(2).add(3).toString)
  println(bar.isEmpty)

  val lll = Empty.add(3).add(2).add(1)
  println(lll)
  println(lll.foreach(println))
  println(lll.sort((x, y) => y - x))
  println(lll.zipWith(lll.add(99).reverse(), (i:Int, value:Int) => i - value))
  println(lll.fold(0, _ + (_: Int)))

  val add3AndMultiply2: Int => Int = MyListF.compose((_:Int) + 3, _ * 2)
  val add3AndMultiply2prime: Int => Int = MyListF.andThen(_ + 3, (_:Int) * 2)
  println(add3AndMultiply2(2))
  println(add3AndMultiply2prime(2))

}
