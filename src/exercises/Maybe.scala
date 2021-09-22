package com.stackbuilders.scalapet
package exercises


abstract class Maybe[+T] {
  def map[B](f: T => B): Maybe[B]
  def flatMap[B](f: T => Maybe[B]): Maybe[B]
  def filter(p: T => Boolean): Maybe[T]
}

case object MaybeNot extends Maybe[Nothing] {
  override def map[B](f: Nothing => B): Maybe[B] = MaybeNot
  override def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = MaybeNot
  override def filter(p: Nothing => Boolean): Maybe[Nothing] = MaybeNot
}

case class Just[+T](value: T) extends Maybe[T] {
  override def map[B](f: T => B): Maybe[B] = Just(f(value))
  override def flatMap[B](f: T => Maybe[B]): Maybe[B] = f(value)

  override def filter(p: T => Boolean): Maybe[T] =
    if (p(value)) this
    else MaybeNot
}