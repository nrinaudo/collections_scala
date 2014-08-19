package com.nrinaudo.collection

import scala.language.higherKinds

trait Stack[A] {
  def isEmpty: Boolean
  def push(a: A): Stack[A]
  def top: Option[A]
  def pop(): Stack[A]
}

object Stack {
  implicit class Wrapped[A, Impl[_] : StackLike](stack: Impl[A]) extends Stack[A] {
    private val stackLike = implicitly[StackLike[Impl]]

    override def isEmpty    = stackLike.isEmpty(stack)
    override def push(a: A) = new Wrapped(stackLike.push(a, stack))
    override def top        = stackLike.top(stack)
    override def pop()      = new Wrapped(stackLike.pop(stack))
  }

  /** Marks Scala lists as valid implementations of Stack. */
  implicit object ListStack extends StackLike[List] {
    override def isEmpty[A](as: List[A])    = as.isEmpty
    override def push[A](a: A, as: List[A]) = a :: as
    override def top[A](as: List[A])        = as.headOption
    override def pop[A](as: List[A])        = as.tail
  }
}

trait StackLike[Impl[_]] {
  def isEmpty[A](as: Impl[A]): Boolean
  def push[A](a: A, as: Impl[A]): Impl[A]
  def top[A](as: Impl[A]): Option[A]
  def pop[A](as: Impl[A]): Impl[A]
}
