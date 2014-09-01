package com.nrinaudo.collection

import scala.language.higherKinds

trait Heap[A] {
  def isEmpty     : Boolean
  def insert(a: A): Heap[A]
  def min         : Option[A]
  def deleteMin() : Heap[A]

  def nonEmpty: Boolean = !isEmpty
  def merge(as: Heap[A]): Heap[A] =
    as.min.map(insert(_).merge(as.deleteMin())).getOrElse(this)
}

object Heap {
  implicit class Wrapped[A, Impl[_]](val queue: Impl[A])(implicit queueLike: HeapLike[Impl]) extends Heap[A] {
    override def isEmpty      = queueLike.isEmpty(queue)
    override def insert(a: A) = new Wrapped(queueLike.insert(a, queue))
    override def min          = queueLike.min(queue)
    override def deleteMin()  = new Wrapped(queueLike.deleteMin(queue))
  }
}

trait HeapLike[Impl[_]] {
  def isEmpty[A](a: Impl[A])      : Boolean
  def insert[A](a: A, as: Impl[A]): Impl[A]
  def min[A](a: Impl[A])          : Option[A]
  def deleteMin[A](a: Impl[A])    : Impl[A]
}
