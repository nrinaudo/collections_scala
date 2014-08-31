package com.nrinaudo.collection

import scala.language.higherKinds

trait PriorityQueue[A] {
  def isEmpty     : Boolean
  def insert(a: A): PriorityQueue[A]
  def min         : Option[A]
  def deleteMin() : PriorityQueue[A]

  def nonEmpty: Boolean = !isEmpty
  def merge(as: PriorityQueue[A]): PriorityQueue[A] =
    as.min.map(insert(_).merge(as.deleteMin())).getOrElse(this)
}

object PriorityQueue {
  implicit class Wrapped[A, Impl[_]](val queue: Impl[A])(implicit queueLike: PriorityQueueLike[Impl]) extends PriorityQueue[A] {
    override def isEmpty      = queueLike.isEmpty(queue)
    override def insert(a: A) = new Wrapped(queueLike.insert(a, queue))
    override def min          = queueLike.min(queue)
    override def deleteMin()  = new Wrapped(queueLike.deleteMin(queue))
  }
}

trait PriorityQueueLike[Impl[_]] {
  def isEmpty[A](a: Impl[A])      : Boolean
  def insert[A](a: A, as: Impl[A]): Impl[A]
  def min[A](a: Impl[A])          : Option[A]
  def deleteMin[A](a: Impl[A])    : Impl[A]
}
