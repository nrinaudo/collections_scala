package com.nrinaudo.collection

import scala.language.higherKinds

trait Heap[A] {
  def isEmpty: Boolean
  def insert(a: A): Heap[A]
  def min: Option[A]
  def deleteMin(): Heap[A]

  def nonEmpty: Boolean = !isEmpty
  def merge(as: Heap[A]): Heap[A] =
    as.min.map(insert(_).merge(as.deleteMin())).getOrElse(this)
}

object Heap {
  implicit class Wrapped[A, Impl[_]: HeapLike](val heap: Impl[A]) extends Heap[A] {
    private lazy val heapLike = implicitly[HeapLike[Impl]]

    override def isEmpty      = heapLike.isEmpty(heap)
    override def insert(a: A) = new Wrapped(heapLike.insert(a, heap))
    override def min          = heapLike.min(heap)
    override def deleteMin()  = new Wrapped(heapLike.deleteMin(heap))
  }
}

trait HeapLike[Impl[_]] {
  def isEmpty[A](a: Impl[A]): Boolean
  def insert[A](a: A, as: Impl[A]): Impl[A]
  def min[A](a: Impl[A]): Option[A]
  def deleteMin[A](a: Impl[A]): Impl[A]
}
