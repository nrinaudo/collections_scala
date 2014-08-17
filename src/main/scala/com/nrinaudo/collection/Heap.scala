package com.nrinaudo.collection

import scala.language.higherKinds

/** Generic heap.
  *
  * While it's possible to mix this trait in directly, it's not always convenient: some data structures can behave as,
  * but are not inherently, heaps. [[LeftistTree]] is an example of such a class. In these cases, developers should use
  * the [[HeapLike]] type class.
  */
trait Heap[A] {
  def isEmpty: Boolean
  def insert(a: A): Heap[A]
  def min: Option[A]
  def deleteMin(): Heap[A]

  def merge(as: Heap[A]): Heap[A] =
    as.min.map(insert(_).merge(as.deleteMin())).getOrElse(this)
}

/**
 * Declares implicit conversions from data structures that can behave as a heap
 * (ie that have a [[HeapLike]] implementation) to [[Heap]].
 */
object Heap {
  implicit class Wrapped[A, Impl[_]: HeapLike](val heap: Impl[A]) extends Heap[A] {
    private lazy val heapLike = implicitly[HeapLike[Impl]]

    override def isEmpty      = heapLike.isEmpty(heap)
    override def insert(a: A) = new Wrapped(heapLike.insert(a, heap))
    override def min          = heapLike.min(heap)
    override def deleteMin()  = new Wrapped(heapLike.deleteMin(heap))
  }
}

/** Type class used to transform data structures with heap-like functionality into actual instances of [[Heap]]. */
trait HeapLike[Impl[_]] {
  def isEmpty[A](a: Impl[A]): Boolean
  def insert[A](a: A, as: Impl[A]): Impl[A]
  def min[A](a: Impl[A]): Option[A]
  def deleteMin[A](a: Impl[A]): Impl[A]
}
