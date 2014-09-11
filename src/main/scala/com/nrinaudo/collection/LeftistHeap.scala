package com.nrinaudo.collection

object LeftistHeap {
  def empty[A: Ordering]: LeftistHeap[A] = Leaf()
  def apply[A: Ordering](as: A*): LeftistHeap[A] = {
    val leaf = empty[A]

    def merge(data: Seq[LeftistHeap[A]]): LeftistHeap[A] = data match {
      case Nil      => empty
      case a :: Nil => a
      case l        => merge(l.grouped(2).map {
        case a :: b :: _ => a.merge(b)
        case a :: _      => a
      }.toList)
    }

    merge(as.map(Node(_, 1, leaf, leaf)))
  }

  implicit object AsHeap extends HeapLike[LeftistHeap] {
    override def isEmpty[A](a: LeftistHeap[A])         = a.isEmpty
    def merge[A](a: LeftistHeap[A], b: LeftistHeap[A]) = a.merge(b)
    override def insert[A](a: A, as: LeftistHeap[A])   = as.insert(a)
    override def findMin[A](a: LeftistHeap[A])             = a.min
    override def deleteMin[A](a: LeftistHeap[A])       = a.deleteMin()
  }

  case class Node[A: Ordering](value: A, rank: Int, left: LeftistHeap[A], right: LeftistHeap[A]) extends LeftistHeap[A] {
    private def lessThan(a: A, b: A): Boolean = implicitly[Ordering[A]].lt(a, b)

    private def tag(a: A, left: LeftistHeap[A], right: LeftistHeap[A]): LeftistHeap[A] =
      if(left.rank > right.rank) Node(a, left.rank + 1, left, right)
      else                       Node(a, right.rank + 1, right, left)

    override def isEmpty = false

    override def merge(as: LeftistHeap[A]) = as match {
      case Leaf()                         => this
      case Node(value2, _, left2, right2) =>
        if(lessThan(value, value2)) tag(value,  left,  right.merge(as))
        else                        tag(value2, left2, this.merge(right2))
    }

    override def insert(a: A) = merge(Node(a, 1, Leaf(), Leaf()))
    override def deleteMin()  = left.merge(right)
    override def min          = Some(value)
  }

  case class Leaf[A: Ordering]() extends LeftistHeap[A] {
    override val rank                      = 0
    override def isEmpty                   = true
    override def merge(as: LeftistHeap[A]) = as
    override def insert(a: A)              = Node(a, 1, this, this)
    override def deleteMin()               = throw new UnsupportedOperationException("Leaf.deleteMin")
    override def min                       = None
  }
}

sealed trait LeftistHeap[A] {
  def rank: Int
  def isEmpty: Boolean
  def merge(as: LeftistHeap[A]): LeftistHeap[A]
  def insert(a: A): LeftistHeap[A]
  def deleteMin(): LeftistHeap[A]
  def min: Option[A]
}
