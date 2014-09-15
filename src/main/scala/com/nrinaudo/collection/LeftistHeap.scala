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
    override def isEmpty[A](as: LeftistHeap[A])      = as.isEmpty
    override def insert[A](a: A, as: LeftistHeap[A]) = as.insert(a)
    override def findMin[A](as: LeftistHeap[A])      = as.findMin
    override def deleteMin[A](as: LeftistHeap[A])    = as.deleteMin()
  }

  case class Node[A](value: A, rank: Int, left: LeftistHeap[A], right: LeftistHeap[A])(implicit ord: Ordering[A])
    extends LeftistHeap[A] {
    private def sortRank(a: A, as1: LeftistHeap[A], as2: LeftistHeap[A]): LeftistHeap[A] =
      if(as1.rank > as2.rank) Node(a, as1.rank + 1, as1, as2)
      else                    Node(a, as2.rank + 1, as2, as1)

    override val isEmpty = false

    override def merge(as: LeftistHeap[A]) = as match {
      case Leaf()                         => this
      case Node(value2, _, left2, right2) =>
        if(ord.lt(value, value2)) sortRank(value,  left,  right.merge(as))
        else                      sortRank(value2, left2, this.merge(right2))
    }

    override def insert(a: A) = merge(Node(a, 1, Leaf(), Leaf()))
    override def deleteMin() = left.merge(right)
    override def findMin = Some(value)
  }

  case class Leaf[A: Ordering]() extends LeftistHeap[A] {
    override val rank                      = 0
    override val isEmpty                   = true
    override def merge(as: LeftistHeap[A]) = as
    override def insert(a: A)              = Node(a, 1, this, this)
    override def deleteMin()               = throw new UnsupportedOperationException("Leaf.deleteMin")
    override val findMin                   = None
  }
}

sealed trait LeftistHeap[A] {
  def rank                     : Int
  def isEmpty                  : Boolean
  def merge(as: LeftistHeap[A]): LeftistHeap[A]
  def insert(a: A)             : LeftistHeap[A]
  def deleteMin()              : LeftistHeap[A]
  def findMin                  : Option[A]

  def +(a: A): LeftistHeap[A] = insert(a)
  def nonEmpty = !isEmpty
}
