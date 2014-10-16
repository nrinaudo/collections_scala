package com.nrinaudo.collection

object BinomialHeap {
  def empty[A: Ordering]: BinomialHeap[A] = new BinomialHeap(Nil)
  def apply[A: Ordering](as: A*): BinomialHeap[A] = as.foldLeft(empty)(_ insert _)

  implicit object AsHeap extends HeapLike[BinomialHeap] {
      override def isEmpty[A](as: BinomialHeap[A])      = as.isEmpty
      override def insert[A](a: A, as: BinomialHeap[A]) = as.insert(a)
      override def findMin[A](as: BinomialHeap[A])      = as.findMin
      override def deleteMin[A](as: BinomialHeap[A])    = as.deleteMin()
    }
}

class BinomialHeap[A: Ordering] private (private val trees: List[BinomialTree[A]]) {
  private def insTree(tree: BinomialTree[A], trees: List[BinomialTree[A]]): List[BinomialTree[A]] = trees match {
    case Nil    => List(tree)
    case h :: t =>
      if(tree.rank < h.rank) tree :: trees
      else                   insTree(tree.link(h), t)
  }

  private def merge(trees1: List[BinomialTree[A]], trees2: List[BinomialTree[A]]): List[BinomialTree[A]] = (trees1, trees2) match {
    case (Nil,    _)          => trees2
    case (_,      Nil)        => trees1
    case (h1 :: t1, h2 :: t2) =>
      if(h2.rank < h1.rank)      h1 :: merge(trees2, t1)
      else if(h1.rank < h2.rank) h2 :: merge(trees1, t2)
      else                       insTree(h1.link(h2), merge(t1, t2))
  }

  private def removeMinTree(trees: List[BinomialTree[A]]): (Option[BinomialTree[A]], List[BinomialTree[A]]) = trees match {
    case Nil         => (None, trees)
    case tree :: Nil => (Some(tree), Nil)
    case h :: t      =>
      val (Some(min), rest) = removeMinTree(t)
      if(implicitly[Ordering[A]].lt(h.value, min.value)) (Some(h), t)
      else                                               (Some(min), h :: rest)
  }

  def isEmpty: Boolean = trees.isEmpty
  def insert(a: A) : BinomialHeap[A] = new BinomialHeap(insTree(BinomialTree(a), trees))
  def merge(as:  BinomialHeap[A]): BinomialHeap[A] = new BinomialHeap(merge(trees, as.trees))
  def findMin : Option[A] = removeMinTree(trees)._1.map(_.value)
  def deleteMin(): BinomialHeap[A] = removeMinTree(trees) match {
    case (Some(BinomialTree(_, _, r1)), r2) => new BinomialHeap(merge(r1.reverse, r2))
    case _                                  => throw new UnsupportedOperationException("empty.deleteMin")
  }

  def +(a: A)  = insert(a)
  def nonEmpty = !isEmpty
}



// - Binomial trees ----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
private object BinomialTree {
  def apply[A: Ordering](a: A): BinomialTree[A] = BinomialTree(a, 0, Nil)
}

private case class BinomialTree[A: Ordering](value: A, rank: Int, children: List[BinomialTree[A]]) {
  def link(as: BinomialTree[A]): BinomialTree[A] = {
    require(as.rank == rank)

    if(implicitly[Ordering[A]].lteq(value, as.value)) BinomialTree(value,    rank + 1, as :: children)
    else                                              BinomialTree(as.value, rank + 1, this :: as.children)
  }
}
