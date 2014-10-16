package com.nrinaudo.collection

object RedBlackTree {
  // - Smart constructors ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def empty[A: Ordering]: RedBlackTree[A] = Leaf()
  def apply[A: Ordering](as: A*): RedBlackTree[A] = as.foldLeft(empty)(_.add(_))



  // - Normal node -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Node[A: Ordering](value: A, isRed: Boolean, left: RedBlackTree[A], right: RedBlackTree[A]) extends RedBlackTree[A] {
    /** If this node is black but its two descendants are red, flip their colors. */
    private def flipColors: Node[A] = this match {
      case Node(_, false, l@Node(_, true, _, _), r@Node(_, true, _, _)) =>
        Node(value, true, l.copy(isRed = false), r.copy(isRed = false))
      case _ => this
    }

    /** If the left child is black and the right one red, rotate the node to lean left. */
    private def rotateLeft: Node[A] = this match {
      case Node(_, _, Node(_, false, _, _), Node(v, true, l, r)) => Node(v, isRed, Node(value, true, left, l), r)
      case _                                                     => this
    }

    /** If the left child and grandchild are both red, rotate right. */
    def rotateRight: Node[A] = this match {
      case Node(_, true, Node(v, true, l, r), _) => Node(v, isRed, l, Node(value, true, r, right))
      case _                                     => this
    }

    private def fix: Node[A] = rotateLeft.rotateRight.flipColors

    override def isEmpty = false

    override def contains(a: A) =
      if(implicitly[Ordering[A]].lt(a, value))      left.contains(a)
      else if(implicitly[Ordering[A]].gt(a, value)) right.contains(a)
      else                                          true

    override def add(a: A) =
      if(implicitly[Ordering[A]].lt(a, value))      copy(left = left.add(a)).fix
      else if(implicitly[Ordering[A]].gt(a, value)) copy(right = right.add(a)).fix
      else                                          this

    override def min = left.min.orElse(Some(value))
    override def max = right.max.orElse(Some(value))
  }



  // - Leaf node -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Leaf[A: Ordering]() extends RedBlackTree[A] {
    override val isRed          = false
    override val isEmpty        = true
    override def contains(a: A) = false
    override def add(a: A)      = Node(a, false, this, this)
    override def min            = None
    override def max            = None
  }



  // - Type class instances --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit object AsSet extends SetLike[RedBlackTree] {
      override def isEmpty[A](as: RedBlackTree[A])        = as.isEmpty
      override def insert[A](a: A, as: RedBlackTree[A])   = as + a
      override def contains[A](a: A, as: RedBlackTree[A]) = as.contains(a)
    }
}

sealed trait RedBlackTree[A] {
  protected def isRed: Boolean

  def isEmpty: Boolean
  def add(a: A): RedBlackTree[A]
  def contains(a: A): Boolean

  def +(a: A): RedBlackTree[A] = add(a)
  def nonEmpty = !isEmpty

  def min: Option[A]
  def max: Option[A]
}
