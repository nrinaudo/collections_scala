package com.nrinaudo.collection

object BinarySearchTree {
  def empty[A: Ordering]: BinarySearchTree[A] = Leaf()
  def apply[A: Ordering](as: A*): BinarySearchTree[A] = as.foldLeft(empty)(_.add(_))

  case class Node[A](value: A, left: BinarySearchTree[A], right: BinarySearchTree[A])(implicit ord: Ordering[A])
    extends BinarySearchTree[A] {
    override def isEmpty = false

    override def add(a: A) =
      if(ord.lt(a, value))      copy(left  = left + a)
      else if(ord.gt(a, value)) copy(right = right + a)
      else                      this

    override def contains(a: A) =
      if(ord.lt(a, value))      left.contains(a)
      else if(ord.gt(a, value)) right.contains(a)
      else                      true
  }

  case class Leaf[A: Ordering]() extends BinarySearchTree[A] {
    override def isEmpty        = true
    override def add(a: A)      = Node(a, this, this)
    override def contains(a: A) = false
  }

  implicit object AsSet extends SetLike[BinarySearchTree] {
    override def isEmpty[A](as: BinarySearchTree[A])        = as.isEmpty
    override def insert[A](a: A, as: BinarySearchTree[A])   = as + a
    override def contains[A](a: A, as: BinarySearchTree[A]) = as.contains(a)
  }
}

sealed trait BinarySearchTree[A] {
  def isEmpty: Boolean
  def add(a: A): BinarySearchTree[A]
  def contains(a: A): Boolean

  def +(a: A): BinarySearchTree[A] = add(a)
  def nonEmpty = !isEmpty
}