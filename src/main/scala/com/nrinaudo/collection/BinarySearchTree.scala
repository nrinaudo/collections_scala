package com.nrinaudo.collection

object BinarySearchTree {
  // - Smart constructors ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def empty[A: Ordering]: BinarySearchTree[A] = Leaf()
  def apply[A: Ordering](as: A*): BinarySearchTree[A] = as.foldLeft(empty)(_.add(_))



  // - Normal node -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
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

    override def min: Option[A] = left.min.orElse(Some(value))
    override def max: Option[A] = right.max.orElse(Some(value))
  }


  // - Leaf node -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Leaf[A: Ordering]() extends BinarySearchTree[A] {
    override def isEmpty        = true
    override def add(a: A)      = Node(a, this, this)
    override def contains(a: A) = false
    override def min: Option[A] = None
    override def max: Option[A] = None
  }



  // - Type class instances --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
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
  def min: Option[A]
  def max: Option[A]
}