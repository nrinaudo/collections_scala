package com.nrinaudo.collection

import Ordering.Implicits._

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
      if(a < value)      copy(left  = left + a)
      else if(a > value) copy(right = right + a)
      else               this

    override def contains(a: A) =
      if(a < value)      left.contains(a)
      else if(a > value) right.contains(a)
      else               true

    override def remove(a: A) =
      // Looks for a node that matches the specified value.
      if(a < value)      copy(left  = left - a)
      else if(a > value) copy(right = right - a)

      // Found the node. If it has an empty left or right child, we can use the other as the parent's new descendant
      else if(left.isEmpty)  right
      else if(right.isEmpty) left

      // It does not have an empty child, we need to get slightly funkier: remove the right child's min, replace the
      // current node's value with said min. While the tree's topology changes, all invariants are still respected.
      else {
        var rmin = value
        def step(as: BinarySearchTree[A]): BinarySearchTree[A] = as match {
          case Node(v, l@Node(_, _, _), r) => Node(v, step(l), r)
          case Node(v, l,               r) => rmin = v; r
        }
        val newr = step(right)
        Node(rmin, left, newr)
      }


    override def min: Option[A] = left.min.orElse(Some(value))
    override def max: Option[A] = right.max.orElse(Some(value))
  }


  // - Leaf node -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  case class Leaf[A: Ordering]() extends BinarySearchTree[A] {
    override def isEmpty        = true
    override def add(a: A)      = Node(a, this, this)
    override def remove(a: A)   = this
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
  def min: Option[A]
  def max: Option[A]
  def remove(a: A): BinarySearchTree[A]

  @inline def +(a: A): BinarySearchTree[A] = add(a)
  @inline def -(a: A): BinarySearchTree[A] = remove(a)
  @inline def nonEmpty = !isEmpty
}