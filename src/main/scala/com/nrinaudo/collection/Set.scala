package com.nrinaudo.collection

import scala.language.higherKinds

trait Set[A] extends (A => Boolean) {
  def isEmpty       : Boolean
  def insert(a: A)  : Set[A]
  def contains(a: A): Boolean

  override def apply(a: A): Boolean = contains(a)
  def nonEmpty = !isEmpty
  @inline def +(a: A): Set[A] = insert(a)
}

object Set {
  def apply[A, Impl[_]: SetLike](set: Impl[A]) = Wrapped(set)

  implicit class Wrapped[A, Impl[_]](val set: Impl[A])(implicit setLike: SetLike[Impl]) extends Set[A] {
    override def isEmpty        = setLike.isEmpty(set)
    override def insert(a: A)   = new Wrapped(setLike.insert(a, set))
    override def contains(a: A) = setLike.contains(a, set)
  }
}

trait SetLike[Impl[_]] {
  def isEmpty[A](as: Impl[A])       : Boolean
  def insert[A](a: A, as: Impl[A])  : Impl[A]
  def contains[A](a: A, as: Impl[A]): Boolean
}