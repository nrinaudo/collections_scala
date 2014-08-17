package com.nrinaudo.collection

import scala.language.higherKinds

trait Set[A] extends (A => Boolean) {
  def isEmpty: Boolean
  def insert(a: A): Set[A]
  def contains(a: A): Boolean

  override def apply(a: A): Boolean = contains(a)
}

object Set {
  def apply[A, Impl[_]: SetLike](set: Impl[A]) = Wrapped(set)

  implicit class Wrapped[A, Impl[_]: SetLike](val set: Impl[A]) extends Set[A] {
    private lazy val setLike = implicitly[SetLike[Impl]]

    override def isEmpty        = setLike.isEmpty(set)
    override def insert(a: A)   = new Wrapped(setLike.insert(a, set))
    override def contains(a: A) = setLike.contains(a, set)
  }
}

trait SetLike[Impl[_]] {
  def isEmpty[A](as: Impl[A]): Boolean
  def insert[A](a: A, as: Impl[A]): Impl[A]
  def contains[A](a: A, as: Impl[A]): Boolean
}