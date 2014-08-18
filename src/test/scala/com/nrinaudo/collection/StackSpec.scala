package com.nrinaudo.collection

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import Stack._

import scala.language.higherKinds

class StackSpec[A: Arbitrary, Impl[_]: StackLike](empty: Impl[A]) extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def arbStack: Arbitrary[Impl[A]] = Arbitrary {
    Arbitrary.arbitrary[List[A]].map(create)
  }

  def create(as: A*): Impl[A] = as.foldRight(empty)((a, set) => implicitly[StackLike[Impl]].push(a, set))

  describe("An empty Stack") {
    it("should be empty") { empty.isEmpty should be(true) }
    it("should not have a top") { empty.top.isDefined should be(false) }
    it("should fail to pop") { intercept[UnsupportedOperationException](empty.pop) }
    it("should pop the correct value after a push") {
      forAll { a: A =>
        empty.push(a).top should be(Some(a))
        empty.push(a).pop().isEmpty should be(true)
      }
    }
  }
}

class ListStackSpec extends StackSpec[Int, List](Nil)