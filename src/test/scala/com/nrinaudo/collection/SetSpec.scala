package com.nrinaudo.collection

import com.nrinaudo.collection.Set._
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.language.higherKinds

class SetSpec[A: Arbitrary, Impl[_]: SetLike](empty: Impl[A]) extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def arbSet: Arbitrary[Impl[A]] = Arbitrary {
    Arbitrary.arbitrary[List[A]].map(create)
  }

  def create(as: A*): Impl[A] = as.foldLeft(empty)((set, a) => implicitly[SetLike[Impl]].insert(a, set))

  describe("An empty Set") {
    it("should be empty") { empty.isEmpty should be(true) }

    it("should not contain any element") {
      forAll { a: A => empty.contains(a) should be(false) }
    }
  }

  describe("A non-empty Set") {
    it("should contain all elements that are added to it") {
      forAll { as: List[A] =>
        def set = create(as:_*)
        as.foreach { set.contains(_) should be(true) }
      }
    }
  }
}
