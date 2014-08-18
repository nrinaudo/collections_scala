package com.nrinaudo.collection

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import com.nrinaudo.collection.Heap._

import scala.language.higherKinds

class HeapSpec[A: Arbitrary: Ordering, Impl[_]: HeapLike](empty: Impl[A]) extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def arbHeap: Arbitrary[Impl[A]] = Arbitrary {
    Arbitrary.arbitrary[List[A]].map(create)
  }

  def create(as: A*): Impl[A] = as.foldLeft(empty)((heap, a) => implicitly[HeapLike[Impl]].insert(a, heap))

  describe("An empty Heap") {
    it("should be empty") {
      empty.isEmpty should be(true)
    }

    it("should not have a min value") {
      empty.min.isDefined should be(false)
    }

    it("should fail to delete its minimum value") {
      intercept[UnsupportedOperationException](empty.deleteMin())
    }

    it("should yield the correct minimum value after insertion of an element") {
      forAll { a: A =>
        empty.insert(a).min should be(Some(a))
        empty.insert(a).deleteMin().isEmpty should be(true)
      }
    }

  }

  describe("A non-empty Heap") {
    it("should unfold in ascending order") {
      def step[T: Ordering](heap: Heap[T], prev: T): Unit = {
        if(heap.nonEmpty) {
          implicitly[Ordering[T]].gteq(heap.min.get, prev) should be(true)
          step(heap.deleteMin(), heap.min.get)
        }
      }

      forAll { ts: Impl[A] =>
        whenever(ts.nonEmpty) {
          step(ts.deleteMin(), ts.min.get)
        }
      }
    }
  }
}
