package com.nrinaudo.collection

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import com.nrinaudo.collection.Heap._

import scala.language.higherKinds

class HeapSpecs[A: Arbitrary: Ordering, Impl[_]: HeapLike](empty: Impl[A])
  extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def arbHeap: Arbitrary[Impl[A]] = Arbitrary {
    Arbitrary.arbitrary[List[A]].map(create)
  }

  def create(as: A*): Impl[A] = as.foldLeft(empty)((queue, a) => implicitly[HeapLike[Impl]].insert(a, queue))

  describe("An empty Heap") {
    it("should be empty") {
      empty.isEmpty should be(true)
    }

    it("should not have a min value") {
      empty.findMin.isDefined should be(false)
    }

    it("should fail to delete its minimum value") {
      intercept[UnsupportedOperationException](empty.deleteMin())
    }

    it("should yield the correct minimum value after insertion of an element") {
      forAll { a: A =>
        empty.insert(a).findMin should be(Some(a))
        empty.insert(a).deleteMin().isEmpty should be(true)
      }
    }

  }

  describe("A non-empty Heap") {
    it("should unfold in ascending order") {
      def step[T: Ordering](queue: Heap[T], prev: T): Unit = {
        if(queue.nonEmpty) {
          implicitly[Ordering[T]].gteq(queue.findMin.get, prev) should be(true)
          step(queue.deleteMin(), queue.findMin.get)
        }
      }

      forAll { ts: Impl[A] =>
        whenever(ts.nonEmpty) {
          step(ts.deleteMin(), ts.findMin.get)
        }
      }
    }
  }
}
