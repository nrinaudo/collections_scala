package com.nrinaudo.collection

import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}
import com.nrinaudo.collection.PriorityQueue._

import scala.language.higherKinds

class PriorityQueueSpec[A: Arbitrary: Ordering, Impl[_]: PriorityQueueLike](empty: Impl[A])
  extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def arbPriorityQueue: Arbitrary[Impl[A]] = Arbitrary {
    Arbitrary.arbitrary[List[A]].map(create)
  }

  def create(as: A*): Impl[A] = as.foldLeft(empty)((queue, a) => implicitly[PriorityQueueLike[Impl]].insert(a, queue))

  describe("An empty PriorityQUeue") {
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

  describe("A non-empty PriorityQueue") {
    it("should unfold in ascending order") {
      def step[T: Ordering](queue: PriorityQueue[T], prev: T): Unit = {
        if(queue.nonEmpty) {
          implicitly[Ordering[T]].gteq(queue.min.get, prev) should be(true)
          step(queue.deleteMin(), queue.min.get)
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
