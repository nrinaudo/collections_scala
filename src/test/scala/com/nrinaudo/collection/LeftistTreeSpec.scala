package com.nrinaudo.collection

import com.nrinaudo.collection.LeftistTree.{Leaf, Node}
import org.scalacheck.Arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

object LeftistTreeSpec {
  implicit def arbNode[T: Ordering: Arbitrary]: Arbitrary[Node[T]] = Arbitrary {
    Arbitrary.arbitrary[T].map { t => Node(t, 1, Leaf(), Leaf()) }
  }

  implicit def arbTree[T: Ordering: Arbitrary]: Arbitrary[LeftistTree[T]] = Arbitrary {
    Arbitrary.arbitrary[List[T]].map { ts => LeftistTree(ts :_*)}
  }
}

class LeftistTreeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import com.nrinaudo.collection.LeftistTreeSpec._

  describe("A leaf") {
    it("should be empty") { LeftistTree.empty[Int].isEmpty should be(true) }

    it("should not return a minimum value") {
      LeftistTree.empty[Int].min.isDefined should be(false)
    }

    it("should fail to delete its minimum value") {
      intercept[UnsupportedOperationException](LeftistTree.empty[Int].deleteMin())
    }
    it("should have a rank of 0") { LeftistTree.empty[Int].rank should be(0) }
  }

  describe("A node") {
    it("should not be empty") {
      forAll { node: Node[Int] => node.nonEmpty should be(true) }
    }

    it("should return its value as its min") {
      forAll { node: Node[Int] => node.min should be(Some(node.value)) }
    }

    it("should accept deleting its minimum value") {
      forAll { node: Node[Int] => node.deleteMin().isEmpty should be(true) }
    }
  }

  describe("A tree") {
    it("should unfold in ascending order") {
      def step[T: Ordering](tree: LeftistTree[T], prev: T): Unit = {
        if(tree.nonEmpty) {
          implicitly[Ordering[T]].gteq(tree.min.get, prev) should be(true)
          step(tree.deleteMin(), tree.min.get)
        }
      }

      forAll { ts: LeftistTree[Int] =>
        whenever(ts.nonEmpty) {
          step(ts.deleteMin(), ts.min.get)
        }
      }
    }
  }
}
