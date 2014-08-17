package com.nrinaudo.collection

import com.nrinaudo.collection.BinarySearchTree.{Node, Leaf}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}

object BinarySearchTreeSpec {
  implicit def arbLeaf[T: Ordering]: Arbitrary[Leaf[T]] = Arbitrary {
    Gen.const(BinarySearchTree.Leaf[T]())
  }

  implicit def arbNode[T: Ordering: Arbitrary]: Arbitrary[Node[T]] = Arbitrary {
    Arbitrary.arbitrary[T].map(t => BinarySearchTree.Node(t, Leaf(), Leaf()))
  }

  implicit def arbTree[T: Ordering: Arbitrary]: Arbitrary[BinarySearchTree[T]] = Arbitrary {
    Arbitrary.arbitrary[List[T]].map {ts => BinarySearchTree(ts :_*)}
  }
}

class BinarySearchTreeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import BinarySearchTreeSpec._

  describe("A leaf") {
    it("should be empty") { BinarySearchTree.empty[Int].isEmpty should be(true) }
    it("should not contain any element") {
      forAll { value: Int => BinarySearchTree.empty[Int].contains(value) should be(false) }
    }
  }

  describe("A node") {
    it("should not be empty") {
      forAll { node: Node[Int] => node.nonEmpty should be(true) }
    }

    it("should contain its own value") {
      forAll { node: Node[Int] => node.contains(node.value) should be(true) }
    }
  }

  describe("A tree") {
    it("should contain any element that is added to it") {
      forAll { (i: Int, tree: BinarySearchTree[Int]) =>
        whenever(!tree.contains(i)) {
          (tree + i).contains(i) should be(true)
        }
      }
    }
  }
}
