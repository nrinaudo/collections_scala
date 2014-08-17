package com.nrinaudo.collection

import com.nrinaudo.collection.BinarySearchTree.{Node, Leaf}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}

object BinarySearchTreeSpec {
  def leaf: Gen[Leaf[Int]] = Gen.const(BinarySearchTree.Leaf[Int]())
  def node: Gen[Node[Int]] = Arbitrary.arbitrary[Int].map(i => BinarySearchTree.Node(i, Leaf(), Leaf()))
  def treeData: Gen[List[Int]] = Arbitrary.arbitrary[List[Int]].suchThat(_.nonEmpty)
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
      forAll(node) { node => node.isEmpty should be(false) }
    }

    it("should contain its own value") {
      forAll(node) { node => node.contains(node.value) should be(true) }
    }
  }

  describe("A tree") {
    it("should contain all of its elements") {
      forAll(treeData) { data =>
        val tree = BinarySearchTree(data: _*)

        data foreach { i => tree.contains(i) should be(true) }
      }
    }

    it("should respect its invariants when built incrementally") {
      forAll(treeData) { data =>
        data.foldLeft((BinarySearchTree.empty[Int], List[Int]())) {
          case ((t1, c1), curr) =>
            val t2 = t1 + curr
            val c2 = curr :: c1

            c2 foreach { i => t2.contains(i) should be(true) }

            (t2, c2)
        }
      }
    }
  }
}
