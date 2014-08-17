package com.nrinaudo.collection

import com.nrinaudo.collection.LeftistTree.{Leaf, Node}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FunSpec}

object LeftistTreeSpec {
  def node: Gen[Node[Int]] = Arbitrary.arbitrary[Int].map { i => Node(i, 1, Leaf(), Leaf()) }
  def treeData: Gen[List[Int]] = Arbitrary.arbitrary[List[Int]].suchThat(_.nonEmpty)
}

class LeftistTreeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import LeftistTreeSpec._

  describe("A leaf") {
    it("should be empty") { LeftistTree.empty[Int].isEmpty should be(true) }
    it("should fail to return its minimum value") {
      intercept[UnsupportedOperationException](LeftistTree.empty[Int].min)
    }
    it("should fail to delete its minimum value") {
      intercept[UnsupportedOperationException](LeftistTree.empty[Int].deleteMin())
    }
    it("should have a rank of 0") { LeftistTree.empty[Int].rank should be(0) }
  }

  describe("A node") {
    it("should not be empty") {
      forAll(node) { node => node.isEmpty should be(false) }
    }

    it("should return its value as its min") {
      forAll(node) { node => node.min should be(node.value) }
    }

    it("should accept deleting its minimum value") {
      forAll(node) { node => node.deleteMin() }
    }
  }

  describe("A tree") {
    it("should return the correct minimum value") {
      forAll(treeData) { data => LeftistTree(data :_*).min should be(data.min) }
    }

    it("should return a valid leftist tree when deleteMin is called") {
      forAll(treeData) { data =>
        val tree = LeftistTree(data :_*).deleteMin()

        if(data.length == 1)
          intercept[UnsupportedOperationException](tree.min)
        else
          tree.min should be(data.sorted.tail.min)
      }
    }
  }
}
