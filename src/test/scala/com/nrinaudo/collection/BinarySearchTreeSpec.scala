package com.nrinaudo.collection

import BinarySearchTree._

class BinarySearchTreeSpec extends SetSpec[Int, BinarySearchTree](BinarySearchTree.empty) {
  override def create(as: Int*): BinarySearchTree[Int] = BinarySearchTree(as:_*)

  def isSorted[A: Ordering](as: BinarySearchTree[A]): Boolean = as match {
      // Non empty tree, recursively makes sure everything in l is smaller than v and everything in r larger than v.
    case Node(v, l, r) =>
      val ord = implicitly[Ordering[A]]

      (for {
        smallerLeft <- l.max.map(ord.lt(_, v)).orElse(Some(true))
        smallerRight <- r.min.map(ord.gt(_, v)).orElse(Some(true))
      } yield smallerLeft && smallerRight && isSorted(l) && isSorted(r)).get

    // Empty tree, nothing to test.
    case _ => true
  }

  describe("A BinarySearchTree") {
    it("should always store smaller elements on the left and bigger ones on the right") {
      forAll { tree: BinarySearchTree[Int] => isSorted(tree) should be(true) }
    }
  }
}