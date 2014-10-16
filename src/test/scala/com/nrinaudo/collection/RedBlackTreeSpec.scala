package com.nrinaudo.collection

import RedBlackTree._

class RedBlackTreeSpec extends SetSpec[Int, RedBlackTree](RedBlackTree.empty) {
  override def create(as: Int*): RedBlackTree[Int] = RedBlackTree(as:_*)
  
  def isSorted[A: Ordering](as: RedBlackTree[A]): Boolean = as match {
        // Non empty tree, recursively makes sure everything in l is smaller than v and everything in r larger than v.
      case Node(v, _, l, r) =>
        val ord = implicitly[Ordering[A]]
  
        (for {
          smallerLeft <- l.max.map(ord.lt(_, v)).orElse(Some(true))
          smallerRight <- r.min.map(ord.gt(_, v)).orElse(Some(true))
        } yield smallerLeft && smallerRight && isSorted(l) && isSorted(r)).get
  
      // Empty tree, nothing to test.
      case _ => true
    }
  
    describe("A RedBlackTree") {
      it("should always store smaller elements on the left and bigger ones on the right") {
        forAll { tree: RedBlackTree[Int] => isSorted(tree) should be(true) }
      }
    }
}