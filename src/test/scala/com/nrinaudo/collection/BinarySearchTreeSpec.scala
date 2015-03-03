package com.nrinaudo.collection

import BinarySearchTree._

class BinarySearchTreeSpec extends SetSpec[Int, BinarySearchTree](BinarySearchTree.empty) {
  override def create(as: Int*): BinarySearchTree[Int] = BinarySearchTree(as:_*)

  // The only Binary Search Tree invariant is that all elements to the left of the root are smaller than the root,
  // all elements to its right are larger.
  override def checkInvariants(as: BinarySearchTree[Int]): Unit = isSorted(as) should be(true)


  def isSorted(as: BinarySearchTree[Int]): Boolean = as match {
      // Non empty tree, recursively makes sure everything in l is smaller than v and everything in r larger than v.
    case Node(v, l, r) =>
      (for {
        smallerLeft <- l.max.map(_ < v).orElse(Some(true))
        smallerRight <- r.min.map(_ > v).orElse(Some(true))
      } yield smallerLeft && smallerRight && isSorted(l) && isSorted(r)).get

    // Empty tree, nothing to test.
    case _ => true
  }

  describe("A BinarySearchTree") {
    it("should have a working delete implementation") {
      forAll { as: List[Int] =>
        val tree = create(as:_*)

        as foreach { a =>
          val t = tree.remove(a)

          checkInvariants(t)
          t.contains(a) should be(false)
          as.filter(_ != a).foreach { t.contains(_) should be(true) }
        }
      }
    }
  }
}