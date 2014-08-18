package com.nrinaudo.collection

class BinarySearchTreeSpec extends SetSpec[Int, BinarySearchTree](BinarySearchTree.empty) {
  override def create(as: Int*): BinarySearchTree[Int] = BinarySearchTree(as:_*)
}