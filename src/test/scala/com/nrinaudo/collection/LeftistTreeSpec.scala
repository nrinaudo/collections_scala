package com.nrinaudo.collection

class LeftistTreeSpec extends HeapSpecs[Int, LeftistTree](LeftistTree.empty) {
  override def create(as: Int*): LeftistTree[Int] = LeftistTree(as:_*)
}