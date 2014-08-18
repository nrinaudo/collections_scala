package com.nrinaudo.collection

class LeftistTreeSpec extends HeapSpec[Int, LeftistTree](LeftistTree.empty) {
  override def create(as: Int*): LeftistTree[Int] = LeftistTree(as:_*)
}