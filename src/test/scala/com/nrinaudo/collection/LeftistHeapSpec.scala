package com.nrinaudo.collection

class LeftistHeapSpec extends HeapSpec[Int, LeftistHeap](LeftistHeap.empty) {
  override def create(as: Int*): LeftistHeap[Int] = LeftistHeap(as:_*)
}