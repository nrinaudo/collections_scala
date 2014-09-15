package com.nrinaudo.collection

class BinomialHeapSpec extends HeapSpec[Int, BinomialHeap](BinomialHeap.empty) {
  override def create(as: Int*): BinomialHeap[Int] = BinomialHeap(as:_*)
}