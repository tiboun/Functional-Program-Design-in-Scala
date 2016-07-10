package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._
import Gen._
import Arbitrary._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("all elements should be in the collection") = forAll{(a:Int, b:Int) => {
    val heap:H = insert(b,insert(a,empty))
    findMin(heap) == a.min(b)
    findMin(deleteMin(heap)) == a.max(b)
  }}

  property("all elements should be in the collection") = forAll{(a:Int, b:Int) => {
    val heap1:H = insert(a,empty)
    val heap2:H = insert(b,empty)
    val mergedHeap = meld(heap1, heap2)
    findMin(mergedHeap) == a.min(b)
    findMin(deleteMin(mergedHeap)) == a.max(b)
  }}

  property("delete min should delete the min") = forAll{heap:H => {
    val min: Int = findMin(heap)
    findMin(deleteMin(heap)) != min
  }}
}
