package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    m <- Gen.oneOf(const(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should
    * get the smallest of the two elements back.
    */
  property("minof2") = forAll { (a: A, b: A) =>
    val heap = meld(insert(a, empty), insert(b, empty))
    findMin(heap) == a || findMin(heap) == b
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap
    * should be empty.
    */
  property("Insert in empty, then delete should be empty") = forAll { a: A =>
    deleteMin(insert(a, empty)) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence
    * of elements when continually finding and deleting
    * minima. (Hint: recursion and helper functions are
    * your friends.)
    */
  property("sortedheap") = forAll { h: H =>
    def isSorted(heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val m = findMin(heap)
        val h2 = deleteMin(heap)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    }
    isSorted(h)
  }

  property("deleteMin should not work on empty heap") = forAll { h: H =>
    def deleteWhileNonEmpty(heap: H): H = {
      if (isEmpty(heap)) empty
      else deleteWhileNonEmpty(deleteMin(heap))
    }
    val hp = deleteWhileNonEmpty(h)
    deleteMin(hp) == empty
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("minof2meld") = forAll { (ha: H, hb: H) =>
    val mina = findMin(ha)
    val minb = findMin(hb)
    findMin(meld(ha, hb)) == mina || findMin(meld(ha, hb)) == minb
  }
}