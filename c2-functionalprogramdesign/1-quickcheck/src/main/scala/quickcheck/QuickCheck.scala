package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  private val emptyHeap = empty
  private val emptyHeapGen = const(emptyHeap)

  lazy val genHeap: Gen[H] = oneOf(
    emptyHeapGen,
    for {
      value <- arbitrary[Int]
      newHeap <- oneOf(emptyHeapGen, genHeap)
    } yield insert(value, newHeap)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /*
  If you insert any two elements into an empty heap, finding the minimum
  of the resulting heap should get the smallest of the two elements back.

  In addition, the next minimum after deleting the first minimum should be
  the other of the two elements (bogus heap #3 gets this wrong).
  */
  property("min2") = forAll { (a: Int, b: Int, c: Int) =>
    (a < b) ==> {
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      val firstMinCorrect = findMin(h2) == a
      val secondMinCorrect = findMin(deleteMin(h2)) == b
      firstMinCorrect & secondMinCorrect
    }
  }

  /*
  If you insert an element into an empty heap, then delete the minimum,
  the resulting heap should be empty.
  */
  property("delmin") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  /*
  Given any heap, you should get a sorted sequence of elements when
  continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  */
  def confirmOrder (heap: H): Boolean = {

    def findMinLoop(prevMin: Int, heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val newMin = findMin(heap)
        val newHeap = deleteMin(heap)
        ord.lteq(prevMin, newMin) & findMinLoop (newMin, newHeap)
      }
    }

    if (isEmpty(heap)) true
    else findMinLoop( findMin (heap), deleteMin(heap) )
  }

  property("returnsInSortedOrder") = forAll { (h: H) =>
    confirmOrder(h)
  }

  /*
  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  */

  property("meldedMins") = forAll { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)

    if (isEmpty(h1) & !isEmpty(h2)) {
      val min2 = findMin(h2)
      val meldedMin = findMin(meldedHeap)
      meldedMin == min2
    } else if (isEmpty(h2) & !isEmpty(h1)) {
      val min1 = findMin(h1)
      val meldedMin = findMin(meldedHeap)
      meldedMin == min1
    } else if (isEmpty(h1) & isEmpty(h2)) {
      true
    } else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val meldedMin = findMin(meldedHeap)
      meldedMin == (if (min1 < min2) min1 else min2)
    }
  }

  /*
  Bogus heap #4 does not handle deletes correctly, when there are at least 3 objects with increasing
  value on the heap. Test that property.
  */
  val genThreeInts = for {
    a <- Gen.choose(1, 100)
    b <- Gen.choose(a + 1, 101)
    c <- Gen.choose(b + 1, 102)
  } yield (a, b, c)

  property("delMinAfterLinkOperation") = forAll(genThreeInts) { intTuple: (Int, Int, Int) =>
    val h1 = insert(intTuple._1, empty)
    val h2 = insert(intTuple._2, h1)
    val h3 = insert(intTuple._3, h2)

    findMin(h3) == intTuple._1 &
      findMin(deleteMin(h3)) == intTuple._2 &
      findMin(deleteMin(deleteMin(h3))) == intTuple._3
  }
} 