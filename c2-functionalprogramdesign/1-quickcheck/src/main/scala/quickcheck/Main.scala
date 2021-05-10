package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object Main extends App {
    val i1 = (new IntHeap with BinomialHeap).empty
    //println(i1)

    val ih = new IntHeap with BinomialHeap
    ih.insert(5, i1)

    val testheap = new QuickCheckHeap with BinomialHeap
    for {
        n <- 0 until 0
        heap <- testheap.genHeap.sample
        val _ = println(heap)
    } yield heap

    val sortingTestHeap = ih.insert(
        5, ih.insert(
            4, ih.insert(
                3, ih.insert(
                    2, ih.insert(
                        1, ih.empty
                    )
                )
            )
        )
    )

    /*
    val sth1 = ih.insert(3, ih.empty)
    println(sth1)
    val sth2 = ih.insert(4, sth1)
    println(sth2)
    val sth3 = ih.insert(1, sth2)
    println(sth3)
    val sth4 = ih.insert(4, sth3)
    println(sth4)
    val sth5 = ih.insert(-1, sth4)
    println(sth5)
     */

    /*
    val bth3 = new IntHeap with Bogus3BinomialHeap
    val bth3_1 = bth3.insert(3, bth3.empty)
    println(bth3_1)
    val bth3_2 = bth3.insert(-1, bth3_1)
    println(bth3_2)
    val bth3_3 = bth3.insert(4, bth3_2)
    println(bth3_3)
     */

    val bth4 = new IntHeap with Bogus4BinomialHeap
    val bth4_1 = bth4.insert(3, bth4.empty)
    println(bth4_1)
    val bth4_2 = bth4.insert(4, bth4_1)
    println(bth4_2)
    val bth4_3 = bth4.insert(5, bth4_2)
    println(bth4_3)
    val bth4_4 = bth4.deleteMin(bth4_3)
    println(bth4_4)
    val bth4_5 = bth4.deleteMin(bth4_4)
    println(bth4_5)

    //println(sortingTestHeap)
    //println(ih.findMin(sortingTestHeap))
    //val sth = ih.deleteMin(sortingTestHeap)
    //println(ih.findMin(sth2))

    /*
    Function sortingTestGetter docstring goes here
    */
    def confirmOrder (heap: testheap.H): Boolean = {

        def findMinLoop(prevMin: Int, heap: testheap.H): Boolean = {
            heap match {
                case Nil => true
                case head :: tail =>
                    val newMin = testheap.findMin (heap)
                    val newHeap = testheap.deleteMin (heap)
                    println(newMin, newHeap)
                    prevMin < newMin & findMinLoop (newMin, newHeap)
            }
        }

        if (testheap.isEmpty(heap)) true
        else findMinLoop( testheap.findMin (heap), testheap.deleteMin(heap) )
    }

    for {
        n <- 0 until 0
        heap <- testheap.genHeap.sample
        val _ = println(heap)
    } yield confirmOrder(heap)

}