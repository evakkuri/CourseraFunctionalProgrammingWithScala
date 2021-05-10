// Parallel calculation of p-norm

import scala.math
import scala.concurrent.{Await,Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Double = {
    a.slice(s, t).map((x: Int) => math.pow(math.abs(x), p)).sum
}

def pNorm (a: Array[Int], p: Double): Double = {
    math.pow(sumSegment(a, p, 0, a.length), 1/p)
}

// Calculation of the sum segments can be split into an arbitrary number of
// parts
def pNormTwoPart(a: Array[Int], p: Double): Double = {
    val m = a.length / 2
    println(m)

    val (sum1, sum2) = (
        sumSegment(a, p, 0, m),
        sumSegment(a, p, m, a.length)
    )

    println(sum1, sum2)

    math.pow(sum1 + sum2, 1/p)
}

// Recursive function to split the array to an arbitrary number of pairs of
// threads
def parallel[A, B](taskA: =>A, taskB: =>B): (A,B) = {
  val fB: Future[B] = Future { taskB }
  val a: A = taskA
  val b: B = Await.result(fB, Duration.Inf)
  (a,b)
}

def pNormRec(a: Array[Int], p: Double): Double =
    math.pow(segmentRec(a, p, 0, a.length), 1/p)

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int, threshold: Int = 2): Double = {
    if (t - s < threshold)
        sumSegment(a, p, s, t) // For small segments, run sequential calculation
    else {
        val m = s + (t - s) / 2
        val (sum1, sum2) = parallel(
            segmentRec(a, p, s, m),
            segmentRec(a, p, m, t)
        )
        sum1 + sum2
    }
}

// Testing
val testArr = Array(3, 4)
pNorm(testArr, 2)
pNormTwoPart(testArr, 2)

pNormRec(testArr, 2)
//pNormRec(Array(3, 4, 5, 6), 2)