package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def balanceIter(unmatchedOpens: Int, unmatchedCloses: Int, chars: Array[Char]): Boolean = {
      if (chars.isEmpty) unmatchedOpens == 0 && unmatchedCloses == 0
      else chars.head match {
        case '(' => balanceIter(unmatchedOpens + 1, unmatchedCloses, chars.tail)
        case ')' =>
          if (unmatchedOpens > 0) balanceIter(unmatchedOpens - 1, unmatchedCloses, chars.tail)
          else balanceIter(unmatchedOpens, unmatchedCloses + 1, chars.tail)
        case _ => balanceIter(unmatchedOpens, unmatchedCloses, chars.tail)
      }
    }

    balanceIter(0, 0, chars)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, unmatchedOpens: Int, unmatchedCloses: Int): (Int, Int) = {
      if (idx == until) (unmatchedOpens, unmatchedCloses)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, unmatchedOpens + 1, unmatchedCloses)
        case ')' =>
          if (unmatchedOpens > 0) traverse(idx + 1, until, unmatchedOpens - 1, unmatchedCloses)
          else traverse(idx + 1, until, unmatchedOpens, unmatchedCloses + 1)
        case _ => traverse(idx + 1, until, unmatchedOpens, unmatchedCloses)
      } 
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      println("Processing from " + from + " to " + until)
      if ((until - from) <= threshold) {
        println("Traversing")
        traverse(from, until, 0, 0)
      }
      else {
        println("Splitting")
        val mid = from + (until - from) / 2
        val ((uO1, uC1), (uO2, uC2)) = reductions.parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        println((uO1, uC1), (uO2, uC2), (uO1 + uO2 - uC2, uC1 + uC2 - uO1))
        println("Combining " + (from, mid) + " and " + (mid, until))
        (uO1 + uO2 - uC2, uC1 + uC2 - uO1) 
      }

    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
