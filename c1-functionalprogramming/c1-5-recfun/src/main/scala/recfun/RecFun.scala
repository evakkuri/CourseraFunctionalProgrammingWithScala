package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println(countChange(4, List(2,1)))
    //println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (c < 0 || c > r+1) 0
    else if (r == 0 && c == 0) 1
    else pascal(c-1, r-1) +
      pascal(c, r-1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceIter(unmatchedOpens: Int, unmatchedCloses: Int, chars: List[Char]): Boolean = {
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

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeIter(acc: Int, money: Int, coins: List[Int]): Int = {
      if (money == 0) acc + 1
      else if (money < 0 || coins.isEmpty) acc
      else countChangeIter(acc, money - coins.head, coins) +
        countChangeIter(acc, money, coins.tail)
    }

    countChangeIter(0, money, coins)

  }
}
