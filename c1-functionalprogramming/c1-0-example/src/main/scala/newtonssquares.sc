def abs(x: Double): Double = if (x < 0) -x else x

def sqrt(x: Double, initialGuess: Double = 1.0): Double = {

  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))
  }

  def isGoodEnough(guess: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }

  def improve(guess: Double): Double = {
    (guess + x / guess) / 2
  }

  sqrtIter(initialGuess)
}

sqrt(3)
sqrt(4)
sqrt(25)
sqrt(1e-6)
sqrt(1e60)