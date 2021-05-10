package calculator
import scala.math.{sqrt, pow}

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set()
      else {
        val plusSolution = (-1 * b() + sqrt(delta())) / (2 * a())
        val minusSolution = (-1 * b() - sqrt(delta())) / (2 * a())
        Set(plusSolution, minusSolution)
      }
    }
  }
}
