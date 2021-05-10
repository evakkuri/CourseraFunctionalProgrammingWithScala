def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0 )

isPrime(3)
isPrime(4)

val n = 7

(1 until n)
.flatMap (i => (1 until i) map (j => (i, j)))
.filter(pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

// Scalar product
def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ( (x, y) <- xs zip ys ) yield x * y)
    .sum

class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ other.terms)

  override def toString(): String =
    (for ((exp, coeff) <- terms) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))