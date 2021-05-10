/** A rational number
  * @param num   Numerator
  * @param denom Denominator
  */
case class Rational(num: Int, denom: Int) {
    override def toString: String = num.toString + " / " + denom.toString
}

object RationalOrdering {
  implicit val orderingRational: Ordering[Rational] =
    new Ordering[Rational] {
      def compare(q: Rational, r: Rational): Int =
        q.num * r.denom - r.num * q.denom
    }
}

val r1 = Rational(1, 2)
print(r1)

val r2 = Rational(2, 4)

val r3 = Rational(3, 5)

val list = List(r1, r3, r2)
list.sorted(ord = RationalOrdering.orderingRational)

// Ring type

trait Ring[A] {
  def plus(x: A, y: A): A
  def mult(x: A, y: A): A
  def inverse(x: A): A
  def zero: A
  def one: A
}

object Ring {
  implicit val ringInt: Ring[Int] = new Ring[Int] {
    def plus(x: Int, y: Int): Int = x + y
    def mult(x: Int, y: Int): Int = x * y
    def inverse(x: Int): Int = -x
    def zero: Int = 0
    def one: Int = 1
  }
}

def plusAssociativity[A](x: A, y: A, z: A)(implicit ring: Ring[A]): Boolean =
  ring.plus(ring.plus(x, y), z) == ring.plus(x, ring.plus(y, z))

// Implicit Ordering with implicit parameters
implicit def orderingList[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    new Ordering[List[A]] {
        def compare(xs: List[A], ys: List[A]) =
            (xs, ys) match {
                case (x :: xsTail, y :: ysTail) =>
                    val c = ord.compare(x, y)
                    if (c != 0) c else compare(xsTail, ysTail)
                case (Nil, Nil) => 0
                case (_, Nil)   => 1
                case (Nil, _)   => -1
            }
    }

val xss = List(List(1, 2, 3), List(1), List(1, 1, 3))
xss.sorted

// Sorting by multiple criteria
case class Movie(title: String, rating: Int, duration: Int)

val movies = Seq(
  Movie("Interstellar", 9, 169),
  Movie("Inglourious Basterds", 8, 140),
  Movie("Fight Club", 9, 139),
  Movie("Zodiac", 8, 157)
)

