// A Scala Stream is much like a list, but the tail is not evaluated completely
// at the time of definition. Instead values are evaluated as needed based on
// calls.

// Different ways to construct streams
val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
Stream(1, 2, 3)
(1 to 1000).toStream

// Function examples
def streamRange(lo: Int, hi: Int): Stream[Int] =
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))

streamRange(1, 10)

// Streams support almost all methods of List
def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0 )
((1000 to 10000).toStream filter isPrime)(1)

// The exception is cons (::), which always produces a list.
// However, there is alternative operator #:: which produces a stream
1 #:: xs

// Another example
def streamRange2(lo: Int, hi:Int): Stream[Int] = {
    print(lo + " ")
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange2(1, 10).take(3).toList

// Lazy evaluation

// With lazy evaluation, variable values are only evaluated when called, but
// afterwards the results are cached for reuse. This is the difference to def.
// Example:
def expr = {
    val x = { print("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    z + y + x + z + y + x 
}
expr

// Infinite streams

// Streams can be used to define infinite sequences
def from(n: Int): Stream[Int] = n #:: from(n+1)
val nats = from(0)
val multiplesOfFour = nats map (_ * 4)

// Sieve of Eratosthenes
def sieve(s: Stream[Int]): Stream[Int] =
    s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))
primes.take(100)

// Improved square root approximation algorithm
def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
}

sqrtStream(4).take(10)

def isGoodEnough(guess: Double, x: Double): Boolean =
    math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10)