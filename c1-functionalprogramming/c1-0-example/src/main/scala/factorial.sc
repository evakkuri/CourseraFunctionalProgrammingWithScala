import scala.annotation.tailrec

def factorial(x: Int): Long = {

  @tailrec
  def factorialIter(prev: Int, x: Int): Int = {
    if (x == 0) prev else factorialIter(prev * x, x-1)
  }

  factorialIter(1, x)
}

factorial(3)
factorial(5)
factorial(10)
factorial(11)