import scala.util.Random

// Sequential implementation
def mcCount(iter: Int): Int = {
    val randomX = new Random
    val randomY = new Random

    val hits: Seq[(Double, Double)] = for {
        i <- 0 until iter
        x = randomX.nextDouble()
        y = randomY.nextDouble()
        if (x*x + y*y < 1)
    } yield (x, y) 

    hits.length
}

mcCount(iter = 10)

// Parallel implementation ("parallel" construct not defined
// yet)
def parallel[A, B](taskA: =>A, taskB: =>B): (A,B) = ???

def monteCarloPiPar(iter: Int): Double = {
    val ((pi1, pi2), (pi3, pi4)) = parallel(
        parallel(mcCount(iter/4), mcCount(iter/4)),
        parallel(mcCount(iter/4), mcCount(iter - 3*(iter/4)))
    )

    4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}