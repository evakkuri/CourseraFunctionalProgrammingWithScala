def mapSeq[A, B](lst: List[A], f: A => B): List[B] = lst match {
    case Nil => Nil
    case head :: tl => f(head) :: mapSeq(tl, f)
}

def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int,
    f: A => B, out: Array[B]): Unit = {
        var i = left
        while (i < right) {
            out(i) = f(inp(i))
            i = i + 1
        }
    }

val in = Array(2, 3, 4, 5, 6)
val out = Array(0, 0, 0, 0, 0)
val f = (x: Int) => x * x
mapASegSeq(in, 1, 3, f, out)
out