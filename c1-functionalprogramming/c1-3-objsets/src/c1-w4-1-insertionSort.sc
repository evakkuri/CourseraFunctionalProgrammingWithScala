def insert(x: Int, xs: List[Int]): List[Int] = {
  xs match {
    case List() => x :: xs
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}

def isort(xs: List[Int]): List[Int] = {
  xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }
}

val testList = List(5, 1, 3, 2, 4)
isort(testList)
