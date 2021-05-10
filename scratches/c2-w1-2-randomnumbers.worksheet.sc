// Random element generators
trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
        def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
        def generate: S = f(self.generate).generate
    }
}

val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate: Int = rand.nextInt()
}

integers.generate

val booleans = for (x <- integers) yield x > 0

booleans.generate

def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
} yield (x, y)

pairs(integers, booleans).generate

def single[T](x: T): Generator[T] = new Generator[T] {
    def generate: T = x
}

def choose(low: Int, high: Int): Generator[Int] =
    for (x <- integers) yield low + x % (high - low)

def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

//oneOf("red", "blue", "yellow").generate

// Random lists
def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
} yield list

def emptyLists = single(Nil)

def nonEmptyLists = for {
    head <- integers
    tail <- lists
} yield head :: tail

lists.generate

// Random trees
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = for {
    x <- integers
} yield Leaf(x)

def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
} yield Inner(l, r)

def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
} yield tree

trees.generate
