val word: String = "testword"
val s = List("word1", "word2", "word3")

word.groupBy((char: Char) => char).map { case (key: Char, occs: String) => (key, occs.length) }

//s mkString

val testOccs = List(('a', 2), ('b', 2))
type Occurrences = List[(Char, Int)]

testOccs take 0
testOccs drop 2

def combinations(occurrences: Occurrences): List[Occurrences] = {
    
    def combsHelper(occs: Occurrences): List[Occurrences] = 
        if (occs.isEmpty) List(List())
        else {
            for {
                split <- 1 to occs.length
                start <- occs take split
                n <- 1 to start._2
                rest <- combsHelper(occs drop split)
            } yield (start._1, n) :: rest
        }.toList

    val combs = combsHelper(occurrences)
    
    if (combs.head == List()) combs
    else combs ::: List(Nil)
}

combinations(Nil)
combinations(List(('a', 1), ('b', 1)))
combinations(List(('a', 2), ('b', 2)))
combinations(List(('a', 1), ('b', 2), ('c', 3)))
combinations(List(('a', 1), ('b', 1), ('c', 1)))

val testList = List(List(('a', 1), ('b', 1)), List(('a', 1)), List(('b', 1)))
testList ::: List(Nil)