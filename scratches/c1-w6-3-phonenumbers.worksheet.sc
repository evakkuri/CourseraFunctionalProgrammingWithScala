import scala.io.Source

val sourceUrl = "https://lamp.epfl.ch/wp-content/uploads/2019/01/linuxwords.txt"
val in = Source.fromURL(sourceUrl)

// Get all words in list excluding those with special characters
val words = in.getLines
    .toList
    .filter (word => word forall (char => char.isLetter))

val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode: Map[Char, Char] =
    for {
        (digit, letters) <- mnem
        letter <- letters
    } yield (letter -> digit)

def wordCode(word: String): String = {
    word.toUpperCase map charCode
}

wordCode("java")

val wordsForNum: Map[String, Seq[String]] = {
    words groupBy wordCode withDefaultValue Seq()
}

def encode(numbers: String): Set[List[String]] = {
    if (numbers.isEmpty()) Set(List())
    else {
        for {
            split <- 1 to numbers.length
            word <- wordsForNum(numbers take split)
            rest <- encode(numbers drop split)
        } yield word :: rest
    }.toSet
}

encode("5282")
encode("7225247386")

def translate(numbers: String): Set[String] =
    encode(numbers) map (_ mkString(" "))

translate("7225247386")