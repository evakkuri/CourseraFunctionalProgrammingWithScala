package forcomp
import forcomp.Anagrams.{Occurrences, sentenceOccurrences, subtract, wordOccurrences}

object Main extends App {

    val testSent = List("Linux", "rulez")
    val ta1 = Anagrams.sentenceAnagrams(testSent)
    println(ta1)

}