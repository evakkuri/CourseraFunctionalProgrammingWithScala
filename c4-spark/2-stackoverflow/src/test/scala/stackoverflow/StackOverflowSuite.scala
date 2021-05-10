package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("WARN")
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  /*
  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  @Test def `can read in raw data`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val rawRdd = testObject.rawPostings(lines)
    println("Example Posting object:", rawRdd.take(1)(0))
  }

  @Test def `can group questions and answers`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val rawRdd = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(rawRdd)
    println("Example grouped Posting object:", grouped.take(1)(0))
  }

  @Test def `scored set has a specific number of entries`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val rawRdd = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(rawRdd)
    val scored = testObject.scoredPostings(grouped)
    println("Example scored Posting object:", grouped.take(1)(0))
    val count = scored.count()
    println(s"Scored dataset has $count entries, expected is 2121822.")
    assert(count == 2121822)
  }
  */

  @Test def `cluster mean update works as expected`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val rawRdd = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(rawRdd)
    val scored = testObject.scoredPostings(grouped)
    val vectors = testObject.vectorPostings(scored)

    val means = testObject.sampleVectors(vectors)
    println(s"Starting means size: $means.size)")
    means.foreach(println(_))

    var meansRdd = sc.parallelize(means)

    println()

    val newMeans = vectors
      .map( v => (means(testObject.findClosest(v, means)), v) ) // Assing points to nearest cluster kernel
      .take(10).foreach(println(_))
      //.take(1)
      //.groupByKey
      //.map(_ match {
      //  case (cluster, points_iterable) => testObject.averageVectors(points_iterable) // Update cluster kernels to average of all points assigned to kernel
      //}).collect().toArray

    //println(newMeans)
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
