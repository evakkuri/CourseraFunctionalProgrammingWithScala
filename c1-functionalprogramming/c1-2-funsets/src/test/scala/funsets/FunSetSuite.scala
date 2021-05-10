package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`(): Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`(): Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersection contains common elements of each set`(): Unit = {
    new TestSets {
      private val s_union = union(s1, s2)
      private val s_inters = intersect(s_union, s1)
      assert(contains(s_inters, 1), "Intersection 1")
      assert(!contains(s_inters, 2), "Intersection 2")
    }
  }

  @Test def `difference contains elements from first set not in second set`(): Unit = {
    new TestSets {
      private val s_union = union(s1, s2)
      assert(contains(diff(s_union, s1), 2))
      assert(!contains(diff(s_union, s1), 1))
    }
  }

  @Test def `filter returns elements from set matched by filter func`(): Unit = {
    new TestSets {
      private val s_union_1 = union(s1, s2)
      private val s_union = union(s_union_1, s3)
      assert(contains(filter(s_union, (x: Int) => x % 2 == 0), 2))
    }
  }

  @Test def `forall returns true if all set elements match function`(): Unit = {
    def s(x: Int) = x % 15 == 0
    def p(x: Int) = {
      //println(x, x % 3 == 0)
      x % 3 == 0
    }
    assert(forall(s, p))
  }

  @Test def `exists returns true if an element in set matches function`(): Unit = {
    def evenInts(x: Int) = x % 2 == 0
    assert(exists(evenInts, (x: Int) => x % 10 == 0))
  }

  @Test def `exists returns false if no element in set matches function`(): Unit = {
    def evenInts(x: Int) = x % 2 == 0
    assert(!exists(evenInts, (x: Int) => x % 2 != 0))
  }

  @Test def `map first transforms inputs with function then checks set membership`(): Unit = {
    def f(x: Int) = x * 2
    def s(x: Int) = true
    def p(x: Int) = x % 2 == 0

    assert(forall(map(s, f), p))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
