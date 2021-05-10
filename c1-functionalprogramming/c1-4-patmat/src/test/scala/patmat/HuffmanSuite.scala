package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  trait TestLists {
    val l1 = List('a', 'b', 'a', 'c', 'c', 'b', 'a', 'b', 'a')
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `times test`: Unit = {
    new TestLists {
      val l1Map = times(l1).toMap
      assertEquals(l1Map('a'), 4)
      assertEquals(l1Map('b'), 3)
      assertEquals(l1Map('c'), 2)
    }
  }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `test decode scenarios`: Unit = {
     new TestTrees {
       assertEquals(List('a'), decode(t1, List(0)))
       assertEquals(List('b'), decode(t2, List(0, 1)))
       assertEquals(List('a', 'b', 'd'), decode(t2, List(0, 0, 0, 1, 1)))
     }
  }

  @Test def `test decoding secret`: Unit = {
    assertEquals(decodedSecret, List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  @Test def `test encoding scenarios`: Unit = {
    new TestTrees {
      assertEquals(List(0), encode(t1)(List('a')))
      assertEquals(List(0, 1), encode(t1)(List('a', 'b')))
      assertEquals(List(0, 0, 0, 1, 1), encode(t2)(List('a', 'b', 'd')))
    }
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `test code table generation`: Unit = {
    new TestTrees {
      assertEquals(List(('a', List(0)), ('b', List(1))), convert(t1))
      assertEquals(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))), convert(t2))
    }
  }

  @Test def `test quickEncode`: Unit = {
    new TestTrees {
      assertEquals(List(0), quickEncode(t1)(List('a')))
      assertEquals(List(0, 1), quickEncode(t1)(List('a', 'b')))
      assertEquals(List(0, 0, 0, 1, 1), quickEncode(t2)(List('a', 'b', 'd')))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
