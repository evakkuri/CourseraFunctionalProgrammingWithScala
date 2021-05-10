import conslist.{ConsList, List, NilList}
import objsets.{GoogleVsApple, TweetReader}
import objsets.GoogleVsApple._

def nth[T](n: Int, l: conslist.List[T]): T = {
  if (l.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) l.head
  else nth(n - 1, l.tail)
}

val list = new ConsList(1, new ConsList(2, new ConsList(3, new ConsList(4, new NilList))))

nth(2, list)
//nth(-1, list)
//nth(5, list)

TweetReader.allTweets foreach println
//GoogleVsApple.googleTweets foreach println