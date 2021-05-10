package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  trait TestImage extends PhotoCanvas {}

  @Test def `test image`: Unit =
    new TestImage {
      println((image.width, image.height))
      val testList = List(image(0, 0), image(0, 1), image(1, 0), image(1, 1))
      println(testList)
      println(testList.sum / testList.length)
      println(boxBlurKernel(image, 0, 0, 1))

      val testList2 = List(
        image(756, 654), image(757, 654), image(758, 654),
        image(756, 655), image(757, 655), image(758, 655),
        image(756, 656), image(757, 656), image(758, 656)
      )

      println(testList2.sorted)
      println(testList2.sum / testList2.length)
      println(boxBlurKernel(image, 757, 655, 1))

      assert(true)
    }
  
  @Test def `test one task`: Unit = {
    val computation = task {
      val result = 1 + 1
      println("Done!")
      result
    }
    println("About to wait for some heavy calculation...")
    computation.join()
    assert(true)
  }

  @Test def `test multiple tasks 1`: Unit = {
    val tasks = for (x <- 0 until 2) yield task {
      val result = x + 1
      println(s"Task $x finished, value $result")
      result
    }

    tasks foreach (_.join())
  }

  @Test def `test multiple tasks 2`: Unit = {
    val srcWidth = 6
    val numTasks = 2
    val bandWidth = srcWidth / numTasks
    val startXs = 0 until srcWidth by bandWidth
    val bandEdges = startXs map ((startX: Int) => (startX, startX + bandWidth))
    println(bandEdges)

    val tasks = bandEdges map ({
      case (fromX: Int, endX: Int) => task {
        println(s"Processing from $fromX to $endX")
        endX - fromX
      }
    })

    tasks foreach (_.join())

    assert(true)
  }

  @Test def `vertical blur, 1 band`: Unit = new TestImage {
    val src = image
    val dstOrig = image
    val dstMod = image 
    val from = 341
    val end = 344
    val radius = 2
    scalashop.VerticalBoxBlur.blur(src, dstMod, from, end, radius)
    assert(dstOrig != dstMod, "Destination image not modified")

    var allSame = true

    for (x <- from until end; y <- 0 until dstMod.height){
      if (src(x, y) != dstMod(x, y)) {
        println(src(x, y), dstMod(x, y))
        allSame = false
      }
    }

    assert(!allSame)
  }

  @Test def `vertical blur, parallel, 2 bands`: Unit = new TestImage {
    
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
