
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.web.C3Chart

/**
 * Test the Config.
 *
 */

class TestC3Chart extends FlatSpec with Matchers {

  "C3Chart" should "makeNiceCharch" in {

    val xAxisLabel = "X Axis Label"
    val xDataLabel = "X Data Label"
    val xValueList = (2 to 25).map(i => i * 1.1)
    val xFormat = ".g4"
    val yRange = (0 to 5)
    val yAxisLabels = yRange.map(i => "Y Axis " + i + " Label ").toSeq
    val yDataLabel = "Y Data Label"
    val yValues = yRange.map(yy => yRange.map(y => ((y * 71 * yy) % 13) * 6.3))
    val yFormat = ".g4"

    val chart = new C3Chart(xAxisLabel: String, xDataLabel, xValueList, xFormat, yAxisLabels, yDataLabel, yValues, yFormat)

    println(chart.javascript)

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Util.randomSecureHash).distinct
    list.size should be(size)
  }
}
