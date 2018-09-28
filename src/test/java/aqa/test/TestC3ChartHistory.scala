
package aqa.test;

import org.aqa.Util
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import org.aqa.web.C3ChartHistory
import scala.util.Random
import java.util.Date

/**
 * Test the Config.
 *
 */

class TestC3ChartHistory extends FlatSpec with Matchers {

  "TestC3ChartHistory" should "makeNiceHistoricalCharch" in {

    val baseDate = Util.standardDateFormat.parse("2018-01-24T15:31:56")
    val rand = new Random(135)
    val dateRange = 31 * 24 * 60 * 60

    val xAxisLabel = "X Axis Label"
    val xDataLabel = "X Data Label"
    val xDateList = (0 until 6).map(i => new Date(baseDate.getTime + rand.nextInt(dateRange) * 1000L)).sorted
    val xFormat = ".g4"
    val yRange = (1 to 3)
    val yAxisLabels = yRange.map(i => "Y Axis " + i + " Label ").toSeq
    val yDataLabel = "Y Data Label"
    val yValues = yRange.map(yy => (0 until xDateList.size).map(y => (rand.nextDouble + 1) * 25346.331))
    val yFormat = ".g4"

    val chart = new C3ChartHistory(-1, xAxisLabel: String, xDataLabel, xDateList, xFormat, yAxisLabels, yDataLabel, yValues, yFormat)

    println(chart.javascript)

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Util.randomSecureHash).distinct
    list.size should be(size)
  }
}
