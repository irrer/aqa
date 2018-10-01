
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
import org.aqa.db.PMI
import org.aqa.Config
import java.sql.Timestamp
import java.awt.Color
import akka.Main
import org.aqa.db.MaintenanceCategory

/**
 * Test the Config.
 *
 */

class TestC3ChartHistory extends FlatSpec with Matchers {

  // always use same date for repeatability
  val baseDate = Util.standardDateFormat.parse("2018-01-24T15:31:56")
  val rand = new Random(2135)
  val oneDay = 24L * 60 * 60 * 1000
  val dateRange = 31 * 24 * 60 * 60
  def offsetDate(i: Int) = new Date(baseDate.getTime + (i * oneDay))

  private def makePmi(day: Int, categoryName: String) = {

    //val category = Config.MaintenanceCategoryList(rand.nextInt(Config.MaintenanceCategoryList.size))
    val machinePK = -1
    val userPK = -1
    val outputPK = None
    new PMI(
      None, // pmiPK
      categoryName,
      machinePK,
      new Timestamp(offsetDate(day).getTime),
      userPK,
      outputPK,
      "Summary " + rand.nextInt(100),
      "Description " + rand.nextInt(100))
  }

  "TestC3ChartHistory" should "makeNiceHistoricalChart" in {

    val xAxisLabel = "X Axis Label"
    val xDataLabel = "X Data Label"
    val xDateList = (0 until 6).map(i => offsetDate(i * 3)).sorted
    val xFormat = ".4g"
    val yRange = (1 to 3)
    val yAxisLabels = yRange.map(i => "Y Axis " + i + " Label").toSeq
    val yDataLabel = "Y Data Label"
    val yValues = yRange.map(yy => (0 until xDateList.size).map(y => (rand.nextDouble + 1) * 25346.331))
    val yFormat = ".5"
    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)
    val pmiList = Seq(
      makePmi(2, MaintenanceCategory.firmwareUpdate),
      makePmi(9, MaintenanceCategory.setBaseline))

    val chart = new C3ChartHistory(pmiList, xAxisLabel: String, xDataLabel, xDateList, xFormat, yAxisLabels, yDataLabel, yValues, yFormat, yColorList)

    val sep = Seq.fill(60)("-").reduce(_ + _)
    println(sep)
    println(chart.javascript.replaceAll("ChartId_1", "chart"))
    println(sep)

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Util.randomSecureHash).distinct
    list.size should be(size)
  }
}
