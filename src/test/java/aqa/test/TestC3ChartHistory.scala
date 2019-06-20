
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
import org.aqa.db.MaintenanceRecord
import org.aqa.Config
import java.sql.Timestamp
import java.awt.Color
import akka.Main
import org.aqa.db.MaintenanceCategory
import org.aqa.web.C3Chart
import org.aqa.db.Baseline
import org.aqa.db.BaselineSetup
import org.aqa.Crypto

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
    new MaintenanceRecord(
      None, // maintenanceRecordPK
      categoryName,
      machinePK,
      new Timestamp(offsetDate(day).getTime),
      userPK,
      outputPK,
      "Summary " + rand.nextInt(100),
      "Description " + rand.nextInt(100))
  }

  "TestC3ChartHistory" should "makeNiceHistoricalChart" in {

    val xLabel = "X Axis Label"
    val xDateList = (0 until 6).map(i => offsetDate(i * 3)).sorted
    val xFormat = ".4g"
    val yRange = (1 to 3)
    val yAxisLabels = yRange.map(i => "Y Axis " + i + " Label").toSeq
    val yDataLabel = "Y Data Label"
    val yValues = yRange.map(yy => (0 until xDateList.size).map(y => (rand.nextDouble + 1) * 25346.331))
    val yFormat = ".5"
    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)
    val XmaintenanceRecordList = Seq(
      makePmi(2, MaintenanceCategory.firmwareUpdate),
      makePmi(9, MaintenanceCategory.setBaseline))

    val yMin = yValues.flatten.min
    val yMax = yValues.flatten.max
    val yDiff = yMax - yMin
    val maintenanceRecordList = Seq[MaintenanceRecord]()
    val baselineSpec = new Baseline(None, -1, XmaintenanceRecordList(1).creationTime, Some("1.2.3.4.5.6.7.8.9"), "baseline ID", yValues.head(3).toString, BaselineSetup.byDefault.toString)

    //yValues.head(3), Color.green)

    val tolerance = new C3Chart.Tolerance((yDiff * .2) + yMin, (yDiff * .8) + yMin)

    val chart = new C3ChartHistory(Some("TestChart"), maintenanceRecordList, Some(600), Some(200), xLabel, xDateList, Some(baselineSpec), Some(tolerance), yAxisLabels, yDataLabel, yValues, yValues.head.size / 2, yFormat, yColorList)
    //val chart = new C3ChartHistory(maintenanceRecordList, xAxisLabel: String, xDataLabel, xDateList, xFormat, None, yAxisLabels, yDataLabel, yValues, yFormat, yColorList)

    val sep = Seq.fill(60)("-").reduce(_ + _)
    println(sep)
    println(chart.javascript.replaceAll("ChartId_1", "chart"))
    println(sep)

    (11 > 10) should be(true)
  }

  "randomSecureHash" should "be different each time" in {
    val size = 100
    val list = (0 until size).map(i => Crypto.randomSecureHash).distinct
    list.size should be(size)
  }
}
