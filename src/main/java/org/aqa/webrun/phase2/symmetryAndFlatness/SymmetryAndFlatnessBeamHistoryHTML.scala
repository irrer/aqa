package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Logging
import org.aqa.db.SymmetryAndFlatness
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import java.awt.Color
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Baseline
import org.aqa.Config
import org.aqa.web.C3Chart
import org.aqa.webrun.ExtendedData
import scala.collection.Seq
import org.aqa.db.Output
import org.aqa.db.MaintenanceCategory
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ScalaUtil.Trace

/**
 * Analyze DICOM files for symmetry and flatness.
 */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, outputPK: Long) extends Logging {

  val output = Output.get(outputPK).get
  val machinePK = output.machinePK.get

  private val history = SymmetryAndFlatness.recentHistory(Config.SymFlatConstHistoryRange, machinePK, output.procedurePK, beamName, output.dataDate)
  private val dateList = history.map(h => h.date)
  private val dateListFormatted = dateList.map(d => Util.standardDateFormat.format(d))

  // index of the entry being charted.
  private val yIndex = history.indexWhere(h => h.symmetryAndFlatness.outputPK == output.outputPK.get)

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val inTimeRange = MaintenanceRecord.getRange(machinePK, history.head.date, history.last.date)
    val releventBaseline = Baseline.filterOutUnrelatedBaselines(inTimeRange.map(itr => itr.maintenanceRecordPK.get).toSet, Set("symmetry", "flatness", "constancy")).map(_.maintenanceRecordPK.get).toSet
    inTimeRange.filter(itr => releventBaseline.contains(itr.maintenanceRecordPK.get) || (!itr.category.equals(MaintenanceCategory.setBaseline)))
  }

  private def getBaseline(dataName: String): Option[Baseline] = {
    val baselineName = SymmetryAndFlatnessAnalysis.makeBaselineName(beamName, dataName)
    Baseline.findLatest(machinePK, baselineName, output.dataDate.get) match {
      case Some(maintAndBaseline) => Some(maintAndBaseline._2)
      case _ => None
    }
  }

  private def makeChart(id: String, limit: Double, valueList: Seq[Double]): C3ChartHistory = {

    val baseline = getBaseline(id)
    if (baseline.isEmpty) // TODO rm
      logger.info("No baseline found for beam " + beamName) // TODO rm
    val currentDateIndex = dateList.indexWhere(d => output.dataDate.get.getTime == d.getTime)
    val minDateTag = dateListFormatted.head
    val maxDateTag = dateListFormatted.last
    val chartId = C3Chart.idTagPrefix + Util.textToId(id)

    val width = None
    val height = None
    val xLabel = "Date"
    val xDateList = dateList
    val xFormat = ".4g"
    val yDataLabel = id + " %"
    val yAxisLabels = Seq(id + " %")
    val yValues = Seq(valueList)
    val yFormat = ".4g"
    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)

    val tolerance = if (baseline.isDefined) Some(new C3Chart.Tolerance(baseline.get.value.toDouble - limit, baseline.get.value.toDouble + limit)) else None
    val chart = new C3ChartHistory(
      Some(chartId),
      MaintenanceRecordList,
      width,
      height,
      xLabel, xDateList,
      baseline,
      tolerance,
      None, // yRange
      yAxisLabels, yDataLabel, yValues, yIndex, yFormat, yColorList)

    chart
  }

  val javascript = {
    import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis._

    history.head.symmetryAndFlatness.axialSymmetry_pct     // TODO rm
    val chartAxial = makeChart(axialSymmetryName, Config.SymmetryPercentLimit, history.map(h => h.symmetryAndFlatness.axialSymmetry_pct))
    val chartTransverse = makeChart(transverseSymmetryName, Config.SymmetryPercentLimit, history.map(h => h.symmetryAndFlatness.transverseSymmetry_pct))
    val chartFlatness = makeChart(flatnessName, Config.FlatnessPercentLimit, history.map(h => h.symmetryAndFlatness.flatness_pct))
    val chartProfileConstancy = makeChart(profileConstancyName, Config.ProfileConstancyPercentLimit, history.map(h => h.symmetryAndFlatness.profileConstancy_pct))

    chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript + chartProfileConstancy.javascript
  }

}
