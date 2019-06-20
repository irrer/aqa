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
import org.aqa.webrun.phase2.ExtendedData
import scala.collection.Seq

/**
 * Analyze DICOM files for symmetry and flatness.
 */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, extendedData: ExtendedData) extends Logging {

  private val history = SymmetryAndFlatness.recentHistory(50, extendedData.machine.machinePK.get, extendedData.output.procedurePK, beamName, extendedData.output.dataDate)
  private val dateList = history.map(h => h.date)
  private val dateListFormatted = dateList.map(d => Util.standardDateFormat.format(d))

  // index of the entry being charted.
  private val yIndex = history.indexWhere(h => h.symmetryAndFlatness.outputPK == extendedData.output.outputPK.get)

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = MaintenanceRecord.getRange(extendedData.machine.machinePK.get, history.head.date, history.last.date)

  private def getBaseline(dataName: String): Baseline = {
    val baselineName = SymmetryAndFlatnessAnalysis.makeBaselineName(beamName, dataName)
    Baseline.findLatest(extendedData.machine.machinePK.get, baselineName).get._2
  }

  private def makeChart(id: String, baseline: Baseline, limit: Double, valueList: Seq[Double]): C3ChartHistory = {

    val currentDateIndex = dateList.indexWhere(d => extendedData.output.dataDate.get.getTime == d.getTime)
    val minDateTag = dateListFormatted.head
    val maxDateTag = dateListFormatted.last

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

    val tolerance = new C3Chart.Tolerance(baseline.value.toDouble - limit, baseline.value.toDouble + limit)
    val chart = new C3ChartHistory(
      None,
      MaintenanceRecordList,
      width,
      height,
      xLabel, xDateList,
      Some(baseline),
      Some(tolerance),
      yAxisLabels, yDataLabel, yValues, yIndex, yFormat, yColorList)

    chart
  }

  private val chartAxial = makeChart("Axial Symmetry", getBaseline(SymmetryAndFlatnessAnalysis.axialSymmetryName), Config.SymmetryPercentLimit, history.map(h => h.symmetryAndFlatness.axialSymmetry_pct))
  private val chartTransverse = makeChart("Transverse Symmetry", getBaseline(SymmetryAndFlatnessAnalysis.transverseSymmetryName), Config.SymmetryPercentLimit, history.map(h => h.symmetryAndFlatness.transverseSymmetry_pct))
  private val chartFlatness = makeChart("Flatness", getBaseline(SymmetryAndFlatnessAnalysis.flatnessName), Config.FlatnessPercentLimit, history.map(h => h.symmetryAndFlatness.flatness_pct))
  private val chartProfileConstancy = makeChart("Profile Constancy", getBaseline(SymmetryAndFlatnessAnalysis.profileConstancyName), Config.ProfileConstancyPercentLimit, history.map(h => h.symmetryAndFlatness.profileConstancy_pct))

  val javascript = chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript + chartProfileConstancy.javascript

  val htmlAxial = chartAxial.html
  val htmlTransverse = chartTransverse.html
  val htmlFlatness = chartFlatness.html
  val htmlProfileConstancy = chartProfileConstancy.html

}
