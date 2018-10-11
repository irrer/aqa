package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.SymmetryAndFlatness
import scala.xml.Elem
import org.aqa.web.WebUtil
import java.sql.Timestamp
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import java.awt.Color
import org.aqa.db.PMI
import org.aqa.db.Baseline

/**
 * Analyze DICOM files for symmetry and flatness.
 */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, extendedData: ExtendedData) extends Logging {

  private val history = SymmetryAndFlatness.recentHistory(50, extendedData.machine.machinePK.get, extendedData.output.procedurePK, beamName, extendedData.output.dataDate)
  private val dateList = history.map(h => h.date)
  private val dateListFormatted = dateList.map(d => Util.standardDateFormat.format(d))

  private val pmiList = PMI.getRange(extendedData.machine.machinePK.get, history.head.date, history.last.date)

  private def getBaseline(dataName: String): Option[C3ChartHistory.BaselineSpec] = {
    val baselineName = SymmetryAndFlatnessAnalysis.makeBaselineName(beamName, dataName)
    Baseline.findLatest(extendedData.machine.machinePK.get, baselineName) match {
      case Some(baseline) => Some(new C3ChartHistory.BaselineSpec(baseline._2.value.toDouble, Color.green))
      case _ => None
    }
  }

  private def makeChart(id: String, baselineSpec: Option[C3ChartHistory.BaselineSpec], valueList: Seq[Double]): C3ChartHistory = {

    val currentDateIndex = dateList.indexWhere(d => extendedData.output.dataDate.get.getTime == d.getTime)
    val minDateTag = dateListFormatted.head
    val maxDateTag = dateListFormatted.last

    val width = None
    val height = None
    val xLabel = "Date"
    val xDateList = dateList
    val xFormat = ".4g"
    val yAxisLabels = Seq(id)
    val yDataLabel = id
    val yValues = Seq(valueList)
    val yFormat = ".5"
    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)

    val chart = new C3ChartHistory(
      pmiList,
      width,
      height,
      xLabel, xDateList, xFormat,
      baselineSpec,
      yAxisLabels, yDataLabel, yValues, yFormat, yColorList)

    chart
  }

  private val chartAxial = makeChart("Axial Symmetry", getBaseline(SymmetryAndFlatnessAnalysis.axialSymmetryName), history.map(h => h.symmetryAndFlatness.axialSymmetry_mm))
  private val chartTransverse = makeChart("Transverse Symmetry", getBaseline(SymmetryAndFlatnessAnalysis.transverseSymmetryName), history.map(h => h.symmetryAndFlatness.transverseSymmetry_mm))
  private val chartFlatness = makeChart("Flatness Symmetry", getBaseline(SymmetryAndFlatnessAnalysis.flatnessName), history.map(h => h.symmetryAndFlatness.flatness_mm))

  val javascript = chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript

  val htmlAxial = chartAxial.html
  val htmlTransverse = chartTransverse.html
  val htmlFlatness = chartFlatness.html

}
