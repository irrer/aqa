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
import edu.umro.ScalaUtil.Trace

/**
 * Analyze DICOM files for symmetry and flatness.
 */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, extendedData: ExtendedData) extends Logging {

  private val history = SymmetryAndFlatness.recentHistory(50, extendedData.machine.machinePK.get, extendedData.output.procedurePK, beamName, extendedData.output.dataDate)
  private val dateList = history.map(h => h.date)
  private val dateListFormatted = dateList.map(d => Util.standardDateFormat.format(d))

  private def idOf(typ: String): String = "Chart_" + WebUtil.stringToUrlSafe(beamName) + "_" + typ

  private val tagAxial = "AxialSymmetry"
  private val tagTransverse = "TransverseSymmetry"
  private val tagFlatness = "FlatnessSymmetry"

  private val idAxial = idOf(tagAxial)
  private val idTransverse = idOf(tagTransverse)
  private val idFlatness = idOf(tagFlatness)

  // TODO pmiList is null?
  private val pmiList = PMI.getRange(extendedData.machine.machinePK.get, history.head.date, history.last.date)
  Trace.trace("pmiList: " + pmiList)

  private def makeChart(id: String, valueList: Seq[Double]): C3ChartHistory = {

    val currentDateIndex = dateList.indexWhere(d => extendedData.output.dataDate.get.getTime == d.getTime)
    val minDateTag = dateListFormatted.head
    val maxDateTag = dateListFormatted.last

    val width = None
    Trace.trace(width)

    val height = Some(200)
    Trace.trace(height)

    val xAxisLabel = "Date"
    Trace.trace(xAxisLabel)

    val xDataLabel = "Date"
    Trace.trace(xDataLabel)

    val xDateList = dateList
    Trace.trace(xDateList)

    val xFormat = ".4g"
    Trace.trace(xFormat)

    val baselineSpec = None
    Trace.trace(baselineSpec)

    val yAxisLabels = Seq(id)
    Trace.trace(yAxisLabels)

    val yDataLabel = id
    Trace.trace(yDataLabel)

    val yValues = Seq(valueList)
    Trace.trace(yValues)

    val yFormat = ".5"
    Trace.trace(yFormat)

    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)
    Trace.trace(yColorList)

    val chart = new C3ChartHistory(
      pmiList,
      width,
      height,
      xAxisLabel, xDataLabel, xDateList, xFormat,
      baselineSpec,
      yAxisLabels, yDataLabel, yValues, yFormat, yColorList)

    Trace.trace(chart)
    Trace.trace(chart.html)
    Trace.trace(chart.javascript)
    chart
  }

  private val chartAxial = makeChart(idAxial, history.map(h => h.symmetryAndFlatness.axialSymmetry_mm))
  private val chartTransverse = makeChart(idTransverse, history.map(h => h.symmetryAndFlatness.transverseSymmetry_mm))
  private val chartFlatness = makeChart(idFlatness, history.map(h => h.symmetryAndFlatness.flatness_mm))

  val javascript = chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript
  Trace.trace(javascript)

  val htmlAxial = chartAxial.html
  val htmlTransverse = chartTransverse.html
  val htmlFlatness = chartFlatness.html

}
