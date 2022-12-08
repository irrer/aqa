package org.aqa.webrun.gapSkew

import org.aqa.db.GapSkew
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color

object GapSkewHistoryChart {
  def angleChartIdTag(beamName: String): String = C3Chart.textToChartId(beamName) + "_angle"
  def offsetChartIdTag(beamName: String): String = C3Chart.textToChartId(beamName) + "_offset"
}

class GapSkewHistoryChart(outputPK: Long, beamName: String) {

  private val machinePK = Output.get(outputPK).get.machinePK.get

  private val history = GapSkew.historyByBeam(machinePK, beamName)

  private val gs = history.map(_.gapSkew)

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val first = history.head.output.dataDate.get
    val last = history.last.output.dataDate.get
    MaintenanceRecord.getRange(machinePK, first, last)
  }

  private val xDateList = history.map(h => h.output.dataDate.get)

  private val yIndex = if (history.size < 2) -1 else history.indexWhere(h => h.gapSkew.outputPK == outputPK)

  private case class Offset(name: String, colorInt: Int, value: GapSkew => Option[Double]) {
    val color = new Color(colorInt)
    def valueList: Seq[Double] = gs.flatMap(gs => value(gs))
  }

  private val angleChart = {

    val offsetList = Seq(
      Offset("Top", 0x4477bb, gs => gs.topHorzSkew_mmPer40cm),
      Offset("Bottom", 0x44bb77, gs => gs.bottomHorzSkew_mmPer40cm)
    ).filter(_.valueList.size == gs.size)

    val chart = new C3ChartHistory(
      chartIdOpt = Some(GapSkewHistoryChart.angleChartIdTag(beamName)),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = offsetList.map(_.name),
      yDataLabel = "Angle (mm/40cm)",
      yValues = offsetList.map(_.valueList),
      yIndex = yIndex,
      yFormat = ".2g",
      yColorList = offsetList.map(_.color),
      Seq()
    )

    chart
  }

  private val offsetChart = {
    val offsetList = Seq(
      Offset("Top Left", 0xff0000, gs => gs.topLeftHorzDelta_mm),
      Offset("Top Right", 0x00ff00, gs => gs.topRightHorzDelta_mm),
      Offset("Bottom Left", 0x000000, gs => gs.bottomLeftHorzDelta_mm),
      Offset("Bottom Right", 0xffc800, gs => gs.bottomRightHorzDelta_mm),
      Offset("Left Vert", 0x0000ff, gs => gs.leftDeltaSeparationOfHorzEdges_mm),
      Offset("Right Vert", 0xffff00, gs => gs.rightDeltaSeparationOfHorzEdges_mm),
      Offset("Top Delta", 0xff00ff, gs => gs.topHorzDelta_mm),
      Offset("Bottom Delta", 0x808080, gs => gs.bottomHorzDelta_mm)
    ).filter(_.valueList.size == gs.size)
    /* Note: the filter above takes into account that some results may
    be missing.  In that case, skip all results of that type (e.g.
    Left Vert).  This approach is needed because otherwise the y-value
    arrays would be of different size, and values could be associated
    with the wrong date.  Ideally there would be a different date
    list for each value type, but we're not there yet. */

    val chart = new C3ChartHistory(
      chartIdOpt = Some(GapSkewHistoryChart.offsetChartIdTag(beamName)),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = offsetList.map(_.name),
      yDataLabel = "Offset (mm)",
      yValues = offsetList.map(_.valueList),
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = offsetList.map(_.color),
      Seq()
    )

    chart
  }

  val javascript: String = angleChart.javascript + "\n" + offsetChart.javascript

}
