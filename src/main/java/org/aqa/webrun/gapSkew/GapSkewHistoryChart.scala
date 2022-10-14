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

  private val angleChart = {
    val yValues = Seq(gs.map(h => h.topHorzSkew_deg), gs.map(h => h.bottomHorzSkew_deg))

    val yColorList = Seq(new Color(0x4477bb), new Color(0x44bb77))

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
      yAxisLabels = Seq("Top", "Bottom"),
      yDataLabel = "Angle (deg)",
      yValues,
      yIndex = yIndex,
      yFormat = ".2g",
      yColorList,
      Seq()
    )

    chart
  }

  private val offsetChart = {

    case class Offset(name: String, colorInt: Int, value: GapSkew => Double) {
      val color = new Color(colorInt)
      def valueList: Seq[Double] = gs.map(value)
    }

    val offsetList = Seq(
      Offset("Top Left", 0xff0000, gs => gs.topLeftHorzDelta_mm),
      Offset("Top Right", 0x00ff00, gs => gs.topRightHorzDelta_mm),
      Offset("Bottom Left", 0x000000, gs => gs.bottomLeftHorzDelta_mm),
      Offset("Bottom Right", 0xffc800, gs => gs.bottomRightHorzDelta_mm),
      Offset("Left Vert", 0x0000ff, gs => gs.leftDeltaSeparationOfHorzEdges_mm),
      Offset("Right Vert", 0xffff00, gs => gs.rightDeltaSeparationOfHorzEdges_mm),
      Offset("Top Delta", 0xff00ff, gs => gs.topHorzDelta_mm),
      Offset("Bottom Delta", 0x808080, gs => gs.bottomHorzDelta_mm)
    )

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
