package org.aqa.webrun.gapSkew

import org.aqa.db.GapSkew
import org.aqa.db.MaintenanceRecord
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory
import org.aqa.webrun.ExtendedData

import java.awt.Color
import scala.collection.Seq

class GapSkewHistoryChart(extendedData: ExtendedData, leafSet: LeafSet) {

  private val history = GapSkew.history(extendedData.machine.machinePK.get, leafSet.gapSkew.beamName)

  private val gs = history.map(_.gapSkew)

  val angleChartIdTag: String = C3Chart.textToChartId(leafSet.gapSkew.beamName) + "_angle"
  val offsetChartIdTag: String = C3Chart.textToChartId(leafSet.gapSkew.beamName) + "_offset"

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val first = history.head.output.dataDate.get
    val last = history.last.output.dataDate.get
    MaintenanceRecord.getRange(extendedData.machine.machinePK.get, first, last)
  }

  private val xDateList = history.map(h => h.output.dataDate.get)

  private val yIndex = if (history.size < 2) -1 else history.indexWhere(h => h.gapSkew.outputPK == extendedData.output.outputPK.get)

  private val angleChart = {
    val yValues = Seq(gs.map(h => h.topAngle_deg), gs.map(h => h.bottomAngle_deg))

    val yColorList = Seq(new Color(0x4477bb), new Color(0x44bb77))

    val chart = new C3ChartHistory(
      chartIdOpt = Some(angleChartIdTag),
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
      Offset("Top Left", 0xff0000, gs => gs.topPlannedY_mm - gs.topLeftY_mm),
      Offset("Top Right", 0x00ff00, gs => gs.topPlannedY_mm - gs.topRightY_mm),
      Offset("Bottom Left", 0x000000, gs => gs.bottomPlannedY_mm - gs.bottomLeftY_mm),
      Offset("Bottom Right", 0xffc800, gs => gs.bottomPlannedY_mm - gs.bottomRightY_mm),
      Offset("Left Vert", 0x0000ff, gs => (gs.topPlannedY_mm - gs.bottomPlannedY_mm) - (gs.topLeftY_mm - gs.bottomLeftY_mm)),
      Offset("Right Vert", 0xffff00, gs => (gs.topPlannedY_mm - gs.bottomPlannedY_mm) - (gs.topRightY_mm - gs.bottomRightY_mm)),
      Offset("Left Horz", 0xff00ff, gs => gs.topLeftY_mm - gs.topRightY_mm),
      Offset("Right Horz", 0x808080, gs => gs.bottomLeftY_mm - gs.bottomRightY_mm)
    )

    val chart = new C3ChartHistory(
      chartIdOpt = Some(offsetChartIdTag),
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

  val js: String = angleChart.javascript + "\n" + offsetChart.javascript

}
