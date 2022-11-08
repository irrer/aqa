package org.aqa.webrun.gapSkew

import org.aqa.db.GapSkew
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.web.C3ChartHistory

import java.awt.Color

class GapSkewHistoryCombinedChart(outputPK: Long, machine: Machine) {

  case class Group(output: Output, gapOffsetSkew: GapOffsetSkew) {}

  private def makeGroup(list: Seq[GapSkew.GapSkewHistory]): Option[Group] = {
    val gapOffsetSkew = GapOffsetSkew.makeGapOffsetSkew(list.map(_.gapSkew))
    if (gapOffsetSkew.isRight)
      Some(Group(list.head.output, gapOffsetSkew.right.get))
    else
      None
  }

  // list of gap offset skew data with the output they belong to
  private val history: Seq[Group] = {
    // @formatter:off
    GapSkew.
      historyByMachine(machine.machinePK.get).
      groupBy(_.output.outputPK.get).
      values.
      flatMap(makeGroup).
      toSeq.
      sortBy(_.output.dataDate.get.getTime)
    // @formatter:on
  }

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val first = history.head.output.dataDate.get
    val last = history.last.output.dataDate.get
    MaintenanceRecord.getRange(machine.machinePK.get, first, last)
  }

  private val xDateList = history.map(h => h.output.dataDate.get)

  private val yIndex = if (history.size < 2) -1 else history.indexWhere(h => h.output.outputPK.get == outputPK)

  private val historyChart = {

    case class YValue(gapOffsetSkew: GapOffsetSkew => ColAngle, colorInt: Int, gosValue: ColAngle => GosValue) {
      val gosVal: GosValue = {
        val ca: ColAngle = gapOffsetSkew(history.head.gapOffsetSkew)
        val gv: GosValue = gosValue(ca)
        gv
      }

      val name: String = gosVal.group + " " + gosVal.name + " (" + gosVal.units + ")"
      val color = new Color(colorInt)

      def getYValue(group: Group): Double = {
        val ca: ColAngle = gapOffsetSkew(group.gapOffsetSkew)
        val gv: GosValue = gosValue(ca)
        gv.v
      }

      def valueList: Seq[Double] = history.map(getYValue)
    }

    val yList = Seq(
      YValue(_.col090, 0x0000ff, _.gap),
      YValue(_.col090, 0x00ff00, _.offset),
      YValue(_.col090, 0xff0000, _.aSkew_mmPer40cm),
      YValue(_.col090, 0x888888, _.bSkew_mmPer40cm),
      //
      YValue(_.col270, 0x00ffff, _.gap),
      YValue(_.col270, 0xff00ff, _.offset),
      YValue(_.col270, 0xffff00, _.aSkew_mmPer40cm),
      YValue(_.col270, 0x000000, _.bSkew_mmPer40cm)
    )

    val chart = new C3ChartHistory(
      chartIdOpt = Some("GapOffsetSkew"),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yList.map(_.name),
      yDataLabel = "Offset (mm)",
      yValues = yList.map(_.valueList),
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = yList.map(_.color),
      Seq()
    )

    chart
  }

  val javascript: String = historyChart.javascript
}
