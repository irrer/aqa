package org.aqa.webrun.wl

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.WinstonLutz
import org.aqa.web.C3ChartHistory
import org.aqa.Util

import java.awt.Color
import java.util.Date

class WLChart(outputPK: Long) extends Logging {
  private val output: Output = Output.get(outputPK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get

  // make a set of beams we are interested in
  val beamNameList: Seq[String] = WinstonLutz.getByOutput(outputPK).map(_.beamNameOf).sorted

  /**
    * Make a chart for the given history.
    * @param history History for one beam sorted by date.
    * @return history chart
    */
  private def makeChart(history: Seq[WinstonLutz.WinstonLutzHistory]): C3ChartHistory = {

    val maintenanceList = {
      val first = history.head.output.dataDate.get
      val last = history.last.output.dataDate.get
      MaintenanceRecord.getRange(machine.machinePK.get, first, last).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
    }

    val chartIdOpt = Some("C_" + Util.textToId(history.head.winstonLutz.beamNameOf))

    case class YData(name: String, data: WinstonLutz.WinstonLutzHistory => Double) {}

    val yData = Seq(
      // @formatter:off
      YData("R (Total Offset)" , h => h.winstonLutz.errorXY   ),
      YData("X offset"         , h => h.winstonLutz.errorX    ),
      YData("Y offset"         , h => h.winstonLutz.errorY    ),
      YData("X box center"     , h => h.winstonLutz.boxCenterX),
      YData("Y box center"     , h => h.winstonLutz.boxCenterY),
      YData("X ball center"    , h => h.winstonLutz.ballX_mm  ),
      YData("Y ball center"    , h => h.winstonLutz.ballY_mm  )
      // @formatter:on
    )

    val dateList = {
      val list = history.map(h => h.output.dataDate.get.asInstanceOf[Date])
      yData.map(_ => list)
    }

    val yIndex = history.indexWhere(_.output.outputPK.get == outputPK)
    val yColorList = Seq(
      Color.red,
      Color.green,
      Color.black,
      Color.lightGray,
      Color.orange,
      Color.blue,
      Color.magenta
    )
    new C3ChartHistory(
      chartIdOpt = chartIdOpt,
      maintenanceList = maintenanceList,
      width = None,
      height = None,
      xLabel = "Date",
      xDateList = dateList,
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yData.map(_.name),
      yDataLabel = "mm",
      yValues = yData.map(yd => history.map(yd.data)),
      yIndex = yIndex,
      yFormat = ".2r",
      yColorList = yColorList,
      setBaselineList = Seq()
    )
  }

  // Get all the history for this machine, but only for beams referenced by this
  // output.  Group by beam name, sort the groups by beam name, and sort each
  // data set for each beam by dataDate.
  private val historyForAllBeams = {
    val list = WinstonLutz.historyByMachine(machine.machinePK.get).filter(h => beamNameList.contains(h.winstonLutz.beamNameOf))
    list.groupBy(_.winstonLutz.beamNameOf).toSeq.sortBy(_._1).map(_._2.sortBy(_.output.dataDate.get.getTime))
  }

  val chartList: Seq[C3ChartHistory] = historyForAllBeams.map(makeChart)

}
