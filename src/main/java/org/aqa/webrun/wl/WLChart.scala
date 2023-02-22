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

    abstract class YD(val name: String, val color: Color) {
      def get(h: WinstonLutz.WinstonLutzHistory): Double
    }
    class YData(name: String, color: Color, data: WinstonLutz.WinstonLutzHistory => Double) extends YD(name, color) {
      def get(h: WinstonLutz.WinstonLutzHistory): Double = data(h)
    }
    class YDataOpt(name: String, color: Color, data: WinstonLutz.WinstonLutzHistory => Option[Double]) extends YD(name, color) {
      def get(h: WinstonLutz.WinstonLutzHistory): Double = if (data(h).isDefined) data(h).get else 0.0
    }

    val yData: Seq[YD] = Seq(
      // @formatter:off
      new YData   ("R (Total Offset)"      , new Color(0xff0000), h => h.winstonLutz.errorXY_mm     ),
      new YData   ("X offset"              , new Color(0x444444), h => h.winstonLutz.errorX_mm      ),
      new YData   ("Y offset"              , new Color(0x888888), h => h.winstonLutz.errorY_mm      ),
      
      new YData   ("X box center"          , new Color(0x00ffff), h => h.winstonLutz.boxCenterX_mm  ),
      new YData   ("Y box center"          , new Color(0x006666), h => h.winstonLutz.boxCenterY_mm  ),
      
      new YData   ("X ball center"         , new Color(0xff00ff), h => h.winstonLutz.ballX_mm       ),
      new YData   ("Y ball center"         , new Color(0x990099), h => h.winstonLutz.ballY_mm       ),
      //
      new YDataOpt("Top edge - planned"    , new Color(0xffc800), h => h.winstonLutz.topError_mm    ),
      new YDataOpt("Bottom edge - planned" , new Color(0x44ff44), h => h.winstonLutz.bottomError_mm ),
      new YDataOpt("Left edge - planned"   , new Color(0x000080), h => h.winstonLutz.leftError_mm   ),
      new YDataOpt("Right edge - planned"  , new Color(0x804000), h => h.winstonLutz.rightError_mm  )
      // @formatter:on
    )

    val dateList = {
      val list = history.map(h => h.output.dataDate.get.asInstanceOf[Date])
      yData.map(_ => list)
    }

    val yIndex = history.indexWhere(_.output.outputPK.get == outputPK)

    val yColorList = yData.map(_.color)

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
      yValues = yData.map(yd => history.map(yd.get)),
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
