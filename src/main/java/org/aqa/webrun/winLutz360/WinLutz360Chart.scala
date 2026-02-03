package org.aqa.webrun.winLutz360

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.WinLutz360
import org.aqa.web.C3ChartHistory
import org.aqa.Config
import org.aqa.Util

import java.awt.Color
import java.util.Date

/**
 * Make history charts for the various WL beams.  These are displayed at the bottom of the main report page.
 *
 * @param outputPK For this output.
 */
class WinLutz360Chart(outputPK: Long) extends Logging with WLChartGeneric {
  private val output: Output = Output.get(outputPK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get

  // make a set of beams we are interested in
  val beamNameList: Seq[String] = WinLutz360.getByOutput(outputPK).map(_.beamNameOf).sorted

  /**
   * Make a chart for the given history.
   *
   * @param history History for one beam sorted by date.
   * @return history chart
   */
  private def makeChart(history: Seq[WinLutz360.WinLutz360History]): C3ChartHistory = {

    val maintenanceList = {
      val first = history.head.output.dataDate.get
      val last = history.last.output.dataDate.get
      MaintenanceRecord.getRange(machine.machinePK.get, first, last).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
    }

    val chartIdOpt = Some("C_" + Util.textToId(history.head.winLutz360.beamNameOf))

    abstract class YD(val name: String, val color: Color) {
      def get(h: WinLutz360.WinLutz360History): Double
    }
    class YData(name: String, color: Color, data: WinLutz360.WinLutz360History => Double) extends YD(name, color) {
      def get(h: WinLutz360.WinLutz360History): Double = data(h)
    }
    class YDataOpt(name: String, color: Color, data: WinLutz360.WinLutz360History => Option[Double]) extends YD(name, color) {
      def get(h: WinLutz360.WinLutz360History): Double = if (data(h).isDefined) data(h).get else 0.0
    }

    val offsetList: Seq[YD] = {

      if (
        history.exists(h => //
          h.winLutz360.X1PlannedOffset_mm.isEmpty ||
            h.winLutz360.X2PlannedOffset_mm.isEmpty ||
            h.winLutz360.Y1PlannedOffset_mm.isEmpty ||
            h.winLutz360.Y2PlannedOffset_mm.isEmpty
        )
      )
        Seq()
      else {
        Seq( //
          new YDataOpt("X1 edge - planned", new Color(0xffc800), h => Some(h.winLutz360.X1Offset_mm.get - h.winLutz360.X1PlannedOffset_mm.get.abs)),
          new YDataOpt("X2 edge - planned", new Color(0x44ff44), h => Some(h.winLutz360.X2Offset_mm.get - h.winLutz360.X1PlannedOffset_mm.get.abs)),
          new YDataOpt("Y1 edge - planned", new Color(0x000080), h => Some(h.winLutz360.Y1Offset_mm.get - h.winLutz360.X1PlannedOffset_mm.get.abs)),
          new YDataOpt("Y2 edge - planned", new Color(0x804000), h => Some(h.winLutz360.Y2Offset_mm.get - h.winLutz360.X1PlannedOffset_mm.get.abs))
        )
      }
    }

    val yData: Seq[YD] = Seq(
      // @formatter:off
      new YData   ("R (Total Offset)"      , new Color(0xff0000), h => h.winLutz360.errorXY_mm     ),
      new YData   ("X offset"              , new Color(0x444444), h => h.winLutz360.errorX_mm      ),
      new YData   ("Y offset"              , new Color(0x888888), h => h.winLutz360.errorY_mm      ),
      
      new YData   ("X box center"          , new Color(0x00ffff), h => h.winLutz360.boxCenterX_mm  ),
      new YData   ("Y box center"          , new Color(0x006666), h => h.winLutz360.boxCenterY_mm  ),
      
      new YData   ("X ball center"         , new Color(0xff00ff), h => h.winLutz360.ballCenterX_mm ),
      new YData   ("Y ball center"         , new Color(0x990099), h => h.winLutz360.ballCenterY_mm ),
      // @formatter:on
    ) ++ offsetList

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
    val list = WinLutz360.historyByMachine(machine.machinePK.get).filter(h => beamNameList.contains(h.winLutz360.beamNameOf))
    list.groupBy(_.winLutz360.beamNameOf).toSeq.sortBy(_._1).map(_._2.sortBy(_.output.dataDate.get.getTime))
  }

  val chartList: Seq[C3ChartHistory] = historyForAllBeams.map(_.takeRight(Config.WLMaxChartHistory)).map(makeChart)

}
