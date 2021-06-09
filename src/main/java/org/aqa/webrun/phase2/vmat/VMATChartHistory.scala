package org.aqa.webrun.phase2.vmat

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.VMAT
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color
import scala.xml.Elem

object VMATChartHistory {
  def chartId(beamNameMLC: String): String = C3Chart.idTagPrefix + Util.textToId(beamNameMLC)

  def chartReference(beamNameMLC: String): Elem = {
    C3ChartHistory.htmlRef(VMATChartHistory.chartId(beamNameMLC))
  }
}

/**
  * Make a history chart for one VMAT beam pair.
  */
class VMATChartHistory(outputPK: Long, beamNameMLC: String) extends Logging {

  private val output = Output.get(outputPK).get
  private val machine = Machine.get(output.machinePK.get).get
  private val history = VMAT.history(machine.machinePK.get).filter(h => h.vmat.beamNameMLC.equals(beamNameMLC))

  /**
    * Get history of given beam as an array of sets of percents ordered by date and each set within that date sorted by X position.
    */
  private def getBeamHist =
    history.groupBy(h => h.vmat.leftRtplan_mm).toSeq.sortBy(g => g._1).map(g => g._2).map(hSeq => hSeq.sortBy(vmat => vmat.getTime).map(h => h.vmat.diff_pct))

  /**
    * Get index of this output in the list of output.  Used to mark the orange dot in the chart.
    */
  private def getBeamIndex = {
    val outputTime = output.dataDate.get.getTime
    val index = history.map(h => h.getTime).distinct.sorted.indexWhere(d => d == outputTime)
    if (index == -1) None else Some(index)
  }

  /**
    * Construct names for the different lines to be drawn on the chart.
    */
  private def getLineNames =
    history.map(h => (h.vmat.leftRtplan_mm + h.vmat.rightRtplan_mm) / 2).distinct.sorted.map(c => "Center mm: " + Util.fmtDbl(c))

  private val allDates = history.map(cd => cd.date).distinct.sortBy(_.getTime)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else
      MaintenanceRecord.getRange(machine.machinePK.get, allDates.minBy(_.getTime), allDates.maxBy(_.getTime)).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
  }

  // list of shades of green
  private val colorList4 = Seq(new Color(25, 99, 60), new Color(39, 151, 91), new Color(53, 203, 123), new Color(66, 255, 154))

  // longer list of shades of green
  private val colorList7 =
    Seq(new Color(25, 99, 60), new Color(32, 125, 75), new Color(39, 151, 91), new Color(46, 177, 107), new Color(53, 203, 123), new Color(60, 229, 138), new Color(66, 255, 154))

  private def chartOf(index: Int): C3ChartHistory = {
    val units = "pct"
    val beamHist = getBeamHist
    val colorList = if (4 >= beamHist.head.size) colorList4 else colorList7

    if (true) {
      new C3ChartHistory(
        Some(VMATChartHistory.chartId(beamNameMLC)),
        maintenanceRecordList,
        None, // width
        None, // height
        "Date",
        allDates,
        None, // BaselineSpec
        None, // tolerance Some(new C3Chart.Tolerance(-Config.VMATDeviationThreshold_pct, Config.VMATDeviationThreshold_pct)),
        None, // range
        getLineNames,
        units,
        beamHist,
        index,
        ".3r",
        Seq[Color]()
      )
    } else {
      // TODO rm

      val min = history.minBy(h => h.vmat.leftRtplan_mm).vmat.leftRtplan_mm
      val hist = history.filter(h => h.vmat.leftRtplan_mm == min).sortBy(h => h.getTime).map(h => h.vmat.diff_pct)

      println(
        "hist:\n    " + history
          .filter(h => h.vmat.leftRtplan_mm == min)
          .sortBy(h => h.getTime)
          .map(h => h.date + " : " + Util.fmtDbl(h.vmat.leftRtplan_mm) + " : " + Util.fmtDbl(h.vmat.diff_pct))
          .mkString("\n    ")
      )

      new C3ChartHistory(
        Some(VMATChartHistory.chartId(beamNameMLC)),
        maintenanceRecordList,
        None, // width
        None, // height
        "Date",
        allDates,
        None, // BaselineSpec
        None, // tolerance Some(new C3Chart.Tolerance(-Config.VMATDeviationThreshold_pct, Config.VMATDeviationThreshold_pct)),
        None, // range
        Seq("hey"),
        units,
        Seq(hist),
        index,
        ".3r",
        colorList
      )
    }
  }

  private val chart = {
    getBeamIndex match {
      case Some(index) =>
        Some(chartOf(index))

      case _ => None
    }
  }

  val chartScript: String = {
    if (chart.isDefined) chart.get.javascript
    else ""
  }

}
