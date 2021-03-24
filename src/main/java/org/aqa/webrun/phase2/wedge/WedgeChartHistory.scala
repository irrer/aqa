package org.aqa.webrun.phase2.wedge

import org.aqa.db.WedgePoint
import org.aqa.web.C3ChartHistory
import org.aqa.db.Baseline
import org.aqa.web.C3Chart
import org.aqa.web.C3Chart.Tolerance
import edu.umro.ScalaUtil.Trace
import org.aqa.db.MaintenanceRecord
import org.aqa.Util
import org.aqa.Config
import org.aqa.db.Output
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.MaintenanceCategory

/**
 * Create the javascript that shows the wedge history for the given output.
 */

class WedgeChartHistory(outputPK: Long) {

  val output = Output.get(outputPK).get
  val machinePK = output.machinePK.get

  val allHistory = WedgePoint.recentHistory(machinePK)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  val allMaintenanceRecordList = MaintenanceRecord.
    getRange(machinePK, allHistory.head.date, allHistory.last.date) //.filter(m => !(m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline.toString)))

  def indexOfThis(beamHistory: Seq[WedgePoint.WedgePointHistory1]): Int = {
    val i = beamHistory.indexWhere(h => h.outputPK == output.outputPK.get)
    Math.max(0, i)
  }

  def currentWedgePoint(history: Seq[WedgePoint]): WedgePoint = {
    history.find(w => w.outputPK == output.outputPK.get).get
  }

  def getBaseline(wedgePoint: WedgePoint): Option[Baseline] = {
    val maintenanceRecordBaseline = Baseline.findLatest(machinePK, WedgeAnalysis.makeWedgeBaselineName(wedgePoint), output.dataDate.get)
    if (maintenanceRecordBaseline.isDefined) Some(maintenanceRecordBaseline.get._2) else None
  }

  def getTolerance(baseline: Option[Baseline]): Option[C3Chart.Tolerance] = {
    if (baseline.isDefined) {
      val value = baseline.get.value.toDouble
      Some(new Tolerance(value - Config.WedgeTolerance_pct, value + Config.WedgeTolerance_pct))
    } else None
  }

  def historyChart(wedgePoint: WedgePoint) = {
    val chartId = C3Chart.idTagPrefix + Util.textToId(wedgePoint.wedgeBeamName)
    val beamHistory = allHistory.filter(h => h.wedgeBeamName.equalsIgnoreCase(wedgePoint.wedgeBeamName)).sortBy(_.date)
    val maintenanceRecordList = {
      val min = beamHistory.head.date.getTime
      val max = beamHistory.last.date.getTime
      val inTimeRange = allMaintenanceRecordList.filter(m => (m.creationTime.getTime >= min) && (m.creationTime.getTime <= max))

      val releventBaseline = Baseline.filterOutUnrelatedBaselines(inTimeRange.map(itr => itr.maintenanceRecordPK.get).toSet, Set("wedge")).map(_.maintenanceRecordPK.get).toSet

      inTimeRange.filter(itr => releventBaseline.contains(itr.maintenanceRecordPK.get) || (!itr.category.equals(MaintenanceCategory.setBaseline)))
    }
    val xDateList = beamHistory.map(_.date)
    val baseline = getBaseline(wedgePoint)
    val tolerance = getTolerance(baseline)

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      None, None, // chart width, height
      "Date", // x axis label
      xDateList, // xDateList
      baseline, // baseline
      tolerance, // tolerance
      None, // yRange
      Seq("Percent of Background"), // y axis labels
      "Percent of Background", // y data label
      Seq(beamHistory.map(_.percentOfBackground_pct)), // y values to plot
      indexOfThis(beamHistory), // index of y value that is new
      ".4g", // y number format
      Seq(WedgeHTML.lineColor) // y line colors
    ).javascript
  }

  private def makeCharts = {
    val javascript = WedgePoint.getByOutput(outputPK).map(w => historyChart(w)).mkString("\n")
    javascript
  }

  var chartScript: String = makeCharts

}
