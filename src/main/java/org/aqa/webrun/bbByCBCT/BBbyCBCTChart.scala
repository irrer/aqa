package org.aqa.webrun.bbByCBCT

import org.aqa.Logging
import org.aqa.db.BBbyCBCT
import java.text.SimpleDateFormat
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import org.aqa.db.MaintenanceRecord
import java.awt.Color
import org.aqa.webrun.ExtendedData
import edu.umro.ScalaUtil.Trace
import org.aqa.db.Output
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.Config
import org.aqa.web.C3Chart
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.MaintenanceCategory

/**
 * Make a history chart for BBbyCBCT.
 */
class BBbyCBCTChart(outputPK: Long) extends Logging {
  Trace.trace

  val output = Output.get(outputPK).get
  Trace.trace
  val procedure = Procedure.get(output.procedurePK).get
  Trace.trace
  val input = Input.get(output.inputPK).get
  Trace.trace
  val machine = Machine.get(output.machinePK.get).get
  Trace.trace
  /** A list all items for the given time range, but limited to the last one done for each day. */
  val history = {
    Trace.trace
    val hList = BBbyCBCT.history(machine.machinePK.get, procedure.procedurePK.get)
    Trace.trace
    val onePerDay = hList.groupBy(h => edu.umro.ScalaUtil.Util.roundToDate(h.date)).map(hh => hh._2.sortBy(_.date.getTime).last)
    Trace.trace
    onePerDay.toSeq
  }
  Trace.trace

  private val allDates = history.map(cd => cd.date)
  Trace.trace

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = MaintenanceRecord.
    getRange(machine.machinePK.get, allDates.min, allDates.max).
    filter(m => !(m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline.toString)))
  Trace.trace

  def chartId = C3Chart.idTagPrefix + Util.textToId(machine.id)

  def chartReference = {
    val ciob = chartId
    <div id={ ciob }>{ ciob }</div>
  }
  Trace.trace

  private def chartOf: C3ChartHistory = {
    Trace.trace
    val index = history.indexWhere(sh => sh.bbByCBCT.outputPK == output.outputPK.get)
    Trace.trace
    val units = "mm"
    Trace.trace
    val dataToBeGraphed = Seq(
      history.map(h => h.bbByCBCT.offset_mm),
      history.map(h => h.bbByCBCT.rtplanX_mm - h.bbByCBCT.cbctX_mm),
      history.map(h => h.bbByCBCT.rtplanY_mm - h.bbByCBCT.cbctY_mm),
      history.map(h => h.bbByCBCT.rtplanZ_mm - h.bbByCBCT.cbctZ_mm))
    Trace.trace

    val colorList = Seq(
      new Color(102, 136, 187),
      new Color(104, 187, 154),
      new Color(104, 187, 112),
      new Color(137, 187, 104))
    Trace.trace

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date", history.map(h => h.date),
      None, // BaselineSpec
      Some(new C3Chart.Tolerance(-Config.BBbyCBCTChartTolerance_mm, Config.BBbyCBCTChartTolerance_mm)), // tolerance
      Some(new C3Chart.YRange(-Config.BBbyCBCTChartYRange_mm, Config.BBbyCBCTChartYRange_mm)), // yRange
      Seq("Total offset", "X offset", "Y offset", "Z offset"), units, dataToBeGraphed, index, ".3r", colorList)
  }
  Trace.trace

  private val chart = chartOf
  Trace.trace

  val chartScript = chart.javascript
  Trace.trace

}
