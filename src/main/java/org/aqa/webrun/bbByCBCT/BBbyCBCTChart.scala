package org.aqa.webrun.bbByCBCT

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory2

import java.awt.Color
import scala.xml.Elem

/**
  * Make a history chart for BBbyCBCT.
  */
class BBbyCBCTChart(outputPK: Long) extends Logging {

  val output: Output = Output.get(outputPK).get
  val procedure: Procedure = Procedure.get(output.procedurePK).get
  val input: Input = Input.get(output.inputPK).get
  val machine: Machine = Machine.get(output.machinePK.get).get

  /** A list all items for the given time range, but limited to the last one done for each day. */
  val history: Seq[BBbyCBCT.BBbyCBCTHistory] = {
    val hList = BBbyCBCT.history(machine.machinePK.get, procedure.procedurePK.get)
    val onePerDay = hList.groupBy(h => edu.umro.ScalaUtil.Util.roundToDate(h.date)).map(hh => hh._2.maxBy(_.date.getTime))
    onePerDay.toSeq
  }

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList =
    MaintenanceRecord.getRange(machine.machinePK.get, allDates.min, allDates.max).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))

  def chartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id)

  def chartReference: Elem = {
    C3ChartHistory2.htmlRef(chartId)
  }

  private def chartOf: C3ChartHistory2 = {
    val index = history.indexWhere(sh => sh.bbByCBCT.outputPK == output.outputPK.get)
    val units = "mm"
    val dataToBeGraphed = Seq(
      history.map(h => h.bbByCBCT.offset_mm),
      history.map(h => h.bbByCBCT.err_mm.getX),
      history.map(h => h.bbByCBCT.err_mm.getY),
      history.map(h => h.bbByCBCT.err_mm.getZ)
    )

    val colorList = Seq(new Color(102, 136, 187), new Color(104, 187, 154), new Color(104, 187, 112), new Color(137, 187, 104))

    new C3ChartHistory2(
      Some(chartId),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date",
      history.map(h => h.date),
      None, // BaselineSpec
      Some(new C3Chart.Tolerance(-Config.BBbyCBCTChartTolerance_mm, Config.BBbyCBCTChartTolerance_mm)), // tolerance
      Some(new C3Chart.YRange(-Config.BBbyCBCTChartYRange_mm, Config.BBbyCBCTChartYRange_mm)), // yRange
      Seq("Total offset", "X offset", "Y offset", "Z offset"),
      units,
      dataToBeGraphed,
      index,
      ".3r",
      colorList
    )
  }

  private val chart = chartOf

  val chartScript: String = chart.javascript
}
