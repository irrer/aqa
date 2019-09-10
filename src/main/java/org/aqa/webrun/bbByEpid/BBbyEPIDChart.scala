package org.aqa.webrun.bbByEpid

import org.aqa.Logging
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.BBbyEPIDComposite
import org.aqa.Config
import org.aqa.db.MaintenanceRecord
import org.aqa.db.MaintenanceCategory
import org.aqa.web.C3Chart
import org.aqa.Util
import org.aqa.web.C3ChartHistory
import java.awt.Color

/**
 * Make a history chart for BBbyEPID.
 */
class BBbyEPIDChart(outputPK: Long) extends Logging {

  val output = Output.get(outputPK).get
  val procedure = Procedure.get(output.procedurePK).get
  val input = Input.get(output.inputPK).get
  val machine = Machine.get(output.machinePK.get).get
  val history = BBbyEPIDComposite.recentHistory(Config.BBbyEPIDHistoryRange, machine.machinePK.get, procedure.procedurePK.get, output.dataDate)

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else
      MaintenanceRecord.
        getRange(machine.machinePK.get, allDates.min, allDates.max).
        filter(m => !(m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline.toString)))
  }

  def chartId = C3Chart.idTagPrefix + Util.textToId(machine.id)

  def chartReference = {
    val ciob = chartId
    <div id={ ciob }>{ ciob }</div>
  }

  private def chartOf(index: Int): C3ChartHistory = {
    val units = "mm"
    val dataToBeGraphed = Seq(
      history.map(h => h.bbByEPIDComposite.offset_mm),
      history.map(h => h.bbByEPIDComposite.x_mm),
      history.map(h => h.bbByEPIDComposite.y_mm),
      history.map(h => h.bbByEPIDComposite.z_mm))

    val colorList = Seq(
      new Color(102, 136, 187),
      new Color(104, 187, 154),
      new Color(104, 187, 112),
      new Color(137, 187, 104))

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date", history.map(h => h.date),
      None, // BaselineSpec
      None, // minMax
      Seq("Total offset", "X offset", "Y offset", "Z offset"), units, dataToBeGraphed, index, ".3r", colorList)
  }

  private val chart = {
    val index = history.indexWhere(sh => sh.bbByEPIDComposite.outputPK == output.outputPK.get)
    if (index == -1)
      None
    else
      Some(chartOf(index))
  }

  val chartScript = {
    if (chart.isDefined) chart.get.javascript
    else ""
  }

}
