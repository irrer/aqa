package org.aqa.webrun.phase2.centerDose

import org.aqa.Logging
import org.aqa.db.CenterDose
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
 * Make a history chart for center dose.
 */
class CenterDoseChart(outputPK: Long) extends Logging {

  val output = Output.get(outputPK).get
  val procedure = Procedure.get(output.procedurePK).get
  val input = Input.get(output.inputPK).get
  val machine = Machine.get(output.machinePK.get).get
  val history = CenterDose.recentHistory(Config.CenterDoseHistoryRange, machine.machinePK.get, procedure.procedurePK.get, output.dataDate)

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = MaintenanceRecord.
    getRange(machine.machinePK.get, allDates.min, allDates.max).
    filter(m => !(m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline.toString)))

  /**
   * Filter the history to get only center doses for the given beam, and sort by increasing date.
   */
  private def sortedHistoryForBeam(beamName: String) = {
    val sortedBeamHistory = history.filter(h => h.centerDose.beamName.equals(beamName)).sortWith((a, b) => (a.date.getTime < b.date.getTime))
    sortedBeamHistory
  }

  def chartIdOfBeam(beamName: String) = C3Chart.idTagPrefix + Phase2Util.textToId(beamName)

  def chartReferenceToBeam(beamName: String) = {
    val ciob = chartIdOfBeam(beamName)
    <div id={ ciob }>{ ciob }</div>
  }

  private def chartOfBeam(beamName: String): C3ChartHistory = {
    val sortedHistory = sortedHistoryForBeam(beamName)
    val index = sortedHistory.indexWhere(sh => sh.centerDose.outputPK == output.outputPK.get)
    val units = sortedHistory(index).centerDose.units

    new C3ChartHistory(
      Some(chartIdOfBeam(beamName)),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date", sortedHistory.map(h => h.date),
      None, // BaselineSpec
      None, // minMax
      Seq(units), units, Seq(sortedHistory.map(h => h.centerDose.dose)), index, ".5g", Seq(new Color(102, 136, 187)))
  }

  private val beamList = history.filter(h => h.centerDose.outputPK == outputPK).map(_.centerDose.beamName).distinct
  private val chartList = beamList.map(beamName => chartOfBeam(beamName))

  val chartScript = chartList.map(c => c.javascript).mkString("\n")

}
