/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2.centerDose

import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CenterDose
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color
import scala.xml.Elem

/**
  * Make a history chart for center dose.
  */
class CenterDoseChart(outputPK: Long) extends Logging {

  val output: Output = Output.get(outputPK).get
  val procedure: Procedure = Procedure.get(output.procedurePK).get
  val input: Input = Input.get(output.inputPK).get
  val machine: Machine = Machine.get(output.machinePK.get).get

  val history: Seq[CenterDose.CenterDoseHistory] = CenterDose.history(machine.machinePK.get)

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList =
    MaintenanceRecord.getRange(machine.machinePK.get, allDates.minBy(_.getTime), allDates.maxBy(_.getTime)).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))

  /**
    * Filter the history to get only center doses for the given beam, and sort by increasing date.
    */
  private def sortedHistoryForBeam(beamName: String) = {
    val thisTime = output.dataDate.get.getTime
    val all = history.filter(h => h.centerDose.beamName.equals(beamName)).sortBy(_.getTime)
    val before = all.filter(cd => cd.getTime < thisTime).takeRight(Config.CenterDoseHistoryRange)
    val after = all.filter(cd => cd.getTime >= thisTime).take(Config.CenterDoseHistoryRange + 1)
    val sortedBeamHistory = before ++ after
    Trace.trace(all.size)
    Trace.trace(before.size)
    Trace.trace(after.size)
    Trace.trace(sortedBeamHistory.size)
    sortedBeamHistory
  }

  def chartIdOfBeam(beamName: String): String = C3Chart.idTagPrefix + Util.textToId(beamName)

  def chartReferenceToBeam(beamName: String): Elem = {
    val chartId = chartIdOfBeam(beamName)
    C3ChartHistory.htmlRef(chartId)
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
      "Date",
      sortedHistory.map(h => h.date),
      None, // BaselineSpec
      None, // tolerance
      None, // yRange
      Seq(units),
      units,
      Seq(sortedHistory.map(h => h.centerDose.dose)),
      index,
      ".5g",
      Seq(new Color(102, 136, 187))
    )
  }

  private val beamList = history.filter(h => h.centerDose.outputPK == outputPK).map(_.centerDose.beamName).distinct
  private val chartList = beamList.map(beamName => chartOfBeam(beamName))

  val chartScript: String = chartList.map(c => c.javascript).mkString("\n")

}
