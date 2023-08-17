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

package org.aqa.webrun.bbByEpid

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.Input
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color

/**
  * Make a history chart for BBbyEPID.
  */
class BBbyEPIDChart(outputPK: Long) extends Logging {

  val output: Output = Output.get(outputPK).get
  val procedure: Procedure = Procedure.get(output.procedurePK).get
  val input: Input = Input.get(output.inputPK).get
  val machine: Machine = Machine.get(output.machinePK.get).get
  val history: Seq[BBbyEPIDComposite.BBbyEPIDCompositeHistory] = BBbyEPIDComposite.history(machine.machinePK.get, procedure.procedurePK.get)

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else
      MaintenanceRecord.getRange(machine.machinePK.get, allDates.min, allDates.max).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
  }

  def chartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id)

  private def chartOf(index: Int): C3ChartHistory = {
    val units = "mm"
    val dataToBeGraphed = Seq(
      history.map(h => h.bbByEPIDComposite.offsetAdjusted_mm.get),
      history.map(h => h.bbByEPIDComposite.xAdjusted_mm.get),
      history.map(h => h.bbByEPIDComposite.yAdjusted_mm.get),
      history.map(h => h.bbByEPIDComposite.zAdjusted_mm.get)
    )

    val colorList = Seq(new Color(102, 136, 187), new Color(104, 187, 154), new Color(104, 187, 112), new Color(137, 187, 104))

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date",
      Seq(history.map(h => h.date)),
      None, // BaselineSpec
      Some(new C3Chart.Tolerance(-Config.BBbyEPIDChartTolerance_mm, Config.BBbyEPIDChartTolerance_mm)),
      Some(new C3Chart.YRange(-Config.BBbyEPIDChartYRange_mm, Config.BBbyEPIDChartYRange_mm)),
      Seq("Total offset", "X offset", "Y offset", "Z offset"),
      units,
      dataToBeGraphed,
      index,
      ".3r",
      colorList
    )
  }

  private val chart = {
    val index = history.indexWhere(sh => sh.bbByEPIDComposite.outputPK == output.outputPK.get)
    Some(chartOf(index))
  }

  val chartScript: String = {
    if (chart.isDefined) chart.get.javascript
    else ""
  }

}
