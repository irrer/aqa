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

package org.aqa.webrun.focalSpot

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.FocalSpotSet
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color
import java.util.Date
import scala.xml.Elem

/**
  * Make a history chart for BBbyCBCT.
  */
class FSMainChart(outputPK: Long) extends Logging {

  private val output: Output = Output.get(outputPK).get
  private val procedure: Procedure = Procedure.get(output.procedurePK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get

  /** Focal spot sets. */
  private val history = FocalSpotSet.history(machine.machinePK.get, procedure.procedurePK.get)

  private val allDates = history.map(_.output.dataDate.get).groupBy(_.getTime).values.map(_.head).toSeq.sortBy(_.getTime)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList =
    MaintenanceRecord.getRange(machine.machinePK.get, allDates.minBy(_.getTime), allDates.maxBy(_.getTime)).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))

  def chartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id)

  def chartReference: Elem = {
    C3ChartHistory.htmlRef(chartId)
  }

  private val setList = history.groupBy(_.output.outputPK.get).values.map(_.sortBy(_.focalSpotSet.KVP_kv)).toSeq

  private def chartOf: C3ChartHistory = {
    val index = history.indexWhere(sh => sh.focalSpotSet.outputPK == output.outputPK.get)
    val units = "mm"

    // make a list of sets of data, each group sorted by KV

    val yValues = {
      def getX(i: Int): Seq[Double] = setList.map(s => s(i).focalSpotSet.focalSpotAlignmentX_mm)
      def getY(i: Int): Seq[Double] = setList.map(s => s(i).focalSpotSet.focalSpotAlignmentY_mm)

      val list = setList.head.indices.flatMap(i => Seq(getX(i), getY(i)))
      list
    }

    val yAxisLabels: Seq[String] = {
      def mvText(fsSet: FocalSpotSet): String = {
        val mv = fsSet.KVP_kv / 1000.0
        if (mv.round == mv) mv.round.toString else Util.fmtDbl(mv)
      }

      setList.head.flatMap(h => Seq(h.focalSpotSet.mvText + " X", h.focalSpotSet.mvText + " Y"))
    }

    val colorList = Seq(Color.green, Color.blue, Color.black, Color.red, Color.orange, Color.cyan, Color.magenta, Color.yellow, Color.pink, Color.gray)

    new C3ChartHistory(
      chartIdOpt = Some(chartId),
      maintenanceList = maintenanceRecordList,
      width = None,
      height = None,
      "Date",
      xDateList = Seq(allDates.map(_.asInstanceOf[Date])),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yAxisLabels,
      yDataLabel = units,
      yValues = yValues,
      yIndex = index,
      yFormat = ".3r",
      yColorList = colorList
    )
  }

  val chart: C3ChartHistory = chartOf

  val chartScript: String = chart.javascript
}
