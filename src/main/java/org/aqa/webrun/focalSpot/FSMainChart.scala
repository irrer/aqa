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

  def chartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id)

  def chartReference: Elem = {
    C3ChartHistory.htmlRef(chartId)
  }

  private val setList = {

    /** Focal spot sets. */
    val history = FocalSpotSet.history(machine.machinePK.get)

    val list = history.groupBy(_.output.outputPK.get).values.map(_.sortBy(f => "%020.2f".format(f.focalSpotSet.KVP_kv) + " " + f.focalSpotSet.isFFF)).toSeq
    val max = {
      if (list.isEmpty)
        0
      else
        list.map(_.size).max
    }
    list.filter(_.size == max).sortBy(_.head.output.dataDate.get.getTime)
  }

  private val allDates = setList.map(_.head.output.dataDate.get).groupBy(_.getTime).values.map(_.head).toSeq.sortBy(_.getTime)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList =
    MaintenanceRecord.getRange(machine.machinePK.get, allDates.minBy(_.getTime), allDates.maxBy(_.getTime)).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))

  private def chartOf: C3ChartHistory = {
    val index = setList.indexWhere(sh => sh.head.focalSpotSet.outputPK == output.outputPK.get)
    val units = "mm"

    // make a list of sets of data, each group sorted by KV

    val yValues = {
      def getX(i: Int): Seq[Double] = setList.map(s => s(i).focalSpotSet.focalSpotAlignmentX_mm)
      def getY(i: Int): Seq[Double] = setList.map(s => s(i).focalSpotSet.focalSpotAlignmentY_mm)

      val list = setList.head.indices.flatMap(i => Seq(getX(i), getY(i)))
      list
    }

    val yAxisLabels: Seq[String] = {
      def name(fsSet: FocalSpotSet): String = {
        fsSet.mvText + (if (fsSet.isFFF) "-FFF" else "")
      }

      setList.head.flatMap(h => Seq(name(h.focalSpotSet) + " X", name(h.focalSpotSet) + " Y"))
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
