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

package org.aqa.webrun.phase2.collimatorCentering

import org.aqa.Logging
import org.aqa.db.CollimatorCentering
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color
import java.sql.Timestamp
import java.util.Date

/**
  * Make history charts for collimator centering.
  */
class CollimatorCenteringChart(outputPK: Long) extends Logging {

  private val output: Output = Output.get(outputPK).get
  private val machinePK: Long = output.machinePK.get

  private val history: Seq[CollimatorCentering.ColCentHistory] = CollimatorCentering.history(machinePK)

  private val idSummary: String = C3Chart.idTagPrefix + "CollCenterSummary"
  private val id090: String = C3Chart.idTagPrefix + "CollCenter090"
  private val id270: String = C3Chart.idTagPrefix + "CollCenter270"

  private val allDates: Seq[Timestamp] = history.map(_.output.dataDate.get)
  private val maintenanceRecordList: Seq[MaintenanceRecord] = MaintenanceRecord.getRange(machinePK, allDates.minBy(_.getTime), allDates.maxBy(_.getTime))
  private val xDateList = Seq(allDates.map(d => new Date(d.getTime)))

  private val index: Int = history.indexWhere(h => h.output.outputPK.get == output.outputPK.get)

  private val yColorList = Seq(0xff0000, 0x00ff00, 0x00ffff, 0x0000ff, 0xffff00, 0x000000, 0xff00ff, 0xffc800, 0xffff00, 0xff00ff, 0x808080).map(i => new Color(i))

  private def makeChart(id: String, yAxisLabels: Seq[String], yValues: Seq[Seq[Double]]): C3ChartHistory = {
    new C3ChartHistory(
      chartIdOpt = Some(id),
      maintenanceList = maintenanceRecordList,
      width = None, // width
      height = None, // height
      xLabel = "Date",
      xDateList = xDateList,
      baseline = None, // BaselineSpec
      tolerance = None, // tolerance
      yRange = None, // yRange
      yAxisLabels = yAxisLabels,
      yDataLabel = "mm",
      yValues = yValues,
      yIndex = index,
      yFormat = ".5g",
      yColorList = yColorList
    )
  }

  val summary: C3ChartHistory = {
    val yValues = Seq(
      history.map(_.colCent.center.distance(0, 0)),
      history.map(_.colCent.xCollimatorCenter_mm),
      history.map(_.colCent.yCollimatorCenter_mm)
    )
    makeChart(
      idSummary,
      Seq("XY distance", "X", "Y"),
      yValues
    )
  }

  val collCenter090: C3ChartHistory = {
    val yValues = Seq(
      history.map(_.colCent.X1_090_mm),
      history.map(_.colCent.Y1_090_mm),
      history.map(_.colCent.X2_270_mm),
      history.map(_.colCent.Y2_270_mm)
    )
    makeChart(
      id090,
      Seq("X1 90", "X2 90", "Y1 90", "Y2 90"),
      yValues
    )
  }

  val collCenter270: C3ChartHistory = {
    val yValues = Seq(
      history.map(_.colCent.X2_090_mm),
      history.map(_.colCent.Y2_090_mm),
      history.map(_.colCent.X1_270_mm),
      history.map(_.colCent.Y1_270_mm)
    )
    makeChart(
      id270,
      Seq("X1 270", "X2 270", "Y1 270", "Y2 270"),
      yValues
    )
  }

  val javascript: String = Seq(summary, collCenter090, collCenter270).map(_.javascript).mkString("\n")

}
