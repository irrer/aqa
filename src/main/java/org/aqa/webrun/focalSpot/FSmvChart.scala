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
import org.aqa.db.FocalSpot
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

/**
  * Make a history chart for BBbyCBCT.
  */
class FSmvChart(outputPK: Long, mv: Double, fluenceName: String) extends Logging {

  private val units = "mm"
  private val output: Output = Output.get(outputPK).get
  private val procedure: Procedure = Procedure.get(output.procedurePK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get

  private def chartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id + "_" + mv)

  private def edgeChartId: String = C3Chart.idTagPrefix + Util.textToId(machine.id + "_Edge_" + mv)

  private val setHist = {

    /** Focal spot sets. */
    val setHistory = FocalSpotSet.history(machine.machinePK.get, procedure.procedurePK.get, mv * 1000, fluenceName.equals("FFF"))
    val fsHistory = FocalSpot.history(machine.machinePK.get, procedure.procedurePK.get, mv * 1000, fluenceName.equals("FFF"))
    val fsMap = fsHistory.map(h => (h.focalSpot.focalSpotPK.get, h.focalSpot)).toMap

    setHistory
      .map(s => SetHist(s.output, s.focalSpotSet, fsMap(s.focalSpotSet.jaw090PK), fsMap(s.focalSpotSet.jaw270PK), fsMap(s.focalSpotSet.mlc090PK), fsMap(s.focalSpotSet.mlc270PK)))
      .sortBy(_.output.dataDate.get.getTime)
  }

  private val allDates = setHist.map(_.output.dataDate.get.getTime).distinct.sorted.map(ms => new Date(ms))

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList =
    MaintenanceRecord.getRange(machine.machinePK.get, allDates.head, allDates.last).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))

  // def chartReference: Elem = C3ChartHistory.htmlRef(chartId)

  private case class SetHist(
      output: Output,
      focalSpotSet: FocalSpotSet,
      jaw090: FocalSpot,
      jaw270: FocalSpot,
      mlc090: FocalSpot,
      mlc270: FocalSpot
  ) {
    val jawCenterX: Double = (jaw090.centerX + jaw270.centerX) / 2
    val jawCenterY: Double = (jaw090.centerY + jaw270.centerY) / 2

    val mlcCenterX: Double = (mlc090.centerX + mlc270.centerX) / 2
    val mlcCenterY: Double = (mlc090.centerY + mlc270.centerY) / 2

  }

  private val index = setHist.indexWhere(sh => sh.output.outputPK.get == output.outputPK.get)

  private case class ChartLine(data: () => Seq[Double], name: String, color: Color) {}

  private val black1 = new Color(0, 0, 0)
  private val black2 = new Color(45, 45, 45)
  private val black3 = new Color(90, 90, 90)
  private val black4 = new Color(135, 135, 135)

  private val blue1 = new Color(0, 0, 255)
  private val blue2 = new Color(50, 50, 255)
  private val blue3 = new Color(100, 100, 255)
  private val blue4 = new Color(150, 150, 255)

  private val red1 = new Color(200, 0, 0)
  private val red2 = new Color(255, 50, 50)
  private val red3 = new Color(255, 100, 100)
  private val red4 = new Color(255, 150, 150)

  private val green1 = new Color(60, 150, 60)
  private val green2 = new Color(0, 200, 0)
  private val green3 = new Color(120, 255, 120)
  private val green4 = new Color(180, 255, 180)

  def chartPair: (C3ChartHistory, C3ChartHistory) = {

    val chartLineList: Seq[ChartLine] = Seq(
      ChartLine(() => setHist.map(_.focalSpotSet.focalSpotAlignmentX_mm), "Focal Spot X", black1),
      ChartLine(() => setHist.map(_.focalSpotSet.focalSpotAlignmentY_mm), "Focal Spot Y", black3),
      //
      ChartLine(() => setHist.map(_.jawCenterX), "Jaw Center X", blue1),
      ChartLine(() => setHist.map(_.jawCenterY), "Jaw Center Y", blue2),
      ChartLine(() => setHist.map(_.mlcCenterX), "MLC Center X", blue3),
      ChartLine(() => setHist.map(_.mlcCenterY), "MLC Center Y", blue4),
      //
      ChartLine(() => setHist.map(_.jaw090.centerX), "Jaw 90 Center X", red1),
      ChartLine(() => setHist.map(_.jaw090.centerY), "Jaw 90 Center Y", red2),
      ChartLine(() => setHist.map(_.jaw270.centerX), "Jaw 270 Center X", red3),
      ChartLine(() => setHist.map(_.jaw270.centerY), "Jaw 270 Center Y", red4),
      //
      ChartLine(() => setHist.map(_.mlc090.centerX), "MLC 90 Center X", green1),
      ChartLine(() => setHist.map(_.mlc090.centerY), "MLC 90 Center Y", green2),
      ChartLine(() => setHist.map(_.mlc270.centerX), "MLC 270 Center X", green3),
      ChartLine(() => setHist.map(_.mlc270.centerY), "MLC 270 Center Y", green4)
    )

    val yValues: Seq[Seq[Double]] = chartLineList.map(_.data())

    val yAxisLabels: Seq[String] = chartLineList.map(_.name)

    val colorList = chartLineList.map(_.color)

    val chart = new C3ChartHistory(
      chartIdOpt = Some(chartId),
      maintenanceList = maintenanceRecordList,
      width = None,
      height = None,
      "Date",
      xDateList = Seq.fill(chartLineList.size)(allDates),
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

    // ------------------------------------------------------------

    val edgeChartLineList: Seq[ChartLine] = Seq(
      ChartLine(() => setHist.map(_.jaw090.topEdgeError_mm), "Jaw 90 Top Edge Error", black1),
      ChartLine(() => setHist.map(_.jaw270.topEdgeError_mm), "Jaw 270 Top Edge Error", black2),
      ChartLine(() => setHist.map(_.mlc090.topEdgeError_mm), "MLC 90 Top Edge Error", black3),
      ChartLine(() => setHist.map(_.mlc270.topEdgeError_mm), "MLC 270 Top Edge Error", black4),
      //
      ChartLine(() => setHist.map(_.jaw090.bottomEdgeError_mm), "Jaw 90 Bottom Edge Error", blue1),
      ChartLine(() => setHist.map(_.jaw270.bottomEdgeError_mm), "Jaw 270 Bottom Edge Error", blue2),
      ChartLine(() => setHist.map(_.mlc090.bottomEdgeError_mm), "MLC 90 Bottom Edge Error", blue3),
      ChartLine(() => setHist.map(_.mlc270.bottomEdgeError_mm), "MLC 270 Bottom Edge Error", blue4),
      //
      ChartLine(() => setHist.map(_.jaw090.leftEdgeError_mm), "Jaw 90 Left Edge Error", green1),
      ChartLine(() => setHist.map(_.jaw270.leftEdgeError_mm), "Jaw 270 Left Edge Error", green2),
      ChartLine(() => setHist.map(_.mlc090.leftEdgeError_mm), "MLC 90 Left Edge Error", green3),
      ChartLine(() => setHist.map(_.mlc270.leftEdgeError_mm), "MLC 270 Left Edge Error", green4),
      //
      ChartLine(() => setHist.map(_.jaw090.rightEdgeError_mm), "Jaw 90 Right Edge Error", red1),
      ChartLine(() => setHist.map(_.jaw270.rightEdgeError_mm), "Jaw 270 Right Edge Error", red2),
      ChartLine(() => setHist.map(_.mlc090.rightEdgeError_mm), "MLC 90 Right Edge Error", red3),
      ChartLine(() => setHist.map(_.mlc270.rightEdgeError_mm), "MLC 270 Right Edge Error", red4)
    )

    val edgeYValues: Seq[Seq[Double]] = edgeChartLineList.map(_.data())

    val edgeYAxisLabels: Seq[String] = edgeChartLineList.map(_.name)

    val edgeColorList = edgeChartLineList.map(_.color)

    val edgeChart = new C3ChartHistory(
      chartIdOpt = Some(edgeChartId),
      maintenanceList = maintenanceRecordList,
      width = None,
      height = None,
      "Date",
      xDateList = Seq.fill(edgeChartLineList.size)(allDates),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = edgeYAxisLabels,
      yDataLabel = units,
      yValues = edgeYValues,
      yIndex = index,
      yFormat = ".3r",
      yColorList = edgeColorList
    )

    (chart, edgeChart)

  }

  val chart: (C3ChartHistory, C3ChartHistory) = chartPair
}
