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

package org.aqa.webrun.phase2.wedge

import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.WedgePoint
import org.aqa.web.C3Chart
import org.aqa.web.C3Chart.Tolerance
import org.aqa.web.C3ChartHistory

/**
  * Create the javascript that shows the wedge history for the given output.
  */

class WedgeChartHistory(outputPK: Long) {

  private val output: Output = Output.get(outputPK).get
  private val machinePK: Long = output.machinePK.get
  private val allHistory: Seq[WedgePoint.WedgePointHistory] = WedgePoint.history(machinePK)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val allMaintenanceRecordList: Seq[MaintenanceRecord] = MaintenanceRecord
    .getRange(machinePK, allHistory.head.output.dataDate.get, allHistory.last.output.dataDate.get)
    .filterNot(m => m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline)) // Ignore any 'Set Baseline' maintenance records.  These are deprecated from the database.

  /**
    * Index of this point on the history graph so it can be shown in a different color (orange).
    * @param beamHistory All points to be displayed.
    * @return Zero relative index.
    */
  private def indexOfThis(beamHistory: Seq[WedgePoint.WedgePointHistory]): Int = {
    val index = beamHistory.indexWhere(h => h.output.outputPK.get == output.outputPK.get)
    Math.max(0, index) // Cover case where index is not in the list.
  }

  /**
    * Create a baseline for the series.
    * @param wedgeHistory Entire history for this machine.
    * @return
    */
  private def getBaseline(wedgeHistory: WedgePoint.WedgePointHistory): Option[Baseline] = {
    val baseline = Baseline(
      baselinePK = None,
      maintenanceRecordPK = -1,
      acquisitionDate = wedgeHistory.baselineOutput.dataDate.get,
      SOPInstanceUID = None,
      id = "",
      value = wedgeHistory.baselineWedgePoint.percentOfBackground_pct.toString,
      setup = ""
    )
    Some(baseline)
  }

  private def getTolerance(baseline: Option[Baseline]): Option[C3Chart.Tolerance] = {
    if (baseline.isDefined) {
      val value = baseline.get.value.toDouble
      Some(new Tolerance(value - Config.WedgeTolerance_pct, value + Config.WedgeTolerance_pct))
    } else None
  }

  private def baselineMaintenanceList(beamHistory: Seq[WedgePoint.WedgePointHistory]): Seq[MaintenanceRecord] = {
    beamHistory
      .filter(wh => wh.wedgePoint.isBaseline)
      .map(wh =>
        MaintenanceRecord(
          maintenanceRecordPK = None,
          category = MaintenanceCategory.setBaseline,
          machinePK,
          creationTime = wh.output.dataDate.get,
          userPK = -1,
          outputPK = None,
          summary = "Baseline",
          description = "Baseline"
        )
      )
  }

  /**
    * Get the maintenance records for this machine that are in the time frame being displayed.  Also get the
    * baselines as maintenance records.
    *
    * @param beamHistory History of this beam.
    * @return List of maintenance records, sorted by time.
    */
  private def getBeamMaintenanceRecordList(beamHistory: Seq[WedgePoint.WedgePointHistory]): Seq[MaintenanceRecord] = {
    val min = beamHistory.head.output.dataDate.get.getTime
    val max = beamHistory.last.output.dataDate.get.getTime
    val inTimeRange = allMaintenanceRecordList.filter(m => (m.creationTime.getTime >= min) && (m.creationTime.getTime <= max)) ++ baselineMaintenanceList(beamHistory)
    inTimeRange.sortBy(_.creationTime.getTime)
  }

  /**
    * Generate a chart that contains the given wedge point.
    *
    * @param wedgePoint Make chart for this.
    * @return Javascript script that displays chart.
    */
  private def historyChart(wedgePoint: WedgePoint): String = {
    val chartId = C3Chart.idTagPrefix + Util.textToId(wedgePoint.wedgeBeamName)
    val beamHistory = allHistory.filter(w => w.wedgePoint.wedgeBeamName.equals(wedgePoint.wedgeBeamName) && w.wedgePoint.backgroundBeamName.equals(wedgePoint.backgroundBeamName))
    val maintenanceRecordList = getBeamMaintenanceRecordList(beamHistory)

    val xDateList = beamHistory.map(_.output.dataDate.get)
    val historyPair = beamHistory.find(_.wedgePoint.wedgePointPK.get == wedgePoint.wedgePointPK.get).get
    val baseline = getBaseline(historyPair)
    val tolerance = getTolerance(baseline)

    val yRange = {
      val mid = (100 * historyPair.baselineWedgePoint.wedgeValue_cu) / historyPair.baselineWedgePoint.backgroundValue_cu
      val range = Config.WedgeTolerance_pct * 2
      new C3Chart.YRange(mid - range, mid + range)
    }

    new C3ChartHistory(
      chartIdOpt = Some(chartId),
      maintenanceList = maintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      xDateList = xDateList,
      baseline = baseline,
      tolerance = tolerance,
      yRange = Some(yRange),
      yAxisLabels = Seq("Percent of Background"),
      yDataLabel = "Percent of Background",
      yValues = Seq(beamHistory.map(_.wedgePoint.percentOfBackground_pct)),
      yIndex = indexOfThis(beamHistory),
      yFormat = ".4g",
      yColorList = Seq(WedgeHTML.lineColor)
    ).javascript
  }

  private def makeCharts = {
    val javascript = WedgePoint.getByOutput(outputPK).map(w => historyChart(w)).mkString("\n")
    javascript
  }

  var chartScript: String = makeCharts

}
