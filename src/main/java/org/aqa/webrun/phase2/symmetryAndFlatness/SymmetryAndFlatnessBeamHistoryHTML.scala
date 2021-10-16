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

package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.SymmetryAndFlatness
import org.aqa.web.C3Chart
import org.aqa.web.C3ChartHistory

import java.awt.Color
import java.sql.Timestamp
import scala.collection.Seq

/**
  * Analyze DICOM files for symmetry and flatness.
  */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, outputPK: Long) extends Logging {

  val output: Output = Output.get(outputPK).get
  val machinePK: Long = output.machinePK.get

  private val history = SymmetryAndFlatness.history(machinePK, beamName)
  private val dateList = history.map(h => h.output.dataDate.get)

  // index of the entry being charted.
  private val yIndex = history.indexWhere(h => h.symmetryAndFlatness.outputPK == output.outputPK.get)

  private val baselineMaintenanceList = {
    history
      .filter(h => h.symmetryAndFlatness.isBaseline)
      .map(h =>
        MaintenanceRecord(
          maintenanceRecordPK = None,
          category = MaintenanceCategory.setBaseline,
          machinePK,
          creationTime = h.output.dataDate.get,
          userPK = -1,
          outputPK = None,
          machineLogPK = None,
          summary = "Baseline",
          description = "Baseline"
        )
      )
  }

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val inTimeRange = MaintenanceRecord.getRange(machinePK, history.head.output.dataDate.get, history.last.output.dataDate.get)
    val relevantBaseline =
      Baseline.filterOutUnrelatedBaselines(inTimeRange.map(itr => itr.maintenanceRecordPK.get).toSet, Set("symmetry", "flatness", "constancy")).map(_.maintenanceRecordPK.get).toSet
    inTimeRange.filter(itr => relevantBaseline.contains(itr.maintenanceRecordPK.get) || (!itr.category.equals(MaintenanceCategory.setBaseline)))
  }

  private val allMaintenanceRecords = (baselineMaintenanceList ++ MaintenanceRecordList).sortBy(_.creationTime.getTime)

  private def makeChart(chartTitle: String, toleranceRange: Double, baselineDate: Timestamp, baselineValue: Double, valueList: Seq[Double]): C3ChartHistory = {

    val chartId = C3Chart.idTagPrefix + Util.textToId(chartTitle)

    val width = None
    val height = None
    val xLabel = "Date"
    val xDateList = dateList
    val yDataLabel = chartTitle + " %"
    val yAxisLabels = Seq(chartTitle + " %")
    val yValues = Seq(valueList)
    val yFormat = ".4g"
    val yColorList = Seq(new Color(0x4477bb))
    val baseline = new Baseline(None, maintenanceRecordPK = -1, baselineDate, SOPInstanceUID = None, chartTitle, baselineValue.toString, setup = "")
    val tolerance = new C3Chart.Tolerance(baselineValue - toleranceRange, baselineValue + toleranceRange)

    val chart = new C3ChartHistory(
      chartIdOpt = Some(chartId),
      allMaintenanceRecords,
      width,
      height,
      xLabel,
      Seq(xDateList),
      baseline = Some(baseline),
      Some(tolerance),
      yRange = None,
      yAxisLabels,
      yDataLabel,
      yValues,
      yIndex,
      yFormat,
      yColorList
    )

    chart
  }

  private def makeEpidCuChart(maintenanceRecords: Seq[MaintenanceRecord], yValues: Seq[Seq[Double]], xDateList: Seq[Seq[Timestamp]]): C3ChartHistory = {

    val yColorList = Seq(
      new Color(44, 160, 44),
      new Color(255, 140, 38),
      new Color(53, 133, 187),
      new Color(218, 61, 62),
      new Color(218, 62, 218)
    )

    val allY = yValues.flatten

    val mean = allY.sum / allY.size

    val middleY = allY.sortBy(y => (y - mean).abs).take((allY.size * 0.9).round.toInt)

    val yRange = new C3Chart.YRange(middleY.min, middleY.max)

    val chart = new C3ChartHistory(
      chartIdOpt = Some(C3Chart.idTagPrefix + "EpidCu"),
      maintenanceRecords,
      width = None,
      height = None,
      xLabel = "Date",
      xDateList,
      baseline = None,
      tolerance = None,
      yRange = Some(yRange),
      yAxisLabels = Seq("top", "bottom", "left", "right", "center"),
      yDataLabel = "Average CU",
      yValues,
      yIndex,
      yFormat = ".4g",
      yColorList
    )
    chart
  }

  private def makeEpidNoiseChart(maintenanceRecords: Seq[MaintenanceRecord], yValues: Seq[Seq[Double]], xDateList: Seq[Seq[Timestamp]]): C3ChartHistory = {

    val yColorList = Seq(
      new Color(44, 160, 44),
      new Color(255, 140, 38),
      new Color(53, 133, 187),
      new Color(218, 61, 62),
      new Color(218, 62, 218)
    )

    val allY = yValues.flatten

    val mean = allY.sum / allY.size

    val middleY = allY.sortBy(y => (y - mean).abs).take((allY.size * 0.9).round.toInt)

    val yRange = new C3Chart.YRange(middleY.min, middleY.max)

    val chart = new C3ChartHistory(
      chartIdOpt = Some(C3Chart.idTagPrefix + "EpidNoise"),
      maintenanceRecords,
      width = None,
      height = None,
      xLabel = "Date",
      xDateList,
      baseline = None,
      tolerance = None,
      yRange = Some(yRange),
      yAxisLabels = Seq("top", "bottom", "left", "right", "center"),
      yDataLabel = "Coefficient of Variation",
      yValues,
      yIndex,
      yFormat = ".4g",
      yColorList
    )
    chart
  }

  val javascript: String = {
    import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis._

    val sfAndBaseline = SymmetryAndFlatness.getBaseline(machinePK, beamName, output.dataDate.get).get

    val chartAxial = {
      val valueList = history.map(h => h.symmetryAndFlatness.axialSymmetry)
      makeChart(
        axialSymmetryName,
        toleranceRange = Config.SymmetryPercentLimit,
        baselineDate = sfAndBaseline.baselineOutput.dataDate.get,
        sfAndBaseline.symmetryAndFlatness.axialSymmetry,
        valueList
      )
    }

    val chartTransverse = {
      val valueList = history.map(h => h.symmetryAndFlatness.transverseSymmetry)
      makeChart(
        transverseSymmetryName,
        toleranceRange = Config.SymmetryPercentLimit,
        baselineDate = sfAndBaseline.baselineOutput.dataDate.get,
        sfAndBaseline.symmetryAndFlatness.transverseSymmetry,
        valueList
      )
    }

    val chartFlatness = {
      val valueList = history.map(h => h.symmetryAndFlatness.flatness)
      makeChart(
        flatnessName,
        toleranceRange = Config.FlatnessPercentLimit,
        baselineDate = sfAndBaseline.baselineOutput.dataDate.get,
        sfAndBaseline.symmetryAndFlatness.flatness,
        valueList
      )
    }

    val chartProfileConstancy = {
      val valueList = history.map(h => h.symmetryAndFlatness.profileConstancy(h.baseline))
      makeChart(
        profileConstancyName,
        toleranceRange = Config.ProfileConstancyPercentLimit,
        baselineDate = sfAndBaseline.baselineOutput.dataDate.get,
        sfAndBaseline.symmetryAndFlatness.profileConstancy(sfAndBaseline.symmetryAndFlatness),
        valueList
      )
    }

    val chartEpidCU = {

      val valueList = Seq(
        history.map(h => h.symmetryAndFlatness.top_cu),
        history.map(h => h.symmetryAndFlatness.bottom_cu),
        history.map(h => h.symmetryAndFlatness.left_cu),
        history.map(h => h.symmetryAndFlatness.right_cu),
        history.map(h => h.symmetryAndFlatness.center_cu)
      )

      makeEpidCuChart(allMaintenanceRecords, valueList, Seq(dateList, dateList, dateList, dateList, dateList))
    }

    val chartEpidNoise = {

      val covHistory = history.filter(h =>
        (h.symmetryAndFlatness.topStdDev_cu != -1) &&
          (h.symmetryAndFlatness.bottomStdDev_cu != -1) &&
          (h.symmetryAndFlatness.leftStdDev_cu != -1) &&
          (h.symmetryAndFlatness.rightStdDev_cu != -1) &&
          (h.symmetryAndFlatness.centerStdDev_cu != -1)
      )

      val covDateList = covHistory.map(h => h.output.dataDate.get)

      val valueList = Seq(
        covHistory.map(h => h.symmetryAndFlatness.topCOV),
        covHistory.map(h => h.symmetryAndFlatness.bottomCOV),
        covHistory.map(h => h.symmetryAndFlatness.leftCOV),
        covHistory.map(h => h.symmetryAndFlatness.rightCOV),
        covHistory.map(h => h.symmetryAndFlatness.centerCOV)
      )

      val minDate = covHistory.minBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
      val maxDate = covHistory.maxBy(_.output.dataDate.get.getTime).output.dataDate.get.getTime
      val covMaintenanceList = allMaintenanceRecords.filter(mr => (mr.creationTime.getTime >= minDate) && (mr.creationTime.getTime <= maxDate))

      makeEpidNoiseChart(covMaintenanceList, valueList, Seq(covDateList, covDateList, covDateList, covDateList, covDateList))
    }

    chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript + chartProfileConstancy.javascript + chartEpidCU.javascript + chartEpidNoise.javascript
  }

}
