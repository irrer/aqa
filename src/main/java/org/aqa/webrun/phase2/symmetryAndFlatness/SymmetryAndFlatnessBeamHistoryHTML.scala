package org.aqa.webrun.phase2.symmetryAndFlatness

import edu.umro.ScalaUtil.Trace
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
  private val dateList = history.map(h => h.timestamp)

  // index of the entry being charted.
  private val yIndex = history.indexWhere(h => h.symmetryAndFlatness.outputPK == output.outputPK.get)

  private val baselineMaintenanceList = {
    history.filter(h => h.symmetryAndFlatness.isBaseline).map(h =>
      MaintenanceRecord(
        maintenanceRecordPK = None,
        category = "Set Baseline",
        machinePK,
        creationTime = h.timestamp,
        userPK = -1,
        outputPK = None,
        summary = "Baseline",
        description = "Baseline"
      ))
  }

  // list of all MaintenanceRecords in this time interval
  private val MaintenanceRecordList = {
    val inTimeRange = MaintenanceRecord.getRange(machinePK, history.head.timestamp, history.last.timestamp)
    val relevantBaseline = Baseline.filterOutUnrelatedBaselines(inTimeRange.map(itr => itr.maintenanceRecordPK.get).toSet, Set("symmetry", "flatness", "constancy")).map(_.maintenanceRecordPK.get).toSet
    inTimeRange.filter(itr => relevantBaseline.contains(itr.maintenanceRecordPK.get) || (!itr.category.equals(MaintenanceCategory.setBaseline)))
  }


  private val allMaintenanceRecords = (baselineMaintenanceList ++ MaintenanceRecordList).sortBy(_.creationTime.getTime)

  private def makeChart(
                         chartTitle: String,
                         toleranceRange: Double,
                         baselineDate: Timestamp,
                         baselineValue: Double,
                         valueList: Seq[Double]): C3ChartHistory = {

    val chartId = C3Chart.idTagPrefix + Util.textToId(chartTitle)

    val width = None
    val height = None
    val xLabel = "Date"
    val xDateList = dateList
    val yDataLabel = chartTitle + " %"
    val yAxisLabels = Seq(chartTitle + " %")
    val yValues = Seq(valueList)
    val yFormat = ".4g"
    val yColorList = Util.colorPallette(new Color(0x4477BB), new Color(0x44AAFF), yValues.size)
    val baseline = new Baseline(None, maintenanceRecordPK = -1, baselineDate, SOPInstanceUID = None, chartTitle, baselineValue.toString, setup = "")
    val tolerance = new C3Chart.Tolerance(baselineValue - toleranceRange, baselineValue + toleranceRange)

    Trace.trace("tolerance: " + tolerance)
    val chart = new C3ChartHistory(
      Some(chartId),
      allMaintenanceRecords,
      width,
      height,
      xLabel, xDateList,
      baseline = Some(baseline),
      Some(tolerance),
      yRange = None,
      yAxisLabels, yDataLabel, yValues, yIndex, yFormat, yColorList
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
        baselineDate = sfAndBaseline.baselineTimestamp,
        sfAndBaseline.symmetryAndFlatness.axialSymmetry,
        valueList)
    }

    val chartTransverse = {
      val valueList = history.map(h => h.symmetryAndFlatness.transverseSymmetry)
      makeChart(
        transverseSymmetryName,
        toleranceRange = Config.SymmetryPercentLimit,
        baselineDate = sfAndBaseline.baselineTimestamp,
        sfAndBaseline.symmetryAndFlatness.transverseSymmetry,
        valueList)
    }

    val chartFlatness = {
      val valueList = history.map(h => h.symmetryAndFlatness.flatness)
      makeChart(
        flatnessName,
        toleranceRange = Config.FlatnessPercentLimit,
        baselineDate = sfAndBaseline.baselineTimestamp,
        sfAndBaseline.symmetryAndFlatness.flatness,
        valueList)
    }

    val chartProfileConstancy = {
      val valueList = history.map(h => h.symmetryAndFlatness.profileConstancy(h.baseline))
      makeChart(
        profileConstancyName,
        toleranceRange = Config.ProfileConstancyPercentLimit,
        baselineDate = sfAndBaseline.baselineTimestamp,
        sfAndBaseline.symmetryAndFlatness.profileConstancy(sfAndBaseline.symmetryAndFlatness),
        valueList)
    }

    chartAxial.javascript + chartTransverse.javascript + chartFlatness.javascript + chartProfileConstancy.javascript
  }

}

































































