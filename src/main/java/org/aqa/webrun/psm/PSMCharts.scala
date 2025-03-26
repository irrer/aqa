package org.aqa.webrun.psm

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.PSMBeam
import org.aqa.web.C3ChartHistory

import java.awt.Color

class PSMCharts(outputPK: Long) extends Logging {

  /**
    * Make a history chart for BBbyCBCT.
    */

  private val output: Output = Output.get(outputPK).get
  // private val procedure: Procedure = Procedure.get(output.procedurePK).get
  // private val input: Input = Input.get(output.inputPK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get

  private val history = PSMBeam.historyByMachine(machine.machinePK.get)

  private val index = history.indexWhere(h => h.output.outputPK.get == outputPK)

  private val beamNameList = {

    val numNot = "[^0-9]"
    val numReg = ".*[0-9].*"

    def sorter(a: String, b: String): Boolean = {
      if (a.matches(numReg) && b.matches(numReg)) {
        a.replaceAll(numNot, "").toInt <= b.replaceAll(numNot, "").toInt
      } else
        a.toLowerCase().compareTo(b.toLowerCase()) <= 0
    }

    history.flatMap(h => h.psmBeamList.map(_.beamName)).distinct.sortWith(sorter)
  }

  private val allDates = history.map(h => h.output.dataDate.get).sortBy(_.getTime)

  private def getYValues(f: PSMBeam => Double) = {
    def getBeam(beamName: String) = history.flatMap(h => h.psmBeamList.find(_.beamName.equals(beamName))).map(f)
    beamNameList.map(getBeam)
  }

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else {
      MaintenanceRecord.getRange(machine.machinePK.get, allDates.minBy(_.getTime), allDates.maxBy(_.getTime)).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
    }
  }

  val meanChart = new C3ChartHistory(
    chartIdOpt = Some("Mean"),
    maintenanceList = maintenanceRecordList,
    width = None, // width
    height = None, // height
    xLabel = "Date",
    xDateList = Seq(allDates),
    baseline = None, // BaselineSpec
    tolerance = None, // tolerance Some(new C3Chart.Tolerance(-Config.VMATDeviationThreshold_pct, Config.VMATDeviationThreshold_pct)),
    yRange = None, // range
    yAxisLabels = beamNameList,
    yDataLabel = "Mean CU",
    yValues = getYValues((psmBeam: PSMBeam) => psmBeam.mean_cu),
    yIndex = index,
    yFormat = ".3r",
    yColorList = Seq[Color](),
    setBaselineList = Seq()
  )

  val stdDevChart = new C3ChartHistory(
    chartIdOpt = Some("StdDev"),
    maintenanceList = maintenanceRecordList,
    width = None, // width
    height = None, // height
    xLabel = "Date",
    xDateList = Seq(allDates),
    baseline = None, // BaselineSpec
    tolerance = None, // tolerance Some(new C3Chart.Tolerance(-Config.VMATDeviationThreshold_pct, Config.VMATDeviationThreshold_pct)),
    yRange = None, // range
    yAxisLabels = beamNameList,
    yDataLabel = "Standard Deviation CU",
    yValues = getYValues((psmBeam: PSMBeam) => psmBeam.stdDev_cu),
    yIndex = index,
    yFormat = ".3r",
    yColorList = Seq[Color](),
    setBaselineList = Seq()
  )

}
