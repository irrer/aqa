package org.aqa.webrun.wl

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.WinstonLutz
import org.aqa.web.C3ChartHistory

import java.util.Date

class WLChart(outputPK: Long) extends Logging {
  private val output: Output = Output.get(outputPK).get
  private val machine: Machine = Machine.get(output.machinePK.get).get
  // make a list of all the distinct dates in the data set
  // private val allDates = history.map(wlh => wlh.output.dataDate.get).groupBy(_.getTime).values.map(_.head).toSeq.sortBy(_.getTime)

  // all history for this machine, grouped by beam name, with each beam sorted temporally
  private val history = {
    val h = WinstonLutz.historyByMachine(machine.machinePK.get)
    h.groupBy(_.winstonLutz.beamName).values.toSeq.sortBy(_.head.winstonLutz.beamName).map(beam => beam.sortBy(_.output.dataDate.get.getTime))
  }

  private val allDates = history.flatten.map(wlh => wlh.output.dataDate.get.asInstanceOf[Date]).sortBy(_.getTime)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else
      MaintenanceRecord.getRange(machine.machinePK.get, allDates.head, allDates.last).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
  }

  private val centerChartId = "Center"

  private val xDateList = history.map(beam => beam.map(_.output.dataDate.get.asInstanceOf[Date]))
  private val yIndex = history.head.indexWhere(beam => beam.output.outputPK.get == output.outputPK.get)
  private val yAxisLabels = {
    history.map(beam => if (beam.head.winstonLutz.beamName.isDefined) beam.head.winstonLutz.beamName.get else "NA")
  }

  new C3ChartHistory(
    chartIdOpt = Some(centerChartId),
    maintenanceList = maintenanceRecordList,
    width = None, // width
    height = None, // height
    xLabel = "Date",
    xDateList = xDateList,
    baseline = None, // BaselineSpec
    tolerance = None,
    yRange = None,
    yAxisLabels = yAxisLabels,
    yDataLabel = "mm",
    history.map(beam => beam.map(_.winstonLutz.errorXY)),
    yIndex = yIndex,
    yFormat = ".3r",
    yColorList = Seq(),
    setBaselineList = Seq()
  )

}
