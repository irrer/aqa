package org.aqa.webrun.bbByEpid

import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyEPID
import org.aqa.db.BBbyEPID.BBbyEPIDHistory
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
  * Make a history chart for BBbyEPID.
  */
class BBbyEPIDChartPartial(outputPK: Long) extends Logging {

  private val output = Output.get(outputPK).get
  private val procedure = Procedure.get(output.procedurePK).get
  private val machine = Machine.get(output.machinePK.get).get

  private class ToBeCharted(epidList: Seq[BBbyEPID.BBbyEPIDHistory]) {
    val date: Date = epidList.head.date
    val outputPK: Long = epidList.head.bbByEPID.outputPK
    // to balance the number of vertical and horizontal readings, use the same number of each
    val matchingSize: Int = Math.min(epidList.count(h => h.bbByEPID.isHorz), epidList.count(h => h.bbByEPID.isVert))

    val horzList: Seq[BBbyEPIDHistory] = epidList.filter(h => h.bbByEPID.isHorz).take(matchingSize)
    val vertList: Seq[BBbyEPIDHistory] = epidList.filter(h => h.bbByEPID.isVert).take(matchingSize)

    val epid3DXVert_mm: Double = vertList.map(_.bbByEPID.epid3DX_mm).sum / matchingSize
    val epid3DYHorz_mm: Double = horzList.map(_.bbByEPID.epid3DY_mm).sum / matchingSize

    val epid3dZVert_mm: Double = vertList.map(_.bbByEPID.epid3DZ_mm).sum / matchingSize
    val epid3dZHorz_mm: Double = horzList.map(_.bbByEPID.epid3DZ_mm).sum / matchingSize

    val epid3dSumVert_mm: Double = Math.sqrt((epid3DXVert_mm * epid3DXVert_mm) + (epid3dZVert_mm * epid3dZVert_mm))
    val epid3dSumHorz_mm: Double = Math.sqrt((epid3DYHorz_mm * epid3DYHorz_mm) + (epid3dZHorz_mm * epid3dZHorz_mm))
  }

  private val history = {
    val all = BBbyEPID.history(machine.machinePK.get, procedure.procedurePK.get)
    def hasBothVH(g: Seq[BBbyEPIDHistory]) = g.exists(e => e.bbByEPID.isHorz) && g.exists(e => e.bbByEPID.isVert)

    val qualified = all.groupBy(_.bbByEPID.outputPK).values.filter(g => hasBothVH(g)).map(g => new ToBeCharted(g))
    qualified.toSeq.sortBy(_.date.getTime)
  }

  private val allDates = history.map(cd => cd.date)

  /** All maintenance records for the entire history interval for all beams except for 'Set Baseline' to reduce clutter. */
  private val maintenanceRecordList = {
    if (history.isEmpty)
      Seq[MaintenanceRecord]()
    else
      MaintenanceRecord.getRange(machine.machinePK.get, allDates.min, allDates.max).filter(m => !m.category.equalsIgnoreCase(MaintenanceCategory.setBaseline))
  }

  private def chartId = C3Chart.idTagPrefix + Util.textToId(machine.id) + "_Partial"

  def chartReference: Elem = {
    // val ciob = chartId
    C3ChartHistory.htmlRef(chartId)
    // <div id={ciob}></div>
  }

  private def chartOf(index: Int): C3ChartHistory = {
    val units = "mm"
    val dataToBeGraphed = Seq(
      history.map(h => h.epid3DXVert_mm),
      history.map(h => h.epid3dZVert_mm),
      history.map(h => h.epid3dSumVert_mm),
      history.map(h => h.epid3DYHorz_mm),
      history.map(h => h.epid3dZHorz_mm),
      history.map(h => h.epid3dSumHorz_mm)
    )

    val colorList = Seq(new Color(160, 160, 160), new Color(80, 80, 80), new Color(20, 20, 20), new Color(204, 255, 51), new Color(61, 245, 0), new Color(46, 184, 0))

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date",
      history.map(h => h.date),
      None, // BaselineSpec
      Some(new C3Chart.Tolerance(-Config.BBbyEPIDChartTolerance_mm, Config.BBbyEPIDChartTolerance_mm)),
      Some(new C3Chart.YRange(-Config.BBbyEPIDChartYRange_mm, Config.BBbyEPIDChartYRange_mm)),
      Seq("LEFT/RIGHT 0/180", "SUP/INF 0/180", "Vect Len 0/180", "POST/ANT 90/270", "SUP/INF 90/270", "Vect Len 90/270"),
      units,
      dataToBeGraphed,
      index,
      ".3r",
      colorList
    )
  }

  private val chart = {
    val index = history.indexWhere(sh => sh.outputPK == outputPK)
    if (index == -1)
      None
    else
      Some(chartOf(index))
  }

  val chartScript: String = {
    if (chart.isDefined) chart.get.javascript
    else ""
  }

}
