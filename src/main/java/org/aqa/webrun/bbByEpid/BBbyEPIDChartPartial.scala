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

package org.aqa.webrun.bbByEpid

import edu.umro.ImageUtil.ImageUtil
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
  * Make a history chart for BBbyEPID without CBCT (that is why it is called partial).
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

  private val all: Seq[BBbyEPIDHistory] = BBbyEPID.history(machine.machinePK.get, procedure.procedurePK.get)

  private val history = {
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

  private def chartIdPartial: String = C3Chart.idTagPrefix + Util.textToId(machine.id) + "_Partial"

  def chartReference: Elem = C3ChartHistory.htmlRef(chartIdPartial)

  // ----------------------------------------------------------------------------------------

  private val index: Int = history.indexWhere(sh => sh.outputPK == outputPK)

  private def chartOf(): C3ChartHistory = {
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
      Some(chartIdPartial),
      maintenanceRecordList,
      None, // width
      None, // height
      "Date",
      Seq(history.map(_.date)),
      None, // BaselineSpec
      Some(new C3Chart.Tolerance(-Config.BBbyEPIDChartTolerance_mm, Config.BBbyEPIDChartTolerance_mm)),
      Some(new C3Chart.YRange(-Config.BBbyEPIDChartYRange_mm, Config.BBbyEPIDChartYRange_mm)),
      //noinspection SpellCheckingInspection
      Seq("LEFT/RIGHT 0/180", "SUP/INF 0/180", "Vect Len 0/180", "POST/ANT 90/270", "SUP/INF 90/270", "Vect Len 90/270"),
      units,
      dataToBeGraphed,
      index,
      ".3r",
      colorList
    )
  }

  // ----------------------------------------------------------------------------------------

  private def chartOfStats(chartId: String, getValue: BBbyEPIDHistory => Double, yDataLabel: String): C3ChartHistory = {

    val allRelevant = all.filter(h => h.bbByEPID.pixelStandardDeviation_cu != -1)

    val plannedFieldList = allRelevant.filterNot(_.bbByEPID.isOpenFieldImage)
    val openFieldList = allRelevant.filter(_.bbByEPID.isOpenFieldImage)

    val dataToBeGraphed = Seq(plannedFieldList.map(getValue), openFieldList.map(getValue))

    val yRange = {
      val stdDevMultiple = 2.0
      val valueList = dataToBeGraphed.flatten
      val mean = valueList.sum / valueList.size
      val stdDev = ImageUtil.stdDev(valueList.map(_.toFloat))
      val range = ImageUtil.stdDev(valueList.map(_.toFloat)) * stdDevMultiple
      Some(new C3Chart.YRange(mean - range, mean + range))
    }

    val xDateList = { Seq(plannedFieldList.map(h => h.date), openFieldList.map(h => h.date)) }

    new C3ChartHistory(
      Some(chartId),
      maintenanceRecordList,
      width = None, // width
      height = None, // height
      xLabel = "Date",
      xDateList = xDateList,
      baseline = None,
      tolerance = None,
      yRange = yRange,
      Seq("Planned Field", "Open Field"),
      yDataLabel = yDataLabel,
      dataToBeGraphed,
      index,
      ".3r",
      yColorList = Seq(new Color(46, 140, 0), new Color(189, 31, 0), Color.gray)
    )
  }

  // ----------------------------------------------------------------------------------------

  private def chartIdPixelCoefficientOfVariation = C3Chart.idTagPrefix + Util.textToId(machine.id) + "_PixelCoefficientOfVariation"
  def chartReferencePixelCoefficientOfVariation: Elem = C3ChartHistory.htmlRef(chartIdPixelCoefficientOfVariation)

  private def chartOfPixelCoefficientOfVariation(): C3ChartHistory =
    chartOfStats(chartIdPixelCoefficientOfVariation, (h: BBbyEPIDHistory) => h.bbByEPID.pixelCoefficientOfVariation, "Image Coefficient of Variation")

  // ----------------------------------------------------------------------------------------

  private def chartIdBBStdDevMultiple = C3Chart.idTagPrefix + Util.textToId(machine.id) + "_BBStdDevMultiple"
  def chartReferenceBBStdDevMultiple: Elem = C3ChartHistory.htmlRef(chartIdBBStdDevMultiple)

  private def chartOfBBStdDevMultiple(): C3ChartHistory = chartOfStats(chartIdBBStdDevMultiple, (h: BBbyEPIDHistory) => h.bbByEPID.bbStdDevMultiple, "Multiple of Std. Dev.")

  // ----------------------------------------------------------------------------------------

  private def chartIdPixelMean = C3Chart.idTagPrefix + Util.textToId(machine.id) + "_PixelMean"
  def chartReferencePixelMean: Elem = C3ChartHistory.htmlRef(chartIdPixelMean)

  private def chartOfPixelMean(): C3ChartHistory = chartOfStats(chartIdPixelMean, (h: BBbyEPIDHistory) => h.bbByEPID.pixelMean_cu, "CU")

  // ----------------------------------------------------------------------------------------

  val chartScript: String = {
    if (index == -1)
      ""
    else {
      val chartList = Seq(chartOf(), chartOfPixelCoefficientOfVariation(), chartOfBBStdDevMultiple(), chartOfPixelMean())
      val js = chartList.map(c => c.javascript).mkString("\n\n")
      js
    }
  }

}
