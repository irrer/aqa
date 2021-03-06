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

package org.aqa.webrun.phase2.centerDose

import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.Institution
import org.aqa.db.Input
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.Util

import java.util.Date
import org.aqa.web.WebServer
import org.aqa.db.CenterDose
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil._
import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.run.ProcedureStatus

import java.io.File
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage

import java.awt.geom.Point2D
import com.pixelmed.dicom.AttributeList

import java.awt.Point
import com.pixelmed.dicom.TagFromName
import org.aqa.Logging
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.web.C3ChartHistory

object CenterDoseHTML extends Logging {
  private val htmlFileName = "CenterDose.html"

  /**
   * Generate a detailed report and write it to the output directory.  Return an
   * HTML snippet that serves as a summary and a link to the detailed report.
   */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[CenterDose], status: ProcedureStatus.Value): Elem = {

    val planHref = Phase2Util.dicomViewHref(runReq.rtplan, extendedData, runReq)
    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={ planHref }>RT Plan</a>
    }

    /**
     * Get the history of results from previous runs of this procedure.  Exclude results that were run on this exact data.
     */
    //    val history = {
    //      val uidSet = resultList.map(cd => cd.SOPInstanceUID).toSet
    //      val j = CenterDose.recentHistory(Config.CenterDoseReportedHistoryLimit, extendedData.machine.machinePK.get, extendedData.procedure.procedurePK.get, extendedData.output.dataDate).toArray
    //      val j1 = j.size
    //      val hist = CenterDose.recentHistory(Config.CenterDoseReportedHistoryLimit, extendedData.machine.machinePK.get, extendedData.procedure.procedurePK.get, extendedData.output.dataDate).
    //        filter(cd => !uidSet.contains(cd.SOPInstanceUID))
    //      hist
    //    }
    //
    //    val resultListAsHistory = resultList.map(cd => new CenterDose.CenterDoseHistory(extendedData.output.dataDate.get, cd.beamName, cd.dose, cd.SOPInstanceUID))
    //    val units = runReq.flood.attributeList.get.get(TagFromName.RescaleType).getSingleStringValueOrDefault("CU")

    val chart = new CenterDoseChart(extendedData.output.outputPK.get)

    class Column(val title: String, columnName: String, val get: (CenterDose) => String) {
      def toHeader = <th title={ title }>{ columnName }</th>
      def toRow(cntrDose: CenterDose) = <td title={ title }>{ get(cntrDose) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class ColumnBeamName(override val title: String, columnName: String, override val get: (CenterDose) => String) extends Column(title, columnName, get) {
      override def toRow(centerDose: CenterDose) = {
        val dicomFile = if (centerDose.beamName.equals(Config.FloodFieldBeamName)) runReq.flood else runReq.rtimageMap(centerDose.beamName)
        val link = Phase2Util.dicomViewHref(dicomFile, extendedData, runReq)
        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(centerDose) }</a></td> }
        elem
      }
    }

    class ColumnChart(override val title: String, columnName: String, override val get: (CenterDose) => String) extends Column(title, columnName, get) {
      override def toHeader = <th title={ title }>History</th>
      override def toRow(cntrDose: CenterDose) = <td title={ title } id={ chart.chartIdOfBeam(cntrDose.beamName) }/>
    }

    val rowList = Seq(
      new ColumnBeamName("Name of beam in plan", "Beam Name", (centerDose: CenterDose) => centerDose.beamName),
      new Column("Dose", "Dose", (centerDose: CenterDose) => centerDose.dose.formatted("%f")),
      new ColumnChart("History of recent values for this machine and beam", "History", (centerDose: CenterDose) => centerDose.dose.formatted("%f")))

    def centerDoseTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def centerDoseToTableRow(centerDose: CenterDose): Elem = {
      <tr>{ rowList.map(row => row.toRow(centerDose)) }</tr>
    }

    val units: String = resultList.head.units

    def imageToHtml(centerDose: CenterDose): Elem = {
      <div class="row">
        <h3>{ centerDose.beamName + " : " }<font color='orange'>{ centerDose.dose.formatted("%6.4f") }</font> { units }</h3>
        { chart.chartReferenceToBeam(centerDose.beamName) }
      </div>
    }

    val tbody = resultList.map(psnChk => centerDoseToTableRow(psnChk))

    val content = {
      <div>
        <div class="row" style="margin:50px;">
          <div class="col-md-2">{ C3ChartHistory.htmlHelp() }</div>
          <div class="col-md-2">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:50px;">
          <div class="row" style="margin:50px;">
            <div class="col-md-12">
              { resultList.map(cd => imageToHtml(cd)) }
            </div>
          </div>
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Center Dose", status, Some(CenterDoseChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)), runReq)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    /**
     * Make a tiny summary and link to the detailed report.
     */
    def makeSummary = {
      val elem = {
        <div title="Click for details.">
          <a href={ htmlFileName }>
            Center Dose<br/>
            <img src={ Config.passImageUrl } height="32"/>
          </a>
        </div>
      }
      elem
    }
    makeSummary
  }
}
