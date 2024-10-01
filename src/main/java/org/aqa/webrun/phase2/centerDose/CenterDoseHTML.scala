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

import org.aqa.Util
import org.aqa.db.CenterDose
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.Logging
import org.aqa.web.C3ChartHistory
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.io.File
import scala.xml.Elem

object CenterDoseHTML extends Logging {
  private val htmlFileName = "CenterDose.html"

  /**
    * Generate a detailed report and write it to the output directory.  Return an
    * HTML snippet that serves as a summary and a link to the detailed report.
    */
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[CenterDose], status: ProcedureStatus.Value): Elem = {

    val planHref = Phase2Util.dicomViewHref(runReq.rtplan, extendedData, runReq)
    val viewRtPlan = {
      <a title="View RT Plan DICOM file" href={planHref}>RT Plan</a>
    }

    val chart = new CenterDoseChart(extendedData.output.outputPK.get)

    val units: String = resultList.head.units

    def imageToHtml(centerDose: CenterDose): Elem = {
      <div class="row">
        <h3>{centerDose.beamName + " : "}<font color='orange'>{centerDose.dose.formatted("%6.4f")}</font> {units}</h3>
        {chart.chartReferenceToBeam(centerDose.beamName)}
      </div>
    }

    // val theBody = resultList.map(psnChk => centerDoseToTableRow(psnChk))

    val content = {
      <div>
        <div class="row" style="margin:50px;">
          <div class="col-md-2">{C3ChartHistory.htmlHelp()}</div>
          <div class="col-md-2">{viewRtPlan}</div>
        </div>
        <div class="row" style="margin:50px;">
          <div class="row" style="margin:50px;">
            <div class="col-md-12">
              {resultList.map(cd => imageToHtml(cd))}
            </div>
          </div>
        </div>
      </div>
    }

    // write the report to the output directory
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "Center Dose", status, Some(CenterDoseChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)), runReq.rtimageMap)
    val file = new File(extendedData.output.dir, htmlFileName)
    Util.writeBinaryFile(file, text.getBytes)

    /**
      * Make a tiny summary and link to the detailed report.
      */
    def makeSummary = {
      val elem = {
        <div title="Click for details.">
          <a href={htmlFileName}>
            Center Dose<br/>
            <img src={Config.passImageUrl} height="32"/>
          </a>
        </div>
      }
      elem
    }
    makeSummary
  }
}
