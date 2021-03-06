/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.gapSkew

import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.io.File
import scala.xml.Elem

object GapSkewHtml {}

class GapSkewHtml(extendedData: ExtendedData, runReq: GapSkewRunReq, leafSetSeq: Seq[LeafSet], procedureStatus: ProcedureStatus.Value) {

  private def beamNameOf(leafSet: LeafSet): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, leafSet.attributeList).get

  private val dicomHtml = DicomHtml(extendedData, "RTPLAN")

  private def rtplanUrl = dicomHtml.htmlUrl

  /**
    * Make an HTML reference to the RTPLAN so it can be viewed.
    * @return Link to rtplan.  Also show the patient ID and patient name.
    */

  private def generalReference(): Elem = {

    val style = "vertical-align:middle; padding:20px;"

    val status = {
      val titleSuffix = WebUtil.titleNewline + "Warning limit: " + Config.GapSkewAngleWarn_deg + " degrees.   Fail limit: " + Config.GapSkewAngleFail_deg + " degrees."
      def toElem(title: String, text: String, color: String): Elem = {
        <td style={style}>
          <center>
            <h2 style={s"background-color:$color; border:solid $color 1px; border-radius: 8px; padding: 12px;"} title={title + titleSuffix}> {text} </h2>
          </center>
        </td>
      }

      val s = procedureStatus match {
        case ProcedureStatus.fail =>
          toElem("At least one of the angles was off by more than the fail limit.", "Failed", GapSkewUtil.colorFail)
        case ProcedureStatus.warning =>
          toElem("At least one of the angles was off by more than the warning limit.", "Warning", GapSkewUtil.colorWarn)
        case _ =>
          toElem("All angles were less than the warning limit.", "Passed", GapSkewUtil.colorPass)
      }
      s
    }

    val patientNameText = leafSetSeq.head.attributeList.get(TagByName.PatientName).getSingleStringValueOrEmptyString()

    val rtplan =
      <td style={style}>
        <center>
          <a href={rtplanUrl}>View RTPLAN</a>
        </center>
      </td>

    val latestGapSkew =
      <td style={style}>
        <center>
          <a href={GapSkewLatestHtml.path}> Latest Gap Skew </a>
        </center>
      </td>

    val patientId =
      <td style={style}>
        Patient ID
        <br/>
        <b aqaalias="">{Util.patientIdOfAl(leafSetSeq.head.attributeList)}</b>
      </td>

    val patientName =
      <td style={style}>
        Patient Name
        <br/>
        <b aqaalias="">{patientNameText}</b>
      </td>

    val ref = {
      <div class="row">
        <div class="col-md-12">
          <table style="padding:20px;">
            <tr>
              {status}
              {rtplan}
              {patientId}
              {patientName}
              {latestGapSkew}
            </tr>
          </table>
        </div>
      </div>
    }

    ref
  }

  val leafSetHtmlList: Seq[GapSkewDetailHtml] = leafSetSeq.sortBy(beamNameOf).map(leafSet => GapSkewDetailHtml(extendedData, leafSet, runReq))

  private def content: Elem = {
    <div class="row" style="margin-top:10px;">
        <div class="col-md-8 col-md-offset-2" style="border:solid #bbbbbb 1px; padding: 12px; margin-bottom:500px;">
          {generalReference()}
          {leafSetHtmlList.map(l => l.summaryHtml())}
        </div>
      </div>
  }

  def makeDisplay(): Unit = {
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "Leaf Gap and Skew", refresh = None)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)

    leafSetHtmlList.foreach(l => l.writeDetailedHtml())
    dicomHtml.makeDicomContent(runReq.rtplan)

    if (true) {
      val img = new DicomImage(runReq.rtimageMap.head._2)
      val text = img.pixelsToText
      val file = new File("""D:\tmp\pix.txt""")
      Util.writeFile(file, text)
      println("wrote to file " + file.getAbsolutePath)
    }
  }

}
