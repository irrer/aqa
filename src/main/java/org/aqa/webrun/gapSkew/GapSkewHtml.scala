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
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.io.File
import scala.xml.Elem

object GapSkewHtml {

  def imageFileOf(leafSet: LeafSet, outputDir: File): File = {
    val pngFileName = FileUtil.replaceInvalidFileNameCharacters(leafSet.beamName, '_').replace(' ', '_') + ".png"
    new File(outputDir, pngFileName)
  }

}

class GapSkewHtml(extendedData: ExtendedData, runReq: GapSkewRunReq, leafSetSeq: Seq[LeafSet], procedureStatus: ProcedureStatus.Value) {

  private def beamNameOf(leafSet: LeafSet): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, leafSet.attributeList).get

  private def rtplanName = DicomHtml(extendedData).makeDicomContent(runReq.rtplan, "RTPLAN")

  /**
    * Make an HTML reference to the RTPLAN so it can be viewed.
    * @return Link to rtplan.  Also show the patient ID and patient name.
    */

  private def generalReference(): Elem = {

    val status = {
      val titleSuffix = WebUtil.titleNewline + "Warning limit: " + Config.GapSkewAngleWarn_deg + " degrees.   Fail limit: " + Config.GapSkewAngleFail_deg + " degrees."
      def toElem(title: String, text: String, color: String): Elem = {
        <div class="col-md-2">
          <center>
            <h2 style={s"background-color:$color; border:solid $color 1px; border-radius: 18px; padding: 12px;"} title={title + titleSuffix}> {text} </h2>
          </center>
        </div>
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

    val planReference = <div class="col-md-2"><center><br/><a href={rtplanName}>View RTPLAN</a></center></div>

    val patientId = <div class="col-md-2"><center>Patient ID <br/><b aqaalias="">{Util.patientIdOfAl(leafSetSeq.head.attributeList)}</b></center></div>

    val patientName = <div class="col-md-2"><center>Patient Name <br/><b aqaalias="">{leafSetSeq.head.attributeList.get(TagByName.PatientName).getSingleStringValueOrEmptyString()}</b> </center></div>

    val ref = <div class="row" style="margin-top:20px;">{status} {planReference} {patientId} {patientName} </div>

    ref
  }

  val leafSetHtmlList: Seq[LeafSetHtml] = leafSetSeq.sortBy(beamNameOf).map(leafSet => LeafSetHtml(extendedData, leafSet, runReq))

  private def content: Elem = {
    <div class="row">
        <div class="col-md-8 col-md-offset-2">
          {generalReference()}
          {leafSetHtmlList.map(_.leafSetToHtml)}
        </div>
      </div>
  }

  def makeDisplay(): Unit = {
    val contentWithHeader = ExtendedData.wrapExtendedData(extendedData, content)
    val js = s"""<script>${leafSetHtmlList.map(_.charts.js).mkString("\n")}</script>"""
    val text = WebUtil.wrapBody(contentWithHeader, "Leaf Gap and Skew", refresh = None, c3 = true, runScript = Some(js))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
    if (true) {
      val img = new DicomImage(runReq.rtimageMap.head._2)
      val text = img.pixelsToText
      val file = new File("""D:\tmp\pix.txt""")
      Util.writeFile(file, text)
      println("wrote to file " + file.getAbsolutePath)
    }
  }

}
