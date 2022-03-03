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
import edu.umro.ScalaUtil.Trace
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

  Trace.trace(procedureStatus)
  private def beamNameOf(leafSet: LeafSet): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, leafSet.attributeList).get

  private def rtplanName = DicomHtml(extendedData).makeDicomContent(runReq.rtplan, "RTPLAN")

  /**
    * Make an HTML reference to the RTPLAN so it can be viewed.
    * @return Link to rtplan.  Also show the patient ID and patient name.
    */

  private def generalReference(): Elem = {

    val planReference = <a href={rtplanName}>View RTPLAN</a>

    val patientId = <span>Patient ID <span aqaalias="">{Util.patientIdOfAl(leafSetSeq.head.attributeList)}</span> </span>

    val patientName = <span>Patient Name <span aqaalias="">{leafSetSeq.head.attributeList.get(TagByName.PatientName).getSingleStringValueOrEmptyString()}</span> </span>

    val sp = (0 to 6) map (_ => WebUtil.nbsp)

    val ref = <span> {planReference} {sp} {patientId} {sp} {patientName} </span>

    ref
  }

  private def content: Elem = {
    <div class="row">
        <div class="col-md-8 col-md-offset-2">
        {generalReference()}
          {leafSetSeq.sortBy(beamNameOf).map(leafSet => LeafSetHtml(extendedData, leafSet, runReq).leafSetToHtml)}
        </div>
      </div>
  }

  def makeDisplay(): Unit = {
    val contentWithHeader = ExtendedData.wrapExtendedData(extendedData, content)
    val text = WebUtil.wrapBody(contentWithHeader, "Leaf Gap and Skew", refresh = None, c3 = true)
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
