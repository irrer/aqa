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

import com.pixelmed.dicom.AttributeList
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

import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

object GapSkewHtml {}

class GapSkewHtml(extendedData: ExtendedData, runReq: GapSkewRunReq, leafSetSeq: Seq[LeafSet], procedureStatus: ProcedureStatus.Value) {

  private def beamNameOf(leafSet: LeafSet): String = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, leafSet.attributeList).get

  private val rtplanHtml = DicomHtml(extendedData, "RTPLAN")

  private def rtplanUrl = rtplanHtml.htmlUrl

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
        case ProcedureStatus.abort =>
          toElem("At least one of the images could not be processed", "Aborted", GapSkewUtil.colorAbort)
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

  /**
    * The analysis of the beam was aborted because it is not able to be analyzed.
    * @param error Description of problem.
    * @param rtimage DICOM of RTIMAGE.
    * @param bufferedImage Image to show user.
    * @return HTML to put in the output's main page.
    */
  def abortHtml(error: String, rtimage: AttributeList, bufferedImage: BufferedImage): Elem = {
    val beamName = Phase2Util.getBeamNameOfRtimage(runReq.rtplan, rtimage).get

    val leafTitle: Elem = {
      val color = GapSkewUtil.colorAbort
      val collimatorAngle = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))
      val style = s"margin:8px; background-color:$color; border:solid $color 1px; border-radius: 8px; padding: 12px;"
      val title = "Collimator angle: " + collimatorAngle
      val heading = <h3 style={style} title={title}> {beamName} </h3>

      heading
    }

    val imageUrl: String = FileUtil.replaceInvalidFileNameCharacters(beamName, '_').replace(' ', '_') + ".png"

    val pngFile = new File(extendedData.output.dir, imageUrl)
    Util.writePng(bufferedImage, pngFile)
    val dicomHtml = DicomHtml(extendedData: ExtendedData, beamName)
    dicomHtml.makeDicomContent(rtimage, Some(pngFile.getName))

    <div class="row" style="margin-top: 40px;">
      <div class="col-md-3">
          <center style="padding: 24px;">
            {leafTitle}<p></p>
          </center>
      </div>
      <div class="col-md-5">
        {error}
      </div>
      <div class="col-md-2">
        <a href={dicomHtml.htmlUrl} title="Click for full sized image and metadata." class="screenshot" rel={imageUrl}>
          <img class="img-responsive fit-image" src={imageUrl} style="width:384px;"/>
        </a>
      </div>
    </div>
  }

  val leafSetHtmlList: Seq[Either[Elem, GapSkewDetailHtml]] = leafSetSeq
    .sortBy(beamNameOf)
    .map(leafSet => {
      if (leafSet.gapSkew.isLeft)
        Left(abortHtml(leafSet.gapSkew.left.get, leafSet.attributeList, leafSet.image))
      else
        Right(GapSkewDetailHtml(extendedData, leafSet.gapSkew.right.get, leafSet.attributeList, leafSet.image))
    })

  private def content: Elem = {

    val list = leafSetHtmlList.map(l => {
      if (l.isLeft)
        l.left.get
      else
        l.right.get.summaryHtml()
    })

    <div class="row" style="margin-top:10px;">
      <div class="col-md-8 col-md-offset-2" style="border:solid #bbbbbb 1px; padding: 12px; margin-bottom:500px;">
        {generalReference()}
        {list}
      </div>
    </div>
  }

  def makeDisplay(): Unit = {
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), "Leaf Gap and Skew", refresh = None)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)

    leafSetHtmlList.filter(_.isRight).foreach(l => l.right.get.writeDetailedHtml())
    rtplanHtml.makeDicomContent(runReq.rtplan)

    if (false) {
      val img = new DicomImage(runReq.rtimageMap.head._2)
      val text = img.pixelsToText
      val file = new File("""D:\tmp\pix.txt""")
      Util.writeFile(file, text)
      println("wrote to file " + file.getAbsolutePath)
    }
  }

}
