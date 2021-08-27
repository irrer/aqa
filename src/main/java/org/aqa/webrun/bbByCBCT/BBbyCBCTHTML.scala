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

package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BBbyCBCT
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.web.C3ChartHistory
import org.aqa.web.MachineUpdate
import org.aqa.web.OutputList
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.restlet.Response

import java.awt.Color
import java.io.File
import java.text.SimpleDateFormat
import scala.annotation.tailrec
import scala.xml.Elem

object BBbyCBCTHTML extends Logging {

  private val axisNameList = Seq("X Axis : side", "Y Axis : top", "Z Axis : longitudinal")
  val matlabFileName = "matlab.txt"

  private def fileNameOfFull(index: Int) = Util.textToId(axisNameList(index)) + "axisViewFull.png"

  private def fileNameOfAreaOfInterest(index: Int) = Util.textToId(axisNameList(index)) + "axisViewAOI.png"

  private val cbctDirName = "cbct"
  private val mainReportFileName = Output.displayFilePrefix + ".html"
  private val cbctMainFileName = "cbctViewer.html"
  private val thumbnailsPerRow = 10

  private def getZ(al: AttributeList): Double = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  def wrap(content: Elem, extendedData: ExtendedData): Elem = {
    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")

    def wrapElement(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span aqaalias="">
            {value}
          </span>
        } else {
          val valueList = value.split("\n");
          {
            <span>
              {valueList.head}{
              valueList.tail.map(line => {
                <span>
                <br/>{line}
              </span>
              })
            }
            </span>
          }
        }

      {
        <div class={"col-md-" + col}>
          <em>
            {name}
            :</em> <br/>{html}
        </div>
      }

    }

    val dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }

    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val showMachine = {
      <div class="col-md-1">
        <h2 title="Treatment machine.  Click for details.">
          {MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id)}
        </h2>
      </div>
    }

    val elem = {

      val header = Seq(
        showMachine,
        wrapElement(2, "Institution", extendedData.institution.name, asAlias = true),
        wrapElement(1, "Data Acquisition", dataAcquisitionDate, asAlias = false),
        wrapElement(1, "Analysis Started", twoLineDate.format(extendedData.output.startDate), asAlias = false),
        wrapElement(1, "User", extendedData.user.id, asAlias = true),
        wrapElement(1, "Elapsed", elapsed, asAlias = false),
        wrapElement(1, "Procedure", procedureDesc, asAlias = false)
      )

      <div class="row">
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {header}<div class="col-md-1">
            {OutputList.redoUrl(extendedData.output.outputPK.get)}
          </div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {content}
          </div>
        </div>
      </div>
    }

    elem
  }

  /**
    * Create a web page for viewing and downloading the CBCT files.
    */
  def makeCbctSlices(extendedData: ExtendedData, runReq: BBbyCBCTRunReq): Elem = {
    val subDir = new File(extendedData.output.dir, cbctDirName)

    def descriptionOf(al: AttributeList) = "Z : " + Util.fmtDbl(getZ(al))

    def fileNameOfPng(al: AttributeList) = Util.textToId(descriptionOf(al).replace("-", "neg")) + ".png"

    def fileNameOfHtml(al: AttributeList) = Util.textToId(descriptionOf(al).replace("-", "neg")) + ".html"

    def writeDicomImage(al: AttributeList): Unit = {
      if (al == null) {
        logger.warn("Internal error.  Attribute list is null")
      } else {
        //  val image = ConsumerFormatImageMaker.makeEightBitImage(al)
        val image = new DicomImage(al).toBufferedImage(Color.white)
        Config.applyWatermark(image)
        val pngFile = new File(subDir, fileNameOfPng(al))
        Util.writePng(image, pngFile)
      }
    }

    def writeDicomMetaData(al: AttributeList): Unit = {
      val content = {
        <div>
          <div class="row">
            <div class="col-md-3 col-md-offset-1">
              <h2>CBCT
                {descriptionOf(al)}
              </h2>
            </div>
            <div class="col-md-2">
              <h2></h2> <a href={"../" + mainReportFileName} title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-2">
              <h2></h2> <a href={cbctMainFileName} title="View thumbnails of all slices">Return to Thumbnails</a>
            </div>
          </div>
          <div class="row">
            <img src={fileNameOfPng(al)}/>
          </div>
          <div class="row">
            <p></p> <br></br>
            <pre>
              {WebUtil.nl + DicomUtil.attributeListToString(al)}
            </pre>{WebUtil.nl}<p></p>
          </div>
        </div>
      }
      val text = WebUtil.wrapBody(wrap(content, extendedData), "CBCT " + descriptionOf(al), None, c3 = true, None)
      val file = new File(subDir, fileNameOfHtml(al))
      Util.writeFile(file, text)
    }

    subDir.mkdirs

    val sortedCbct = runReq.cbctList.sortBy(al => getZ(al))
    sortedCbct.par.foreach(al => writeDicomImage(al))
    sortedCbct.par.foreach(al => writeDicomMetaData(al))

    @tailrec
    def sizedGroups(seq: Seq[AttributeList], grp: Seq[Seq[AttributeList]]): Seq[Seq[AttributeList]] = {
      if (seq.isEmpty) grp
      else sizedGroups(seq.drop(thumbnailsPerRow), grp :+ seq.take(thumbnailsPerRow))
    }

    val cbctThumbnail = {
      val groupedByLine = sizedGroups(sortedCbct, Seq[Seq[AttributeList]]())

      def alToHtml(al: AttributeList) = {
        <td>
          <a href={fileNameOfHtml(al)} class="screenshot" title={descriptionOf(al)} rel={fileNameOfPng(al)}>
            <img src={fileNameOfPng(al)} style="margin-right: 1px; margin-bottom: 1px;" height="64"/>
          </a>
        </td>
      }

      def lineToHtml(line: Seq[AttributeList]) = {
        <tr>
          {line.map(c => alToHtml(c))}
        </tr>
      }

      // Note that the margin-bottom for the table has to be large to create space so
      // that the tooltip images can be viewed in their entirety.
      val content = {
        <div>
          <div class="row">
            <div class="col-md-3 col-md-offset-1">
              <h2>View CBCT Slices</h2>
            </div>
            <div class="col-md-1">
              <h2></h2> <a href={"../" + mainReportFileName} title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-5">
              <h2></h2>
            </div>
          </div>
          <div class="row" style="margin-top: 20px;">
            Hover over images for larger view. Click to see metadata or download.
            <br></br>
            <table style="margin-bottom: 600px; ">
              {groupedByLine.map(line => lineToHtml(line))}
            </table>
          </div>
          <p style="margin=400px;"></p>
        </div>
      }
      content
    }

    val text = WebUtil.wrapBody(wrap(cbctThumbnail, extendedData), "CBCT Viewer", None, c3 = true, None)
    val file = new File(subDir, cbctMainFileName)
    Util.writeFile(file, text)

    val reference = {
      <a href={cbctDirName + "/" + cbctMainFileName}>View CBCT</a>
    }

    reference
  }

  /**
    * Create a web page for viewing and downloading the registration file.
    */
  private def makeRegReference(extendedData: ExtendedData, regAl: AttributeList): Elem = {

    val dicomFile = new File(extendedData.output.dir, "registration.dcm")
    DicomUtil.writeAttributeListToFile(regAl, dicomFile, "AQA")
    val htmlRegFileName = "registration.html"

    val content = {
      <div>
        <div class="row">
          <div class="col-md-3 col-md-offset-1">
            <h2>Registration</h2>
          </div>
          <div class="col-md-2">
            <h2></h2> <a href={mainReportFileName} title="Return to main CBCT report">Main Report</a>
          </div>
          <div class="col-md-2">
            <h2></h2> <a href={dicomFile.getName} title="Download anonymized DICOM">Download DICOM</a>
          </div>
        </div>
        <div class="row">
          <pre>
            {WebUtil.nl + DicomUtil.attributeListToString(regAl)}
          </pre>{WebUtil.nl}<p></p>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(wrap(content, extendedData), "Registration for CBCT", None, c3 = true, None)
    val file = new File(extendedData.output.dir, htmlRegFileName)
    Util.writeFile(file, text)

    <a href={htmlRegFileName} title={"View / download Registration DICOM"}>REG DICOM</a>
  }

  /**
    * Create a web page for viewing and downloading the RTPLAN file.
    */
  private def makePlanReference(extendedData: ExtendedData, runReq: BBbyCBCTRunReq): Elem = {

    val planSop = Util.sopOfAl(runReq.rtplan)

    val dicomSeries = DicomSeries.getBySopInstanceUID(planSop)
    if (dicomSeries.isEmpty) {
      <div>Could not find referenced plan with UID
        {planSop}
      </div>
    } else {
      val rtplanAl = dicomSeries.head.attributeListList.head
      val dicomFile = new File(extendedData.output.dir, "rtplan.dcm")
      DicomUtil.writeAttributeListToFile(rtplanAl, dicomFile, "AQA")
      val htmlRtplanFileName = "rtplan.html"

      val content = {
        <div>
          <div class="row">
            <div class="col-md-3 col-md-offset-1">
              <h2>RTPLAN</h2>
            </div>
            <div class="col-md-2">
              <h2></h2> <a href={mainReportFileName} title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-2">
              <h2></h2> <a href={dicomFile.getName} title="Download anonymized DICOM">Download DICOM</a>
            </div>
          </div>
          <div class="row">
            <pre>
              {WebUtil.nl + DicomUtil.attributeListToString(rtplanAl)}
            </pre>{WebUtil.nl}<p></p>
          </div>
        </div>
      }

      val text = WebUtil.wrapBody(wrap(content, extendedData), "RTPLAN for CBCT", None, c3 = true, None)
      val file = new File(extendedData.output.dir, htmlRtplanFileName)
      Util.writeFile(file, text)

      <a href={htmlRtplanFileName} title={"View / download RTPLAN DICOM"}>RTPLAN DICOM</a>
    }
  }

  /**
    * Make the HTML.
    */
  def generateHtml(
      extendedData: ExtendedData,
      bbByCBCT: BBbyCBCT,
      imageSet: BBbyCBCTAnnotateImages.ImageSet,
      runReq: BBbyCBCTRunReq,
      cbctAnalysisResult: BBbyCBCTAnalysis.CBCTAnalysisResult,
      response: Response
  ): Unit = {
    val outputDir = extendedData.output.dir

    val chart = new BBbyCBCTChart(extendedData.output.outputPK.get)

    def writeImg(index: Int): Unit = {
      val pngFileFull = new File(outputDir, fileNameOfFull(index))
      Util.writePng(imageSet.fullImage(index), pngFileFull)

      val pngFileAoi = new File(outputDir, fileNameOfAreaOfInterest(index))
      Util.writePng(imageSet.areaOfInterest(index), pngFileAoi)
    }

    Seq(0, 1, 2).par.foreach(i => writeImg(i))

    val numberText = {
      def fmt(d: Double) = d.formatted("%5.2f")

      def dataCol(name: String, title: String, value: Double, cols: Int) = {
        <div title={title + " : " + value.formatted("%9.6f")} class={"col-md-" + cols}>
          <h3>
            {name}
            :
            {fmt(value)}
          </h3>
        </div>
      }

      val elem = {
        <div class="row">
          <div title="Test performed" class="col-md-3">
            <h2>CBCT BB Location
              <span style="margin-left:40px;">
                {WebUtil.coordinateDiagramElem(80)}
              </span>
            </h2>
          </div>{dataCol("Offset(mm)", "Distance in mm between plan isocenter and position of BB", bbByCBCT.offset_mm, 2)}{
          dataCol("X", "Plan X position - BB X position in mm", bbByCBCT.err_mm.getX, 1)
        }{dataCol("Y", "Plan Y position - BB Y position in mm", bbByCBCT.err_mm.getY, 1)}{dataCol("Z", "Plan Z position - BB Z position in mm", bbByCBCT.err_mm.getZ, 1)}
        </div>
      }
      elem
    }

    val viewCbctSlices = {
      <div title="View and download DICOM images and metadata">
        {makeCbctSlices(extendedData, runReq)}
      </div>
    }

    val viewReg = {
      <div title="View and download registration file">
        {makeRegReference(extendedData, runReq.reg.get)}
      </div>
    }

    val viewRtplan = {
      <div title="View and download DICOM RTPLAN">
        {makePlanReference(extendedData, runReq)}
      </div>
    }

    val tablePosition = {
      val x = Util.fmtDbl(bbByCBCT.tableXlateral_mm / 10)
      val y = Util.fmtDbl(bbByCBCT.tableYvertical_mm / 10)
      val z = Util.fmtDbl(bbByCBCT.tableZlongitudinal_mm / 10)

      <div title="Table position when CBCT was captured.">
        Table Position X,Y,Z cm:
        {x + "," + y + "," + z}
      </div>
    }

    val matlabReference = {
      <div title="Executable Matlab script showing calculations. Copy and paste to Matlab.">
        <a href={matlabFileName}>Matlab</a>
      </div>
    }

    val details = {
      <div class="row">
        <div class="col-md-2">
          {viewCbctSlices}
        </div>
        <div class="col-md-2">
          {C3ChartHistory.htmlHelp()}
        </div>
        <div class="col-md-2">
          {if (runReq.reg.isDefined) viewReg}
        </div>
        <div class="col-md-2">
          {viewRtplan}
        </div>
        <div class="col-md-3">
          {tablePosition}
        </div>
        <div class="col-md-1">
          {matlabReference}
        </div>
      </div>
    }

    def imageHtmlWithZoom(imageFileNameFull: String, imageFileNameAoi: String, title: String) = {
      <center title={title}>
        <br></br>
        <a href={imageFileNameFull}>
          <div class='zoom' id={Util.textToId(imageFileNameAoi)}>
            <img src={imageFileNameAoi} class="img-responsive fit-image"/>
          </div>
        </a>
      </center>
    }

    def makeImages: Elem = {
      <div class="row">
        <div class="col-md-8">
          {imageHtmlWithZoom(fileNameOfFull(2), fileNameOfAreaOfInterest(2), "Transverse / Axial")}
        </div>
        <div class="col-md-4">
          <div class="row">
            {imageHtmlWithZoom(fileNameOfFull(0), fileNameOfAreaOfInterest(0), "Sagittal")}
          </div>
          <div class="row">
            {imageHtmlWithZoom(fileNameOfFull(1), fileNameOfAreaOfInterest(1), "Frontal")}
          </div>
        </div>
      </div>
    }

    def mainContent = {
      <div class="row">
        {numberText}{details}{chart.chartReference}{makeImages}
      </div>
    }

    val runScript = {
      def zoomy(index: Int) = {
        "\n" +
          "      $(document).ready(function(){ $('#" + Util.textToId(fileNameOfAreaOfInterest(index)) + "').zoom(); });\n" +
          "      $(document).ready(function(){ $('#" + Util.textToId(fileNameOfFull(index)) + "').zoom(); });"
      }

      val zoomList = Seq(0, 1, 2).map(index => zoomy(index))
      val zoomScript = zoomList.mkString("\n<script>\n      ", "\n      ", "\n</script>")
      val chartRef = BBbyCBCTChartHistoryRestlet.makeReference(extendedData.output.outputPK.get)

      zoomScript + "\n" + chartRef
    }

    val text = WebUtil.wrapBody(wrap(mainContent, extendedData), "BB Location by CBCT", None, c3 = true, Some(runScript))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, text)
    new BBbyCBCTMatlabScript(extendedData.output, extendedData.machine, cbctAnalysisResult, runReq, response).make()
    //makeMatlabScript(outputDir, extendedData.output.outputPK.get, cbctAnalysisResult, runReq, response)

    //makeCbctSlices(extendedData, runReq)
  }
}
