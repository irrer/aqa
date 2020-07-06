package org.aqa.webrun.bbByCBCT

import org.aqa.db.BBbyCBCT
import java.awt.image.BufferedImage
import org.aqa.webrun.ExtendedData
import java.io.File
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.run.ProcedureStatus
import java.text.SimpleDateFormat
import scala.xml.Elem
import org.aqa.web.WebUtil
import org.aqa.db.Output
import org.aqa.web.MachineUpdate
import edu.umro.ScalaUtil.Trace
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import com.pixelmed.display.ConsumerFormatImageMaker
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.DicomSeries
import org.aqa.Config
import com.pixelmed.dicom.AttributeTag
import org.aqa.web.OutputList
import org.aqa.Logging

object BBbyCBCTHTML extends Logging {

  private val axisNameList = Seq("X Axis : side", "Y Axis : top", "Z Axis : longitudinal")
  private val axisTitleList = Seq(
    "View as seen while standing by the side of the couch and looking horizontally.",
    "View as seen from leaning over the side of the couch and looking down.",
    "View as seen while standing at the foot of the couch horizontally.")
  //private def idOf(index: Int) = axisNameList(index).replaceAll(" ", "_") + "axisView"
  private def fileNameOfFull(index: Int) = Util.textToId(axisNameList(index)) + "axisViewFull.png"
  private def fileNameOfAreaOfInterest(index: Int) = Util.textToId(axisNameList(index)) + "axisViewAOI.png"
  private def titleOf(index: Int) = axisNameList(index) + "View from " + axisNameList(index) + " axis"
  private val cbctDirName = "cbct"
  private val mainReportFileName = Output.displayFilePrefix + ".html"
  private val cbctMainFileName = "cbctViewer.html"
  private val thumbnailsPerRow = 10
  private def getZ(al: AttributeList): Double = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

  private def wrap(content: Elem, extendedData: ExtendedData) = {
    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")
    def wrapElement(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span aqaalias="">{ value }</span>
        } else {
          val valueList = value.split("\n");
          { <span>{ valueList.head }{ valueList.tail.map(line => { <span><br/> { line } </span> }) }</span> }
        }

      { <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ html }</div> }

    }

    val dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }

    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val showMachine = {
      val href = "/admin/MachineUpdate?machinePK=22"
      <div class="col-md-1">
        <h2 title="Treatment machine.  Click for details.">{ MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id) }</h2>
      </div>
    }

    val elem = {
      <div class="row">
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            { showMachine }
            { wrapElement(2, "Institution", extendedData.institution.name, true) }
            { wrapElement(1, "Data Acquisition", dataAcquisitionDate, false) }
            { wrapElement(1, "Analysis Started", twoLineDate.format(extendedData.output.startDate), false) }
            { wrapElement(1, "User", extendedData.user.id, true) }
            { wrapElement(1, "Elapsed", elapsed, false) }
            { wrapElement(1, "Procedure", procedureDesc, false) }
            <div class="col-md-1">{ OutputList.redoUrl(extendedData.output.outputPK.get) }</div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            { content }
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

    def writeDicomImage(al: AttributeList) = {
      if (al == null) {
        logger.warn("Internal error.  Attribute list is null")
      } else {
        val image = ConsumerFormatImageMaker.makeEightBitImage(al)
        Config.applyWatermark(image)
        val pngFile = new File(subDir, fileNameOfPng(al))
        Util.writePng(image, pngFile)
      }
    }

    def writeDicomMetaData(al: AttributeList) = {
      val content = {
        <div>
          <div class="row">
            <div class="col-md-3 col-md-offset-1">
              <h2>CBCT { descriptionOf(al) }</h2>
            </div>
            <div class="col-md-2">
              <h2> </h2><a href={ "../" + mainReportFileName } title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-2">
              <h2> </h2><a href={ cbctMainFileName } title="View thumbnails of all slices">Return to Thumbnails</a>
            </div>
          </div>
          <div class="row">
            <img src={ fileNameOfPng(al) }/>
          </div>
          <div class="row">
            <p> </p><br> </br>
            <pre>
              { WebUtil.nl + DicomUtil.attributeListToString(al) }
            </pre>
            { WebUtil.nl }
            <p> </p>
          </div>
        </div>
      }
      val text = WebUtil.wrapBody(wrap(content, extendedData), "CBCT " + descriptionOf(al), None, true, None)
      val file = new File(subDir, fileNameOfHtml(al))
      Util.writeFile(file, text)
    }

    subDir.mkdirs

    val sortedCbct = runReq.cbctList.sortBy(al => getZ(al))
    sortedCbct.par.map(al => writeDicomImage(al))
    sortedCbct.par.map(al => writeDicomMetaData(al))

    def sizedGroups(seq: Seq[AttributeList], grp: Seq[Seq[AttributeList]]): Seq[Seq[AttributeList]] = {
      if (seq.isEmpty) grp
      else sizedGroups(seq.drop(thumbnailsPerRow), grp :+ seq.take(thumbnailsPerRow))
    }

    val cbctThumbnail = {
      val groupedByLine = sizedGroups(sortedCbct, Seq[Seq[AttributeList]]())

      def alToHtml(al: AttributeList) = {
        <td>
          <a href={ fileNameOfHtml(al) } class="screenshot" title={ descriptionOf(al) } rel={ fileNameOfPng(al) }>
            <img src={ fileNameOfPng(al) } style="margin-right: 1px; margin-bottom: 1px;" height="64"/>
          </a>
        </td>
      }

      def lineToHtml(line: Seq[AttributeList]) = {
        <tr>
          { line.map(c => alToHtml(c)) }
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
              <h2> </h2><a href={ "../" + mainReportFileName } title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-5">
              <h2> </h2>
            </div>
          </div>
          <div class="row" style="margin-top: 20px;">
            Hover over images for larger view.  Click to see metadata or download.
            <br></br>
            <table style="margin-bottom: 600px; ">
              { groupedByLine.map(line => lineToHtml(line)) }
            </table>
          </div>
          <p style="margin=400px;"> </p>
        </div>
      }
      content
    }

    val text = WebUtil.wrapBody(wrap(cbctThumbnail, extendedData), "CBCT Viewer", None, true, None)
    val file = new File(subDir, cbctMainFileName)
    Util.writeFile(file, text)

    val reference = {
      <a href={ cbctDirName + "/" + cbctMainFileName }>View CBCT</a>
    }

    reference
  }

  /**
   * Create a web page for viewing and downloading the registration file.
   */
  private def makeRegReference(extendedData: ExtendedData, regAl: AttributeList): Elem = {

    val regSop = Util.sopOfAl(regAl)

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
            <h2> </h2><a href={ mainReportFileName } title="Return to main CBCT report">Main Report</a>
          </div>
          <div class="col-md-2">
            <h2> </h2><a href={ dicomFile.getName } title="Download anonymized DICOM">Download DICOM</a>
          </div>
        </div>
        <div class="row">
          <pre>
            { WebUtil.nl + DicomUtil.attributeListToString(regAl) }
          </pre>
          { WebUtil.nl }
          <p> </p>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody(wrap(content, extendedData), "Registration for CBCT", None, true, None)
    val file = new File(extendedData.output.dir, htmlRegFileName)
    Util.writeFile(file, text)

    <a href={ htmlRegFileName } title={ "View / download Registration DICOM" }>REG DICOM</a>
  }

  /**
   * Create a web page for viewing and downloading the RTPLAN file.
   */
  private def makePlanReference(extendedData: ExtendedData, runReq: BBbyCBCTRunReq): Elem = {

    val planSop = Util.sopOfAl(runReq.rtplan)

    val dicomSeries = DicomSeries.getBySopInstanceUID(planSop)
    if (dicomSeries.isEmpty) {
      <div>Could not find referenced plan with UID { planSop }</div>
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
              <h2> </h2><a href={ mainReportFileName } title="Return to main CBCT report">Main Report</a>
            </div>
            <div class="col-md-2">
              <h2> </h2><a href={ dicomFile.getName } title="Download anonymized DICOM">Download DICOM</a>
            </div>
          </div>
          <div class="row">
            <pre>
              { WebUtil.nl + DicomUtil.attributeListToString(rtplanAl) }
            </pre>
            { WebUtil.nl }
            <p> </p>
          </div>
        </div>
      }

      val text = WebUtil.wrapBody(wrap(content, extendedData), "RTPLAN for CBCT", None, true, None)
      val file = new File(extendedData.output.dir, htmlRtplanFileName)
      Util.writeFile(file, text)

      <a href={ htmlRtplanFileName } title={ "View / download RTPLAN DICOM" }>RTPLAN DICOM</a>
    }
  }
  def generateHtml(extendedData: ExtendedData, bbByCBCT: BBbyCBCT, imageSet: BBbyCBCTAnnotateImages.ImageSet, status: ProcedureStatus.Value, runReq: BBbyCBCTRunReq) = {

    val outputDir = extendedData.output.dir

    val chart = new BBbyCBCTChart(extendedData.output.outputPK.get)

    def writeImg(index: Int) = {
      val pngFileFull = new File(outputDir, fileNameOfFull(index))
      Util.writePng(imageSet.fullImage(index), pngFileFull)

      val pngFileAoi = new File(outputDir, fileNameOfAreaOfInterest(index))
      Util.writePng(imageSet.areaOfInterest(index), pngFileAoi)
    }

    Seq(0, 1, 2).par.map(i => writeImg(i))

    val numberText = {
      def fmt(d: Double) = d.formatted("%5.2f")

      def dataCol(name: String, title: String, value: Double, cols: Int) = {
        <div title={ title } class={ "col-md-" + cols }>
          <h3>{ name } : { fmt(value) }</h3>
        </div>
      }

      val errxText = {
        "X: " + fmt(bbByCBCT.err.getX) + " Y: " + fmt(bbByCBCT.err.getY) + "   Z: " + fmt(bbByCBCT.err.getZ)

      }

      val elem = {
        <div class="row">
          <div title="Test performed" class="col-md-3">
            <h2>CBCT BB Location</h2>
          </div>
          { dataCol("Offset(mm)", "Distance in mm between plan isocenter and position of BB", bbByCBCT.offset_mm, 2) }
          { dataCol("X", "Plan X position - BB X position in mm", (bbByCBCT.rtplanX_mm - bbByCBCT.cbctX_mm), 1) }
          { dataCol("Y", "Plan Y position - BB Y position in mm", (bbByCBCT.rtplanY_mm - bbByCBCT.cbctY_mm), 1) }
          { dataCol("Z", "Plan Z position - BB Z position in mm", (bbByCBCT.rtplanZ_mm - bbByCBCT.cbctZ_mm), 1) }
        </div>
      }
      elem
    }

    val viewCbctSlices = {
      <div title="View and download DICOM images and metadata" class="col-md-1">
        <h3> </h3>{ makeCbctSlices(extendedData, runReq) }
      </div>
    }

    val viewReg = {
      <div title="View and download registration file" class="col-md-1">
        <h3> </h3>{ makeRegReference(extendedData, runReq.reg.get) }
      </div>
    }

    val viewRtplan = {
      <div title="View and download DICOM RTPLAN" class="col-md-1">
        <h3> </h3>{ makePlanReference(extendedData, runReq) }
      </div>
    }

    val tablePosition = {
      def getDblCm(tag: AttributeTag) = runReq.cbctList.head.get(tag).getDoubleValues.head / 10
      val x = Util.fmtDbl(bbByCBCT.tableXlateral_mm)
      val y = Util.fmtDbl(bbByCBCT.tableYvertical_mm)
      val z = Util.fmtDbl(bbByCBCT.tableZlongitudinal_mm)

      <div title="Table position when CBCT was captured.">
        <h3> </h3>
        Table Position X,Y,Z cm:{ x + "," + y + "," + z }
      </div>
    }

    val details = {
      <div class="row">
        { viewCbctSlices }
        { if (runReq.reg.isDefined) viewReg }
        { viewRtplan }
        { tablePosition }
      </div>
    }

    def viewTitle(index: Int) = {
      <div title={ axisTitleList(index) }>
        <h4>{ axisNameList(index) } View</h4>
      </div>
    }

    def imageHtmlWithZoom(index: Int, getImageFileName: Int => String, title: String, width: Int) = {
      val name = getImageFileName(index)

      if (false) {
        <a href={ name } title={ title }>
          <div id={ Util.textToId(name.replace("-", "neg")) }>
            <a href={ name }>
              <img src={ name } class="img-responsive" width={ width.toString }/>
            </a>
          </div>
        </a>
      }

      <a href={ name }>
        <div class='zoom' id={ Util.textToId(name) }>
          <img width={ width.toString } src={ name }/>
        </div>
      </a>
    }

    def makeSet(index: Int): Elem = {
      val imageWidth = 350 // imageSet.areaOfInterest(index).getWidth

      <td>
        <center style="margin:50px;">
          { viewTitle(index) }
          <br/>
          { imageHtmlWithZoom(index, fileNameOfAreaOfInterest, "Area of Interest", imageWidth) }
          <br/>
          { imageHtmlWithZoom(index, fileNameOfFull, "Full Image", imageWidth) }
        </center>
      </td>
    }

    def mainContent = {
      <div class="row">
        { numberText }
        { details }
        { chart.chartReference }
        <table class="table table-responsive">
          <tr>
            { Seq(2, 1, 0).map(index => makeSet(index)) }
          </tr>
        </table>
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

    val text = WebUtil.wrapBody(wrap(mainContent, extendedData), "BB Location by CBCT", None, true, Some(runScript))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, text)

    //makeCbctSlices(extendedData, runReq)
  }
}
