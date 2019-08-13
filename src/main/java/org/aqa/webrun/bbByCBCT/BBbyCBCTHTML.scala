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

object BBbyCBCTHTML {

  private val axisNameList = Seq("X Axis : side", "Y Axis : top", "Z Axis : longitudinal")
  private val axisTitleList = Seq(
    "View as seen while standing by the side of the couch and looking horizontally.",
    "View as seen from leaning over the side of the couch and looking down.",
    "View as seen while standing at the foot of the couch horizontally.")
  //private def idOf(index: Int) = axisNameList(index).replaceAll(" ", "_") + "axisView"
  private def fileNameOfFull(index: Int) = Util.textToId(axisNameList(index)) + "axisViewFull.png"
  private def fileNameOfAreaOfInterest(index: Int) = Util.textToId(axisNameList(index)) + "axisViewAOI.png"
  private def titleOf(index: Int) = axisNameList(index) + "View from " + axisNameList(index) + " axis"

  def generateHtml(extendedData: ExtendedData, bbByCBCT: BBbyCBCT, imageSet: BBbyCBCTAnnotateImages.ImageSet, status: ProcedureStatus.Value) = {

    val outputDir = extendedData.output.dir

    val chart = new BBbyCBCTChart(extendedData.output.outputPK.get)

    def writeImg(index: Int) = {
      val pngFileFull = new File(outputDir, fileNameOfFull(index))
      Util.writePng(imageSet.fullImage(index), pngFileFull)

      val pngFileAoi = new File(outputDir, fileNameOfAreaOfInterest(index))
      Util.writePng(imageSet.areaOfInterest(index), pngFileAoi)
    }

    Trace.trace
    Seq(0, 1, 2).par.map(i => writeImg(i))
    Trace.trace

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

    def viewTitle(index: Int) = {
      <div title={ axisTitleList(index) }>
        <h4>{ axisNameList(index) } View</h4>
      </div>
    }

    def imageHtml(index: Int, getImageFileName: Int => String, title: String) = {
      val name = getImageFileName(index)
      <a href={ name } title={ title }>
        <div id={ Util.textToId(name) }>
          <a href={ name }>
            <img src={ name } class="img-responsive"/>
          </a>
        </div>
      </a>
    }

    def imageHtmlWithZoom(index: Int, getImageFileName: Int => String, title: String, width: Int) = {
      val name = getImageFileName(index)
      <a href={ name } title={ title }>
        <div id={ Util.textToId(name) }>
          <a href={ name }>
            <img src={ name } class="img-responsive" width={ width.toString }/>
          </a>
        </div>
      </a>
    }

    def makeSet(index: Int): Elem = {
      val imageWidth = imageSet.areaOfInterest(index).getWidth

      <td>
        <center style="margin:50px;">
          { viewTitle(index) }
          <br/>
          { imageHtml(index, fileNameOfAreaOfInterest, "Area of Interest") }
          <br/>
          { imageHtmlWithZoom(index, fileNameOfFull, "Full Image", imageWidth) }
        </center>
      </td>
    }

    def content = {
      <div class="row">
        { numberText }
        { chart.chartReference }
        <table class="table table-responsive">
          <tr>
            { Seq(2, 1, 0).map(index => makeSet(index)) }
          </tr>
        </table>
      </div>
    }

    def wrap = {
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

    val runScript = {
      def zoomy(index: Int) = {
        "\n" +
          "      $(document).ready(function(){ $('#" + Util.textToId(fileNameOfAreaOfInterest(index)) + "').zoom(); });\n" +
          "      $(document).ready(function(){ $('#" + Util.textToId(fileNameOfFull(index)) + "').zoom(); });"
      }
      "\n<script>\n      " + Seq(0, 1, 2).map(index => zoomy(index)).mkString("\n      ") + "\n" + chart.chartScript + "\n</script>\n"
    }

    val text = WebUtil.wrapBody(wrap, "BB Location by CBCT", None, true, Some(runScript))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, text)
  }
}
