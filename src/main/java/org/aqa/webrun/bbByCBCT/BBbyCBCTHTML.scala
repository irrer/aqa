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

object BBbyCBCTHTML {

  private val axisNameList = Seq("X", "Y", "Z")
  private def idOf(index: Int) = axisNameList(index) + "axisView"
  private def fileNameOf(index: Int) = axisNameList(index) + "axisView.png"
  private def titleOf(index: Int) = axisNameList(index) + "View from " + axisNameList(index) + " axis"

  def generateHtml(extendedData: ExtendedData, bbByCBCT: BBbyCBCT, imageXYZ: Seq[BufferedImage], status: ProcedureStatus.Value) = {

    val outputDir = extendedData.output.dir

    def writeImg(index: Int) = {
      val pngFile = new File(outputDir, fileNameOf(index))
      Util.writePng(imageXYZ(index), pngFile)
    }

    imageXYZ.indices.map(i => writeImg(i))

    val errText = Util.fmtDbl(bbByCBCT.err.getX) + ", " + Util.fmtDbl(bbByCBCT.err.getY) + ", " + Util.fmtDbl(bbByCBCT.err.getZ)

    def imageHtml(index: Int) = {
      <tr>
        <td>
          <center id={ idOf(index) }>
            { axisNameList(index) }
            View
            <a href={ fileNameOf(index) }>
              <img src={ fileNameOf(index) } class="img-responsive"/>
            </a>
          </center>
        </td>
      </tr>
    }

    def content = {
      <div class="row">
        <h2>CBCT BB Location</h2>
        Distance from isocenter:{}
        (mm)
        <div title="Isocenter position minus BB position">
          X, Y, Z:{ errText }
        </div>
        <table class="table table-responsive">
          { imageXYZ.indices.map(index => imageHtml(index)) }
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

      <div class="row">
        <div class="row">
          { wrapElement(2, "Institution", extendedData.institution.name, true) }
          { wrapElement(1, "Data Acquisition", dataAcquisitionDate, false) }
          { wrapElement(1, "Analysis", twoLineDate.format(extendedData.output.analysisDate), false) }
          { wrapElement(1, "User", extendedData.user.id, true) }
          { wrapElement(1, "Elapsed", elapsed, false) }
          { wrapElement(1, "Procedure", procedureDesc, false) }
        </div>
        <div class="row">
          { content }
        </div>
      </div>
    }

    val runScript = {
      def zoomy(index: Int) = { """$(document).ready(function(){ $('""" + idOf(index) + """').zoom(); });""" }
      """\n<script>\n      """ + imageXYZ.indices.map(index => zoomy(index)).mkString("\n      ") + """\n</script>\n"""
    }

    val text = WebUtil.wrapBody(content, "BB Location by CBCT", None, false, Some(runScript))
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, text)
  }
}
