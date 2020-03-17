package org.aqa.webrun.phase2.vmat

import org.aqa.webrun.phase2.RunReq
import org.aqa.db.VMAT
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import scala.xml.Elem
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Util
import java.io.File
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.Config
import org.aqa.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace

object VMATHTML {
  def makeDisplay(extendedData: ExtendedData, runReq: RunReq, resultList: Seq[Seq[VMAT]], status: ProcedureStatus.Value): Elem = {

    val vmatDir = new File(extendedData.output.dir, "VMAT")
    vmatDir.mkdirs

    def bigFont(text: String): Elem = <font size="5">{ text }</font>
    def bigFontDbl(d: Double): Elem = bigFont(Util.fmtDbl(d))
    def bigFontDblTd(d: Double): Elem = <td>{ bigFont(Util.fmtDbl(d)) }</td>

    def makeSet(vmatList: Seq[VMAT]): (Elem, String) = {
      val id = FileUtil.replaceInvalidFileNameCharacters(vmatList.head.beamNameMLC, '_')

      def header(vmat: VMAT): Elem = {
        <th>{ Util.fmtDbl((vmat.leftRtplan_mm + vmat.rightRtplan_mm) / (2 * 10)) } cm</th>
      }

      val mlcValues = {
        vmatList.map(vmat => bigFontDblTd(vmat.doseMLC_cu))
      }

      val openValues = {
        vmatList.map(vmat => bigFontDblTd(vmat.doseOpen_cu))
      }

      val corrValues = {
        vmatList.map(vmat => bigFontDblTd(vmat.percent))
      }

      val diffValues = {
        vmatList.map(vmat => bigFontDblTd(vmat.diff_pct))
      }

      val avgOfAbsoluteDeviations = vmatList.map(_.diff_pct.abs).sum / vmatList.size

      def makeImg: Elem = {
        val mlcDerived = runReq.derivedMap(vmatList.head.beamNameMLC)
        val pngFile = new File(vmatDir, id + ".png")
        val image = mlcDerived.pixelCorrectedImage.toDeepColorBufferedImage(0.001)
        Config.applyWatermark(image)
        val translator = new IsoImagePlaneTranslator(mlcDerived.attributeList)
        val lineColor = new Color(0x6688bb)
        Util.addGraticules(image, translator, lineColor)

        def drawRectangle(vmat: VMAT): Unit = {
          val graphics = ImageUtil.getGraphics(image)
          graphics.setColor(Color.BLACK)
          def xOf(v: Double) = translator.iso2PixCoordX(v).round.toInt
          def yOf(v: Double) = translator.iso2PixCoordY(v).round.toInt
          graphics.setStroke(ImageUtil.solidLine)
          graphics.drawLine(xOf(vmat.leftAOI_mm), yOf(vmat.topAOI_mm), xOf(vmat.rightAOI_mm), yOf(vmat.topAOI_mm)) // top
          graphics.drawLine(xOf(vmat.leftAOI_mm), yOf(vmat.bottomAOI_mm), xOf(vmat.rightAOI_mm), yOf(vmat.bottomAOI_mm)) // bottom
          graphics.drawLine(xOf(vmat.leftAOI_mm), yOf(vmat.topAOI_mm), xOf(vmat.leftAOI_mm), yOf(vmat.bottomAOI_mm)) // left
          graphics.drawLine(xOf(vmat.rightAOI_mm), yOf(vmat.topAOI_mm), xOf(vmat.rightAOI_mm), yOf(vmat.bottomAOI_mm)) // right
        }

        vmatList.map(vmat => drawRectangle(vmat))
        Util.writePng(image, pngFile)
        <center id={ id }>
          <div class="zoom" id={ id }>
            <img width="600" src={ pngFile.getName }/>
          </div>
        </center>
      }

      val table: Elem = {
        <div class="row" style="margin:50px;">
          <table class="table table-responsive table-bordered">
            <tr>
              <th>Band Number</th>
              { vmatList.sortBy(_.leftRtplan_mm).map(vmat => header(vmat)) }
            </tr>
            <tr>
              <td>{ bigFont("R") }LS</td>
              { mlcValues }
            </tr>
            <tr>
              <td>{ bigFont("R") }Open</td>
              { openValues }
            </tr>
            <tr>
              <td>{ bigFont("R") }corr</td>
              { corrValues }
            </tr>
            <tr>
              <td>{ bigFont("Diff(X)") }</td>
              { diffValues }
            </tr>
            <tr>
              <td colspan={ (vmatList.size + 1).toString }><p></p></td>
            </tr>
            <tr>
              <td colspan={ (vmatList.size).toString }>{ bigFont("Average of absolute deviations (Diff<sub>Abs</sub>)") }</td>
              <td>{ bigFontDbl(avgOfAbsoluteDeviations) }</td>
            </tr>
          </table>
          <div>
            { makeImg }
          </div>
        </div>
      }

      val js = """$(document).ready(function(){ $('#""" + id + """').zoom(); });"""

      (table, js)
    }

    val setList = resultList.map(vmatList => makeSet(vmatList))

    Trace.trace("setList.size: " + setList.size)

    val content: Elem = {
      <div>
        <div class="row" style="margin:50px;">
          <div class="row" style="margin:50px;">
            <div class="col-md-12">
              { setList.map(setJs => setJs._1) }
            </div>
          </div>
        </div>
      </div>
    }

    val status = ProcedureStatus.pass
    val js = "<script>" + setList.map(setJs => setJs._2).mkString("\n") + "</script>"
    val mainFile = new File(vmatDir, "VMAT.html")
    val text = Phase2Util.wrapSubProcedure(extendedData, content, "VMAT", status, Some(js), runReq)
    Util.writeBinaryFile(mainFile, text.getBytes)
    val iconImage = if (status.toString.equals(ProcedureStatus.pass.toString)) Config.passImageUrl else Config.failImageUrl
    <div>
      <a href={ vmatDir.getName + "/" + mainFile.getName }>VMAT</a>
      <img src={ iconImage } height="32"/>
    </div>
  }

}