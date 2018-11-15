package org.aqa.webrun.phase2

import org.aqa.db.Output
import scala.xml.Elem
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import org.aqa.run.ProcedureStatus
import org.aqa.Util
import org.aqa.db.Wedge
import java.awt.geom.Point2D
import org.aqa.db.CenterDose
import com.pixelmed.dicom.AttributeList
import java.awt.image.BufferedImage
import java.awt.Color
import org.aqa.web.C3Chart
import org.aqa.IsoImagePlaneTranslator
import org.aqa.db.WedgePoint
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import org.aqa.web.WebUtil
import org.aqa.web.WebServer
import org.aqa.web.C3ChartHistory
import org.aqa.db.PMI
import org.aqa.web.C3ChartHistory
import com.pixelmed.dicom.TagFromName

object WedgeHTML {

  private val htmlFileName = "Wedge.html"

  private val lineColor = new Color(0x6688bb)

  private val scriptPrefix = {
    """
<script>
"""
  }

  private val scriptSuffix = {
    """
    </script>
"""
  }

  /**
   * Make a tiny summary and link to the detailed report.
   */
  private def makeSummary(status: ProcedureStatus.Value, wedgeDir: File) = {
    val iconImage = if ((status == ProcedureStatus.pass) || (status == ProcedureStatus.done)) Config.passImageUrl else Config.failImageUrl
    val elem = {
      <div title="Click for details.">
        <a href={ wedgeDir.getName + "/" + htmlFileName }>
          { WedgeAnalysis.subProcedureName }<br/>
          <img src={ iconImage } height="32"/>
        </a>
      </div>
    }
    elem
  }

  private case class ProfilePoint(position: Double, value: Double);

  private def profile(dicomImage: DicomImage, translator: IsoImagePlaneTranslator, isTransverse: Boolean, runReq: RunReq, attributeList: AttributeList): Seq[ProfilePoint] = {

    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    val profileList = if (isTransverse) {
      val yTop = translator.iso2Pix(0, -Config.WedgeProfileThickness_mm / 2).getY
      val yBottom = translator.iso2Pix(0, Config.WedgeProfileThickness_mm / 2).getY
      val height = (yBottom - yTop).round.toInt
      val rect = new java.awt.Rectangle(0, yTop.round.toInt, dicomImage.width, height)
      val sums = dicomImage.getSubimage(rect).columnSums

      def indexToProfilePoint(i: Int) = {
        val position = translator.pix2Iso(i, 0).getX
        val value = sums(i) / height
        new ProfilePoint(position, value)
      }
      val profList = sums.indices.map(i => indexToProfilePoint(i))
      profList
    } else {
      val xLeft = translator.iso2Pix(-Config.WedgeProfileThickness_mm / 2, 0).getX
      val xRight = translator.iso2Pix(Config.WedgeProfileThickness_mm / 2, 0).getX
      val width = (xRight - xLeft).round.toInt
      val rect = new java.awt.Rectangle(xLeft.round.toInt, 0, width, dicomImage.height)
      val sums = dicomImage.getSubimage(rect).rowSums

      def indexToProfilePoint(i: Int) = {
        val position = translator.pix2Iso(0, i).getY
        val value = sums(i) / width
        new ProfilePoint(position, value)
      }
      val profList = sums.indices.map(i => indexToProfilePoint(i))
      profList
    }

    profileList.map(p => new ProfilePoint(p.position, (p.value * RescaleSlope) + RescaleIntercept))
  }

  private def annotateImage(beamName: String, runReq: RunReq): BufferedImage = {
    val image = runReq.rtimageMap(beamName).correctedDicomImage.get.toDeepColorBufferedImage
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(beamName).attributeList.get)
    Util.addGraticules(image, translator, lineColor)
    image
  }

  /**
   * Create the beam image, write it to disk, and return the HTML to reference and display it.
   */
  private def beamImage(wedgePoint: WedgePoint, runReq: RunReq, wedgeDir: File): Elem = {
    val bufferedImage = annotateImage(wedgePoint.beamName, runReq)
    val pngFile = new File(wedgeDir, "Wedge_" + WebUtil.stringToUrlSafe(wedgePoint.beamName) + ".png")
    Util.writePng(bufferedImage, pngFile)
    <img class="img-responsive" src={ pngFile.getName }/>
  }

  private def histChart(beamName: String, extendedData: ExtendedData, history: Seq[WedgePoint.WedgePointHistory]): C3ChartHistory = {
    val beamHistory = history.filter(h => h.beamName.equals(beamName)).sortBy(_.date)
    val percentHistory = beamHistory.map(h => h.percentOfFlood_pct)
    val dateHistory = beamHistory.map(h => h.date)

    val pmiList = PMI.getRange(extendedData.machine.machinePK.get, dateHistory.head, dateHistory.last)
    val yNew = {
      val i = beamHistory.indexWhere(h => h.date.getTime == extendedData.output.dataDate.get.getTime)
      Math.max(0, i)
    }

    val historyChart = new C3ChartHistory(
      pmiList,
      None, None, // chart width, height
      "Date", // x axis label
      dateHistory,
      None, // baseline
      None, // tolerance
      Seq("Percent of Flood"), // y axis labels
      "Percent of Flood", // y data label
      Seq(percentHistory), // y values to plot
      yNew, // index of y value that is new
      ".4g", // y number format
      Seq(lineColor) // y line colors
    )

    historyChart
  }

  private def beamToDisplay(wedgePoint: WedgePoint, extendedData: ExtendedData, runReq: RunReq, wedgeDir: File, history: Seq[WedgePoint.WedgePointHistory]): (Elem, String) = {
    val historyChart = histChart(wedgePoint.beamName, extendedData, history)

    val isTransverse = WedgeAnalysis.wedgeOrientationTransverse(wedgePoint.beamName, runReq.rtplan.attributeList.get)
    val dicomImage = runReq.rtimageMap(wedgePoint.beamName).correctedDicomImage.get
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(wedgePoint.beamName).attributeList.get)

    val wedgeProfile = profile(dicomImage, translator, isTransverse, runReq, runReq.rtimageMap(wedgePoint.beamName).attributeList.get)
    val valueChart = new C3Chart(None, None,
      "Position mm", "Position mm", wedgeProfile.map(p => p.position), ".3g",
      Seq("Level"), "Level", Seq(wedgeProfile.map(p => p.value)), ".3g", Seq(lineColor))

    val floodProfile = profile(runReq.floodCorrectedImage, translator, isTransverse, runReq, runReq.flood.attributeList.get)
    val percentProfile = wedgeProfile.zip(floodProfile).map(wf => new ProfilePoint(wf._1.position, (wf._1.value * 100) / wf._2.value))
    val percentChart = new C3Chart(None, None,
      "Position mm", "Position mm", percentProfile.map(p => p.position), ".3g",
      Seq("Percent"), "Percent", Seq(percentProfile.map(p => p.value)), ".3g", Seq(lineColor))

    val elem = {
      <div class="row">
        <div class="row">
          <div class="col-md-12">
            <h2>
              { wedgePoint.beamName + " : " }
              <font color="orange" title="Center value from image">{ Util.fmtDbl(wedgePoint.floodValue_cu) } CU</font>
              /
              <font color="orange" title="Center value from image as percent of flood"> { Util.fmtDbl(wedgePoint.percentOfFlood_pct) } %</font>
            </h2>
          </div>
        </div>
        <div class="row">
          <h4>History of Wedge Point</h4>
          { historyChart.html }
        </div>
        <div class="row">
          <div class="col-md-5">
            { beamImage(wedgePoint, runReq, wedgeDir) }
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3>Wedge as Percent of Flood</h3>
              { percentChart.html }
            </div>
            <div class="row">
              <h3>Wedge profile</h3>
              { percentChart.html }
              { valueChart.html }
            </div>
          </div>
        </div>
      </div>
    }

    val js = historyChart.javascript + valueChart.javascript + percentChart.javascript
    (elem, js)
  }

  def makeDisplay(extendedData: ExtendedData, status: ProcedureStatus.Value, runReq: RunReq, wedgePointList: Seq[WedgePoint]): Elem = {

    val outputDir = extendedData.output.dir
    val wedgeDir = new File(outputDir, "wedge")
    wedgeDir.mkdirs

    val history = WedgePoint.recentHistory(50, extendedData.machine.machinePK.get, extendedData.procedure.procedurePK.get, extendedData.output.dataDate)

    val htmlJs = {
      wedgePointList.map(wp => beamToDisplay(wp, extendedData, runReq, wedgeDir, history))
    }

    def wedgeHtml(wedgeList: Seq[Wedge]): Elem = {
      <div class="row">
        <h3>Beam { wedgeList.head.beamName } </h3>
        <div id={ WedgeProfileChart.beamRef(wedgeList.head.beamName) }>filler</div>
      </div>
    }

    val content = {
      <div class="col-md-10 col-md-offset-1">
        { htmlJs.map(ej => ej._1) }
      </div>
    }

    val javascript = "<script>\n" + htmlJs.map(ej => ej._2).mkString("\n") + "</script>\n"

    val html = Phase2Util.wrapSubProcedure(extendedData, content, WedgeAnalysis.subProcedureName, status, Some(javascript), runReq)
    val outFile = new File(wedgeDir, htmlFileName)
    Util.writeFile(outFile, html)

    makeSummary(status, wedgeDir)
  }

}
