package org.aqa.webrun.phase2.wedge

import org.aqa.db.Output
import scala.xml.Elem
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import org.aqa.run.ProcedureStatus
import org.aqa.Util
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
import org.aqa.db.MaintenanceRecord
import com.pixelmed.dicom.TagFromName
import org.aqa.db.Baseline
import org.aqa.web.C3Chart.Tolerance
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

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

  private def profile(dicomImage: DicomImage, translator: IsoImagePlaneTranslator, isTransverse: Boolean, runReq: RunReq, attributeList: AttributeList, collimatorCenterOfRotation: Point2D.Double): Seq[ProfilePoint] = {

    val RescaleSlope = attributeList.get(TagFromName.RescaleSlope).getDoubleValues.head
    val RescaleIntercept = attributeList.get(TagFromName.RescaleIntercept).getDoubleValues.head

    val profileList = if (isTransverse) {
      val yTop = translator.iso2Pix(0, -Config.WedgeProfileThickness_mm / 2).getY
      val yBottom = translator.iso2Pix(0, Config.WedgeProfileThickness_mm / 2).getY
      val height = (yBottom - yTop).round.toInt
      val rect = new java.awt.Rectangle(0, yTop.round.toInt, dicomImage.width, height)
      val sums = dicomImage.getSubimage(rect).columnSums

      def indexToProfilePoint(i: Int) = {
        val position = translator.pix2Iso(i, 0).getX + collimatorCenterOfRotation.getX
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
        val position = translator.pix2Iso(0, i).getY + collimatorCenterOfRotation.getY
        val value = sums(i) / width
        new ProfilePoint(position, value)
      }
      val profList = sums.indices.map(i => indexToProfilePoint(i))
      profList
    }

    profileList.map(p => new ProfilePoint(p.position, (p.value * RescaleSlope) + RescaleIntercept))
  }

  private def annotateImage(beamName: String, runReq: RunReq): BufferedImage = {
    val image = runReq.rtimageMap(beamName).correctedDicomImage.get.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(image)
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(beamName).attributeList.get)
    Util.addGraticules(image, translator, lineColor)
    image
  }

  /**
   * Create the beam image, write it to disk, and return the HTML to reference and display it.
   */
  private def beamImage(beamName: String, id: String, runReq: RunReq, wedgeDir: File): Elem = {
    val bufferedImage = annotateImage(beamName, runReq)
    val pngFile = new File(wedgeDir, "Wedge_" + WebUtil.stringToUrlSafe(beamName) + ".png")
    Util.writePng(bufferedImage, pngFile)
    <center id={ id }><img class="img-responsive" src={ pngFile.getName }/></center>
  }

  private def histChart(wedgePoint: WedgePoint, extendedData: ExtendedData, history: Seq[WedgePoint.WedgePointHistory]): C3ChartHistory = {
    val beamHistory = history.filter(h => h.wedgeBeamName.equals(wedgePoint.wedgeBeamName) && h.backgroundBeamName.equals(wedgePoint.backgroundBeamName)).sortBy(_.date)
    val percentHistory = beamHistory.map(h => h.percentOfBackground_pct)
    val dateHistory = beamHistory.map(h => h.date)

    val maintenanceRecordList = MaintenanceRecord.getRange(extendedData.machine.machinePK.get, dateHistory.head, dateHistory.last)
    val yNew = {
      val i = beamHistory.indexWhere(h => h.date.getTime == extendedData.output.dataDate.get.getTime)
      Math.max(0, i)
    }

    val baseline: Option[Baseline] = {
      val maintenanceRecordBaseline = Baseline.findLatest(extendedData.machine.machinePK.get, WedgeAnalysis.makeBaselineName(wedgePoint.wedgePair))
      if (maintenanceRecordBaseline.isDefined) Some(maintenanceRecordBaseline.get._2) else None
    }

    val tolerance: Option[C3Chart.Tolerance] = {
      if (baseline.isDefined) {
        val value = baseline.get.value.toDouble
        Some(new Tolerance(value - Config.WedgeTolerance_pct, value + Config.WedgeTolerance_pct))
      } else None
    }

    val historyChart = new C3ChartHistory(
      maintenanceRecordList,
      None, None, // chart width, height
      "Date", // x axis label
      dateHistory,
      baseline, // baseline
      tolerance, // tolerance
      Seq("Percent of Background"), // y axis labels
      "Percent of Background", // y data label
      Seq(percentHistory), // y values to plot
      yNew, // index of y value that is new
      ".4g", // y number format
      Seq(lineColor) // y line colors
    )

    historyChart
  }

  private def beamToDisplay(wedgePoint: WedgePoint, extendedData: ExtendedData, runReq: RunReq, wedgeDir: File, history: Seq[WedgePoint.WedgePointHistory], collimatorCenterOfRotation: Point2D.Double): (Elem, String) = {
    val historyChart = histChart(wedgePoint, extendedData, history)

    val isTransverse = WedgeAnalysis.wedgeOrientationTransverse(wedgePoint.wedgeBeamName, runReq.rtplan.attributeList.get)
    val dicomImage = runReq.rtimageMap(wedgePoint.wedgeBeamName).correctedDicomImage.get
    val translator = new IsoImagePlaneTranslator(runReq.rtimageMap(wedgePoint.wedgeBeamName).attributeList.get)

    val wedgeProfile = profile(dicomImage, translator, isTransverse, runReq, runReq.rtimageMap(wedgePoint.wedgeBeamName).attributeList.get, collimatorCenterOfRotation)
    val valueChart = new C3Chart(None, None,
      "Position mm", "Position mm", wedgeProfile.map(p => p.position), ".3g",
      Seq("Level"), "Level", Seq(wedgeProfile.map(p => p.value)), ".3g", Seq(lineColor))

    val backgroundDerived = runReq.rtimageMap(wedgePoint.backgroundBeamName)
    val backgroundProfile = profile(backgroundDerived.correctedDicomImage.get, translator, isTransverse, runReq, backgroundDerived.attributeList.get, collimatorCenterOfRotation)
    val percentProfile = wedgeProfile.zip(backgroundProfile).map(wb => new ProfilePoint(wb._1.position, (wb._1.value * 100) / wb._2.value))
    val percentChart = new C3Chart(None, None,
      "Position mm", "Position mm", percentProfile.map(p => p.position), ".3g",
      Seq("Percent"), "Percent", Seq(percentProfile.map(p => p.value)), ".3g", Seq(lineColor))
    val backgroundChart = new C3Chart(None, None,
      "Position mm", "Position mm", backgroundProfile.map(p => p.position), ".3g",
      Seq("Level"), "Level", Seq(backgroundProfile.map(p => p.value)), ".3g", Seq(lineColor))

    val elem = {
      <div class="row">
        <div class="row">
          <div class="col-md-12">
            <h2>
              { wedgePoint.wedgeBeamName + " : " }
              <font color="orange" title="Center value from image">{ Util.fmtDbl(wedgePoint.backgroundValue_cu) } CU</font>
              /
              <font color="orange" title="Center value from image as percent of background"> { Util.fmtDbl(wedgePoint.percentOfBackground_pct) } %</font>
            </h2>
          </div>
        </div>
        <div class="row">
          <h4>History of Wedge Point</h4>
          { historyChart.html }
        </div>
        <div class="row">
          <div class="col-md-5">
            <h3>Wedge { wedgePoint.wedgeBeamName }</h3>
            { beamImage(wedgePoint.wedgeBeamName, "wedge", runReq, wedgeDir) }
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3 title="Wedge profile as percent of background">Wedge Factor</h3>
              { percentChart.html }
            </div>
            <div class="row">
              <h3>Wedge Profile</h3>
              { valueChart.html }
            </div>
          </div>
        </div>
        <div class="row">
          <div class="col-md-5">
            <h3>Background { wedgePoint.backgroundBeamName }</h3>
            { beamImage(wedgePoint.backgroundBeamName, "background", runReq, wedgeDir) }
          </div>
          <div class="col-md-5">
            <div class="row">
              <h3>Background Profile</h3>
              { backgroundChart.html }
            </div>
          </div>
        </div>
      </div>
    }

    val zoomScript = """
      $(document).ready(function(){ $('#wedge').zoom(); });
      $(document).ready(function(){ $('#background').zoom(); });
"""
    val js = historyChart.javascript + valueChart.javascript + percentChart.javascript + backgroundChart.javascript + zoomScript
    (elem, js)
  }

  def makeDisplay(extendedData: ExtendedData, status: ProcedureStatus.Value, runReq: RunReq, wedgePointList: Seq[WedgePoint], collimatorCenterOfRotation: Point2D.Double): Elem = {

    val outputDir = extendedData.output.dir
    val wedgeDir = new File(outputDir, "wedge")
    wedgeDir.mkdirs

    val history = WedgePoint.recentHistory(50, extendedData.machine.machinePK.get, extendedData.procedure.procedurePK.get, extendedData.output.dataDate)

    val htmlJs = {
      wedgePointList.map(wp => beamToDisplay(wp, extendedData, runReq, wedgeDir, history, collimatorCenterOfRotation))
    }

    val useAsBaselineButton = {
      val href = "/run/WedgeUseAsBaseline?outputPK=" + extendedData.output.outputPK.get
      val title = "Use the values for the " + wedgePointList.size + " beam(s) listed below" + WebUtil.titleNewline + "as the baseline for future wedge analysis"
      <a class="btn btn-primary" href={ href } role="button" title={ title }>
        Use As Baseline
      </a>
    }

    val content = {
      <div class="col-md-10 col-md-offset-1">
        { useAsBaselineButton }
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
