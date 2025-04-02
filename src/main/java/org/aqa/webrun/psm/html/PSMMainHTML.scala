package org.aqa.webrun.psm.html

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.psm.PSMBeamAnalysisResult
import org.aqa.webrun.psm.PSMCharts
import org.aqa.webrun.psm.PSMGradientAscent

import java.io.File

/**
  * Generate HTML page to show all PSM data.
  */

class PSMMainHTML(
    extendedData: ExtendedData,
    rtplan: AttributeList,
    resultList: Seq[PSMBeamAnalysisResult],
    psmGradientAscent: PSMGradientAscent,
    ffAl: AttributeList,
    ffImg: DicomImage,
    wdAl: AttributeList,
    wdImg: DicomImage,
    rawAl: AttributeList,
    rawImg: DicomImage,
    cbrAl: AttributeList,
    cbrImg: DicomImage,
    brAl: AttributeList,
    brImg: DicomImage,
    psmAl: AttributeList,
    psmImg: DicomImage
) extends Logging {

  def make(): Unit = {

    val resultHtml = new ResultHtml(extendedData, resultList)

    val imageHtml = new ImageHTML(
      extendedData: ExtendedData,
      resultList: Seq[PSMBeamAnalysisResult],
      psmGradientAscent: PSMGradientAscent,
      ffAl: AttributeList,
      ffImg: DicomImage,
      wdAl: AttributeList,
      wdImg: DicomImage,
      rawAl: AttributeList,
      rawImg: DicomImage,
      cbrAl: AttributeList,
      cbrImg: DicomImage,
      brAl: AttributeList,
      brImg: DicomImage,
      psmAl: AttributeList,
      psmImg: DicomImage
    )

    val imageStuff = imageHtml.make()

    val historyCharts = new PSMCharts(extendedData.outputPK)

    val planHtml = PlanHTML(extendedData, rtplan)

    val content = {
      <div>
        <div class="row">
          <div class="col-md-2 col-md-offset-1" >
            {WebUtil.showPrecision}
          </div>
          <div class="col-md-2 col-md-offset-1" >
            <a href={planHtml.fileName}>View RTPLAN</a>
          </div>
        </div>
          <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <table class="table responsive table-bordered" style="margin-top:25px;">
              {imageStuff._1}
            </table>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            <h3>Mean Beam Values</h3>
            {historyCharts.meanChart.html}
            <h3>Standard Deviation of Beam Center Pixels</h3>
            {historyCharts.stdDevChart.html}
            <h3>Coordinates of Max Interpolated Points</h3>
            {historyCharts.maxInterpolationCoordinates.html}
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1" >
            {resultHtml.make()}
          </div>
        </div>
        <div class="row">
          <p style="margin-bottom:150px;"> </p>
        </div>
      </div>
    }

    val js =
      s"""<script>
         |${imageStuff._2}
         |</script>
         |${PSMBeamResponseChartRestlet.makeReference(extendedData.outputPK)}
         |""".stripMargin

    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "PSM", c3 = true, runScript = Some(js))
    val htmlFile = new File(extendedData.output.dir, "display.html")
    Util.writeFile(htmlFile, text)

  }

}
