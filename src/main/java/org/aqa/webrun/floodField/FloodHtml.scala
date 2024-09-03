package org.aqa.webrun.floodField

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.Logging
import org.aqa.web.C3Chart
import org.restlet.Response

import java.awt.Color
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.File
import scala.xml.Elem

/**
  * Make the HTML content for the FloodField procedure.
  */
object FloodHtml extends Logging {

  private case class ElemJs(html: Elem, javascript: String) {}

  private val pngId = "floodField" // HTML tag
  private val pngHighContrastId = "floodFieldHighContrast" // HTML tag

  private val pngFileName = pngId + ".png" // name of PNG file
  private val pngHighContrastFileName = pngHighContrastId + ".png" // name of high contrast PNG file

  private val dicomMetadataFileName = "DicomMetadata.html" // name of file for DICOM metadata

  /**
    * Format the image's histogram into a chart.
    * @param hist Image histogram.
    * @return A chart.
    */
  private def GraphHistogram(hist: Seq[DicomImage.HistPoint]): ElemJs = {
    val xValues = hist.map(vc => vc.value.toDouble)
    val yValues = hist.map(vc => vc.count.toDouble)
    val chart = new C3Chart(
      xAxisLabel = "Pixel Value",
      xDataLabel = "Pixel Value",
      xValueList = xValues,
      xFormat = ".0d",
      yAxisLabels = Seq("Count"),
      yDataLabel = "Count",
      yValues = Seq(yValues),
      yFormat = ".0d",
      yColorList = Seq(Color.DARK_GRAY)
    )

    ElemJs(chart.html, chart.javascript)
  }

  /**
    * Format either a vertical or horizontal profile as a chart.
    * @param xValueList List of profile values.
    * @return
    */
  private def GraphProfile(xValueList: Seq[Float]): ElemJs = {
    val chart = new C3Chart(
      xAxisLabel = "Pixel Value",
      xDataLabel = "Pixel Value",
      xValueList = xValueList.indices.map(_.toDouble),
      xFormat = ".0d",
      yAxisLabels = Seq("CU"),
      yDataLabel = "CU",
      yValues = Seq(xValueList.map(_.toDouble)),
      yFormat = ".0d",
      yColorList = Seq(Color.blue)
    )

    ElemJs(chart.html, chart.javascript)
  }

  /**
    * Make HTML for showing the histogram.
    * @param image For this image.
    * @return HTML.
    */
  private def histogramHtml(image: DicomImage): (Elem, String) = {
    // val graphHistogram = GraphHistogram(image.binnedHistogram(512))
    val histogram = image.histogram
    val chart = GraphHistogram(histogram)
    val html = {
      <div title="Shows how many times each approximatge pixel value occurs.">
        <h3>Histogram of Pixel Values verses Count</h3>
        <h4>Number of distinct values: {histogram.size}  Min: {histogram.head.value}   Max: {histogram.last.value}  </h4>
        {chart.html}
      </div>
    }
    (html, chart.javascript)
  }

  /**
    * Construct and write the HTML file for the DICOM metadata.
    * @param extendedData Metadata for this output.
    * @param floodField DICOM of flood field.
    */
  private def writeDicomMetadataFile(extendedData: ExtendedData, floodField: AttributeList): Unit = {
    DicomUtil.attributeListToString(floodField)

    val content = {
      <div class="row">
        <div class="col-md-10 col-md-offset-1">
          <a href="display.html">Main Report</a>
          <h2>DICOM Image and <a href="#meta">Metadata</a> (scroll down)</h2>
          <img src={pngHighContrastFileName}/>
          <h2 id="meta" style="margin-top:40px; margin-bottom:10px;>DICOM Metadata">DICOM Metadata</h2>
          <pre style="margin-bottom:100px;">{WebUtil.nl + DicomUtil.attributeListToString(floodField)}</pre>
        </div>
      </div>
    }
    val text = WebUtil.wrapBody(content = ExtendedData.wrapExtendedData(extendedData, content), pageTitle = "Flood Field Metadata", c3 = true, runScript = None)
    val dicomMetadataFile = new File(extendedData.output.dir, dicomMetadataFileName)
    Util.writeFile(dicomMetadataFile, text)
    logger.info(s"Wrote HTML file $dicomMetadataFile")
  }

  /**
    * Make HTML for showing the image.  Show both a normal rendering and also a high contrast
    * rendering that ignores the pixels in the corners.
    * @param extendedData Metadata.
    * @param image DicomImage
    * @param attributeList DICOM
    * @return HTML to show
    */
  private def imageHtml(extendedData: ExtendedData, image: DicomImage, attributeList: AttributeList): Elem = {

    val bufferedImageHighContrast: BufferedImage = FloodUtil.highContrastImage(attributeList)

    val translator = new IsoImagePlaneTranslator(attributeList)
    val graticuleColor = Color.gray

    Util.addGraticules(bufferedImageHighContrast, translator, graticuleColor)
    val pngHighContrastFile = new File(extendedData.output.dir, pngHighContrastFileName)
    Util.writePng(bufferedImageHighContrast, pngHighContrastFile)

    val bufferedImage = image.toDeepColorBufferedImage(0.5)
    Util.addGraticules(bufferedImage, translator, graticuleColor)
    val pngFile = new File(extendedData.output.dir, pngFileName)
    Util.writePng(bufferedImage, pngFile)

    writeDicomMetadataFile(extendedData, attributeList)

    val html = {
      <div class="row" style="margin-top:25px;margin-bottom:25px;">
        <div class="col-md-6 col-md-offset-3"> 
          <div style="text-align:center">
            <br/>Click to view full image and metadata
            <br/>
            <a href={dicomMetadataFileName}>
              <div class="zoom" id={pngHighContrastId}>
                <img width="300" src={pngHighContrastFileName}/>
              </div>
            </a>
          </div>
        </div>
      </div>
    }

    html
  }

  /**
    * Generate HTML that shows flood field information.
    *
    * @param extendedData meta data
    * @param runReq Contains DICOM flood field.
    * @param response HTML response.
    */
  def makeHtml(extendedData: ExtendedData, runReq: FloodRunReq, response: Response): Unit = {

    val title = "Flood Field"

    val image = new DicomImage(runReq.floodField)

    val hist = histogramHtml(image)

    val thickness_pix = 1
    val horz = image.getSubimage(new Rectangle(0, (image.height - thickness_pix) / 2, image.width, thickness_pix)).columnSums.map(_ / thickness_pix)
    val vert = image.getSubimage(new Rectangle((image.width - thickness_pix) / 2, 0, thickness_pix, image.height)).rowSums.map(_ / thickness_pix)

    val horzChart = GraphProfile(horz)
    val vertChart = GraphProfile(vert)

    val content = {
      <div>
        {imageHtml(extendedData, image, runReq.floodField)}
        <p> </p>
        <h3>Profile of horizontal band one pixel thick from left to right across image center</h3>
        {horzChart.html}
        <p> </p>
        <h3>Profile of vertical band one pixel thick from top to bottom down image center</h3>
        {vertChart.html}
        <p> </p>
        {hist._1}
        <p> </p>
      </div>
    }

    val runScript =
      s"""<script>
         |${hist._2}
         |${horzChart.javascript}
         |${vertChart.javascript}
         | $$(document).ready(function(){ $$('#$pngId').zoom(); });
         | $$(document).ready(function(){ $$('#$pngHighContrastId').zoom(); });
         |
         |</script>
         |""".stripMargin
    val displayFile = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    val text = WebUtil.wrapBody(content = ExtendedData.wrapExtendedData(extendedData, content), pageTitle = title, c3 = true, runScript = Some(runScript))
    Util.writeFile(displayFile, text)
    logger.info(s"Wrote HTML file $displayFile")
  }
}
