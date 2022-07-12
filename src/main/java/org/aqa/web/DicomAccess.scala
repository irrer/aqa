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

package org.aqa.web

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.BadPixel

import java.awt.image.BufferedImage
import java.awt.BasicStroke
import java.awt.Color
import java.io.File
import java.io.FileOutputStream
import scala.xml.Elem

/**
 * Create a web page to display a DICOM file.
 */
object DicomAccess extends Logging {

  private def makeTitle(attrList: Seq[AttributeList]): String = {
    val SeriesDescription = attrList.map(al => al.get(TagFromName.SeriesDescription)).distinct.mkString("   ")
    val Modality = attrList.map(al => al.get(TagFromName.Modality)).distinct.mkString("   ")
    Modality + " : " + SeriesDescription
  }

  /** Size to show a pixel in pixels.  Equivalent to a magnification factor. */
  private val pixelSize = 50
  private val textPointSize = 11

  /** Show this number of zoomed bad pixels per row. */
  private val pixRow = 4

  private val badPixelDirName = "badPixels"

  private val pixelsPerRow = (BadPixel.radius * 2) + 1

  private val naColor = Color.lightGray
  private val naStroke = new BasicStroke(5, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(1), 0)

  def badPixelToPng(badPixel: DicomImage.PixelRating, dicomImage: DicomImage, fileBaseName: String, bufferedImage: BufferedImage, badPixelDir: File): File = {

    // create a buffered image and set it all to white
    val imgSize = (pixelSize + 1) * ((BadPixel.radius * 2) + 1) + 1
    val bufImg = new BufferedImage(imgSize, imgSize, BufferedImage.TYPE_INT_RGB)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(Color.white)
    graphics.fillRect(0, 0, imgSize, imgSize)

    def fmtPoint(bufX: Int, bufY: Int): Unit = {
      val xi = (bufX + BadPixel.radius) * (pixelSize + 1)
      val yi = (bufY + BadPixel.radius) * (pixelSize + 1)

      val xx = badPixel.x + bufX
      val yy = badPixel.y + bufY
      if (dicomImage.validPoint(xx, yy)) {
        val rbg = bufferedImage.getRGB(xx, yy)
        val color = new Color(rbg)
        graphics.setColor(color)
        graphics.fillRect(xi, yi, pixelSize, pixelSize)

        // color for good contrast to background
        val contrastingColor = {
          val brightness = (color.getRed + color.getGreen + color.getBlue) / 3.0
          if (brightness >= 128) Color.black else Color.white
        }
        graphics.setColor(contrastingColor)
        ImageText.setFont(graphics, ImageText.DefaultFont, textPointSize)
        val text = dicomImage.get(xx, yy).round.toInt.toString
        ImageText.drawTextCenteredAt(graphics, xi + pixelSize / 2, yi + pixelSize / 2, text)
      } else {
        graphics.setColor(naColor)
        graphics.setStroke(naStroke)
        graphics.drawRect(xi, yi, pixelSize, pixelSize)
      }
    }

    def range = -BadPixel.radius to BadPixel.radius
    for (bufX <- range; bufY <- range) fmtPoint(bufX, bufY)

    val fileName = fileBaseName + "_" + badPixel.x.toString + "-" + badPixel.y.toString + ".png"
    val pngFile = new File(badPixelDir, fileName)
    Util.writePng(bufImg, pngFile)
    // TODO rm ImageIO.write(bufImg, "png", pngFile)
    pngFile
  }

  private def badPixelToHtml(badPixel: DicomImage.PixelRating, dicomImage: DicomImage, fileBaseName: String, bufferedImage: BufferedImage, badPixelDir: File): Elem = {
    val pngFile = badPixelToPng(badPixel, dicomImage, fileBaseName, bufferedImage, badPixelDir)

    <div class="col-md-2" style="margin:8px;">
      <center>
        <h4 title="Pixel coordinates">{ badPixel.x.toString + ", " + badPixel.y.toString }</h4>
      </center>
      <img src={ WebServer.urlOfResultsFile(pngFile) }/>
    </div>
  }

  private class GraphHistogram(hist: Seq[DicomImage.HistPoint]) {
    private val xValues = hist.map(vc => vc.value.toDouble)
    private val yValues = hist.map(vc => vc.count.toDouble)
    val chart = new C3Chart(
      xAxisLabel = "Pixel Value",
      xDataLabel = "Pixel Value",
      xValueList = xValues,
      xFormat = ".0d",
      yAxisLabels = Seq("Count"),
      yDataLabel = "Count",
      yValues = Seq(yValues),
      yFormat = ".0d",
      yColorList = Seq(Color.DARK_GRAY))
  }

  /**
   * Remove the outlier pixels at the high end of the histogram.  Use a 5% change as a cutoff.
   */
  private def trimHistogram(h: Seq[(Float, Int)]): Seq[(Float, Int)] = {
    val start = h.lastIndexWhere(vc => vc._2 > 5)

    val pct = 5.0.toFloat

    val left = h.take(start + 1)
    val right = h.takeRight(h.size - left.size)

    val rVal = right.map(vc => vc._1)
    val rr = rVal.zip(rVal.tail :+ rVal.last)

    def tooMuchDelta(a: Float, b: Float) = { (((b - a) / a).abs * 100) > pct }
    val lastGood = {
      val lg = rr.indexWhere(pn => tooMuchDelta(pn._1, pn._2))
      if (lg < 0) 0 else lg
    }

    left ++ right.take(lastGood + 1)
  }

  private def makeImagePage(title: String, pngFile: File, outputDir: File, fileBaseName: String, dicomImage: DicomImage, bufferedImage: BufferedImage, badPixels: Seq[DicomImage.PixelRating], radius: Int): Elem = {

    val pngUrl = WebServer.urlOfResultsFile(pngFile)
    val imagePageFile = new File(outputDir, fileBaseName + "_image.html")
    val imagePageUrl = WebServer.urlOfResultsFile(imagePageFile)

    val badPixelDir = new File(outputDir, badPixelDirName)
    Util.mkdirs(badPixelDir)

    val badPixelVisualizationList = badPixels.map(badPixel => badPixelToHtml(badPixel, dicomImage, fileBaseName, bufferedImage, badPixelDir))
    val rowList = (0 until ((badPixelVisualizationList.size + pixRow - 1) / pixRow)).map(row => badPixelVisualizationList.drop(row * pixRow).take(pixRow))

    val dicomImageCorrected = dicomImage.correctBadPixels(badPixels, radius)
    val graphHistogramFixed = new GraphHistogram(dicomImageCorrected.binnedHistogram(512))

    def rowToHtml(row: Seq[Elem]): Elem = {
      { <div class="row">{ row }</div> }
    }
    val allRows = rowList.map(row => rowToHtml(row)).toList

    val imageContent = {
      <div>
        <a href={ imagePageUrl }><img src={ pngUrl }/></a>
        <p/>
        <h2 title="Zoomed view of bad pixels (centered in each) and raw values with immediate neighbors.">Bad Pixels : { badPixels.size }</h2>
        <p/>
        { allRows }
        <p/>
        <h2 title="Shows how many times each approximatge pixel value occurs.  Bad pixel values have been removed.">Compressed Histogram of Pixel Values verses Count</h2>
        { graphHistogramFixed.chart.html }
        <p/>
      </div>
    }

    val mainImagePage = {
      <div class="col-md-10 col-md-offset-1">
        <h2 title="Image with bad pixels annotated">{ title }</h2>
        { imageContent }
      </div>
    }

    val script = { """<script>""" + graphHistogramFixed.chart.javascript + """</script>""" }

    val imagePageText = WebUtil.wrapBody(mainImagePage, title, None, true, Some(script))
    val imagePageOutStream = new FileOutputStream(imagePageFile)
    imagePageOutStream.write(imagePageText.getBytes)
    imagePageOutStream.close

    <a href={ imagePageUrl }><img src={ pngUrl } width="256"></img></a>
  }

  private def makePage(al: AttributeList, title: String,
    dir: File, fileBaseName: String, bufferedImage: Option[BufferedImage],
    dicomImage: Option[DicomImage], badPixels: Seq[DicomImage.PixelRating]): Option[String] = {

    val pngFile = new File(dir, fileBaseName + ".png")

    val content = {
      val text = DicomUtil.attributeListToString(al)

      {
        <div>
          <br></br>
          { if (bufferedImage.isDefined) makeImagePage(title, pngFile, dir, fileBaseName, dicomImage.get, bufferedImage.get, badPixels, Util.badPixelRadius(al)) }
          <p>
            <pre title="DICOM meta-data">{ WebUtil.nl + text }</pre>
          </p>
        </div>
      }
    }

    val html = {
      <div class="row col-md-10 col-md-offset-1">
        <h2 title="Scroll down for metadata">{ title }</h2>
        { content }
      </div>
    }

    val htmlFile = new File(dir, fileBaseName + ".html")
    val htmlText = WebUtil.wrapBody(html, title)
    Util.writeBinaryFile(htmlFile, htmlText.getBytes)

    if (bufferedImage.isDefined) {
      badPixels.map(bp => ImageUtil.annotatePixel(bufferedImage.get, bp.x, bp.y, bp.x + ", " + bp.y, true))
      Util.writePng(bufferedImage.get, pngFile)
    }

    if (bufferedImage.isDefined) Some(WebServer.urlOfResultsFile(pngFile)) else None
  }

  /**
   * Create a page for viewing and downloading a DICOM file.
   *
   * @param dicom: Build display for this content
   *
   * @param title: Page title
   *
   * @param dir: Directory in which to write the HTML and image.
   *
   * @param contrastModel: How to render the image.  Only applicable for image modalities.
   *
   * @param badPixels: List of bad pixels.  If non-empty, annotate them on the image and show a zoomed view of each.
   */
  def write(al: AttributeList, title: String, dir: File, fileBaseName: String, bufferedImage: Option[BufferedImage], dicomImage: Option[DicomImage], badPixels: Seq[DicomImage.PixelRating]): Option[String] = {
    try {
      Util.mkdirs(dir)
      makePage(al, title, dir, fileBaseName, bufferedImage, dicomImage, badPixels)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while generating DICOM view for directory " + dir.getAbsolutePath + " : " + fmtEx(t))
        None
      }
    }
  }
}
