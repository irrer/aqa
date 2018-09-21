package org.aqa.web

import com.pixelmed.dicom.AttributeList
import java.io.File
import org.aqa.DicomFile
import org.aqa.Logging
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import scala.xml.Elem
import javax.imageio.ImageIO
import edu.umro.ScalaUtil.FileUtil
import java.io.ByteArrayOutputStream
import java.io.FileOutputStream
import edu.umro.ScalaUtil.Trace
import java.awt.Point
import java.awt.image.BufferedImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.Color
import org.aqa.Util
import org.aqa.db.BadPixel
import edu.umro.ImageUtil.DicomImage
import java.awt.BasicStroke
import edu.umro.ImageUtil.ImageText
import slick.lifted.Constraint

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

  def badPixelToPng(badPixel: Point, dicomImage: DicomImage, fileBaseName: String, bufferedImage: BufferedImage, badPixelDir: File): File = {
    val x = badPixel.getX.round.toInt
    val y = badPixel.getY.round.toInt

    // create a buffered image and set it all to white
    val imgSize = (pixelSize + 1) * ((BadPixel.radius * 2) + 1) + 1
    val bufImg = new BufferedImage(imgSize, imgSize, BufferedImage.TYPE_INT_RGB)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(Color.white)
    graphics.fillRect(0, 0, imgSize, imgSize)

    def fmtPoint(bufX: Int, bufY: Int): Unit = {
      val xi = (bufX + BadPixel.radius) * (pixelSize + 1)
      val yi = (bufY + BadPixel.radius) * (pixelSize + 1)

      val xx = x + bufX
      val yy = y + bufY
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

    val fileName = fileBaseName + "_" + x.toString + "-" + y.toString + ".png"
    val pngFile = new File(badPixelDir, fileName)
    Util.writePng(bufImg, pngFile)
    // TODO rm ImageIO.write(bufImg, "png", pngFile)
    pngFile
  }

  private def badPixelToHtml(badPixel: Point, dicomImage: DicomImage, fileBaseName: String, bufferedImage: BufferedImage, badPixelDir: File): Elem = {
    val pngFile = badPixelToPng(badPixel, dicomImage, fileBaseName, bufferedImage, badPixelDir)

    val x = badPixel.getX.round.toInt
    val y = badPixel.getY.round.toInt

    <div class="col-md-2" style="margin:8px;">
      <center>
        <h4 title="Pixel coordinates">{ x.toString + ", " + y.toString }</h4>
      </center>
      <img src={ WebServer.urlOfResultsFile(pngFile) }/>
    </div>
  }

  private def makeImagePage(title: String, pngFile: File, outputDir: File, fileBaseName: String, dicomImage: DicomImage, bufferedImage: BufferedImage, badPixelList: IndexedSeq[Point]): Elem = {

    val pngUrl = WebServer.urlOfResultsFile(pngFile)
    val imagePageFile = new File(outputDir, fileBaseName + "_image.html")
    val imagePageUrl = WebServer.urlOfResultsFile(imagePageFile)

    val badPixelDir = new File(outputDir, badPixelDirName)
    badPixelDir.mkdirs

    val badPixelVisualizationList = badPixelList.map(badPixel => badPixelToHtml(badPixel, dicomImage, fileBaseName, bufferedImage, badPixelDir))
    val rowList = (0 until ((badPixelVisualizationList.size + pixRow - 1) / pixRow)).map(row => badPixelVisualizationList.drop(row * pixRow).take(pixRow))

    def rowToHtml(row: IndexedSeq[Elem]): Elem = {
      { <div class="row">{ row }</div> }
    }
    val allRows = rowList.map(row => rowToHtml(row)).toList

    val imageContent = {
      <div>
        <a href={ imagePageUrl }><img src={ pngUrl }/></a>
        <p/>
        <h2 title="Zoomed view of bad pixels (centered in each) and raw values with immediate neighbors.">Bad Pixels : { badPixelList.size }</h2>
        <p/>
        { allRows }
      </div>
    }

    val mainImagePage = {
      <div class="col-md-10 col-md-offset-1">
        <h2 title="Image with bad pixels annotated">{ title }</h2>
        { imageContent }
      </div>
    }

    val imagePageText = WebUtil.wrapBody(mainImagePage, title)
    val imagePageOutStream = new FileOutputStream(imagePageFile)
    imagePageOutStream.write(imagePageText.getBytes)
    imagePageOutStream.close

    <a href={ imagePageUrl }><img src={ pngUrl } width="256"></img></a>
  }

  private def makePage(dicom: DicomFile, urlOfFile: String, title: String,
    dir: File, fileBaseName: String, bufferedImage: Option[BufferedImage],
    dicomImage: Option[DicomImage], badPixelList: IndexedSeq[Point]): Option[String] = {

    val downloadLink = { <a href={ WebServer.urlOfResultsFile(dicom.file) } title="Download as DICOM">Download</a> }
    val pngFile = new File(dir, fileBaseName + ".png")

    val content = if (dicom.attributeList.isDefined) {
      val al = dicom.attributeList.get
      val text = DicomUtil.attributeListToString(dicom.attributeList.get)

      {
        <div>
          { downloadLink }
          <br></br>
          { if (bufferedImage.isDefined) makeImagePage(title, pngFile, dir, fileBaseName, dicomImage.get, bufferedImage.get, badPixelList) }
          <p>
            <pre title="DICOM meta-data">{ WebUtil.nl + text }</pre>
          </p>
        </div>
      }
    } else { downloadLink }

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
      badPixelList.map(bp => ImageUtil.annotatePixel(bufferedImage.get, bp.getX, bp.getY, bp.getX.toInt + ", " + bp.getY.toInt, true))
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
   * @param badPixelList: List of bad pixels.  If non-empty, annotate them on the image and show a zoomed view of each.
   */
  def write(dicom: DicomFile, urlOfFile: String, title: String, dir: File, fileBaseName: String, bufferedImage: Option[BufferedImage], dicomImage: Option[DicomImage], badPixelList: IndexedSeq[Point]): Option[String] = {
    try {
      dir.mkdirs
      makePage(dicom, urlOfFile, title, dir, fileBaseName, bufferedImage, dicomImage, badPixelList)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while generating DICOM view for directory " + dir.getAbsolutePath + " : " + fmtEx(t))
        None
      }
    }
  }
}
