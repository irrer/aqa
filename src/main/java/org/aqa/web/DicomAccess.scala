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

/**
 * Create a web page to display a DICOM file.
 */
object DicomAccess extends Logging {

  private def makeTitle(attrList: Seq[AttributeList]): String = {
    val SeriesDescription = attrList.map(al => al.get(TagFromName.SeriesDescription)).distinct.mkString("   ")
    val Modality = attrList.map(al => al.get(TagFromName.Modality)).distinct.mkString("   ")
    Modality + " : " + SeriesDescription
  }

  // TODO instead of using javascript canvas, generate a PNG for each bad pixel.  Use bootstrap row+column to arrange them.  Put them in a sub-dir to confine clutter.
  private def makeImagePage(title: String, pngFile: File, outputDir: File, fileBaseName: String, image: BufferedImage, badPixelList: IndexedSeq[Point]): Elem = {

    val pngUrl = WebServer.urlOfResultsFile(pngFile)
    val imagePageFile = new File(outputDir, fileBaseName + "_image.html")
    val imagePageUrl = WebServer.urlOfResultsFile(imagePageFile)

    val mag = 25 // bad pixel magnification factor
    val regionSize = 5 // number of pixels in neighborhood to show

    val badPixPerRow = 5

    val text = """

            context.beginPath();
            context.rect(@x@, @y@, @mag@, @mag@);
            context.fillStyle = '@color@';
            context.fill();
            context.stroke();
            """.replaceAll("@mag@", mag.toString)

    def showBadPix(badPixIndex: Int): String = {

      val badPix = badPixelList(badPixIndex)
      val color = (image.getRGB(badPix.getX.toInt, badPix.getY.toInt) & 0xffffff).formatted("#%06x")

      val x = (badPixIndex % badPixPerRow) * (regionSize + 1) * mag
      val y = (badPixIndex / badPixPerRow) * (regionSize + 1) * mag

      text
        .replaceAll("@x@", x.toString)
        .replaceAll("@y@", y.toString)
        .replaceAll("@color@", color)
    }

    val imageContent = {

      val canvasWidth = (badPixPerRow + 1) * regionSize * mag
      val canvasHeight = ((badPixelList.size / badPixPerRow) + 1) * regionSize * mag

      <div>
        <a href={ imagePageUrl }><img src={ pngUrl }></img></a>
        <p></p>
        Put the bad pixel stuff here
        <canvas id='badPixelCanvas' width={ canvasWidth.toString } height={ canvasHeight.toString }></canvas>
        <script>
          var canvas = document.getElementById('badPixelCanvas');
          var context = canvas.getContext('2d');
          {
            badPixelList.indices.map(bpi => showBadPix(bpi))
          }
        </script>
      </div>
    }

    val mainImagePage = {
      <div class="col-md-10 col-md-offset-1">
        <h2 title="Image and bad pixels">{ title }</h2>
        { imageContent }
      </div>
    }

    val imagePageText = WebUtil.wrapBody(mainImagePage, title)
    val imagePageOutStream = new FileOutputStream(imagePageFile)
    imagePageOutStream.write(imagePageText.getBytes)
    imagePageOutStream.close

    <a href={ imagePageUrl }><img src={ pngUrl } width="256"></img></a>
  }

  private def makePage(dicom: DicomFile, urlOfFile: String, title: String, outputDir: File, image: Option[BufferedImage], badPixelList: IndexedSeq[Point]) = {

    val fileBaseName = Util.removeFileNameSuffix(dicom.file.getName)
    val downloadLink = { <a href={ WebServer.urlOfResultsFile(dicom.file) } title="Download as DICOM">Download</a> }
    val pngFile = new File(outputDir, fileBaseName + ".png")

    val content = if (dicom.attributeList.isDefined) {
      val al = dicom.attributeList.get
      val text = DicomUtil.attributeListToString(dicom.attributeList.get)

      {
        <div>
          { downloadLink }
          <br></br>
          {
            if (image.isDefined) makeImagePage(title, pngFile, outputDir, fileBaseName, image.get, badPixelList)
          }
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

    val htmlFile = new File(outputDir, fileBaseName + ".html")
    val htmlText = WebUtil.wrapBody(html, title)
    val htmlOutStream = new FileOutputStream(htmlFile)
    htmlOutStream.write(htmlText.getBytes)
    htmlOutStream.close

    badPixelList.map(bp => ImageUtil.annotatePixel(image.get, bp.getX, bp.getY, Color.yellow, bp.getX.toInt + ", " + bp.getY.toInt, true))
    val fos = new FileOutputStream(pngFile)
    ImageIO.write(image.get, "png", fos)

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
  def write(dicom: DicomFile, urlOfFile: String, title: String, dir: File, image: Option[BufferedImage], badPixelList: IndexedSeq[Point]): Unit = {
    try {
      dir.mkdirs
      makePage(dicom, urlOfFile, title, dir, image, badPixelList)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while generating DICOM view for directory " + dir.getAbsolutePath + " : " + fmtEx(t))
      }
    }
  }
}
