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

/**
 * Create a web page to display a DICOM file.
 */
object DicomAccess extends Logging {

  private def makeTitle(attrList: Seq[AttributeList]): String = {
    val SeriesDescription = attrList.map(al => al.get(TagFromName.SeriesDescription)).distinct.mkString("   ")
    val Modality = attrList.map(al => al.get(TagFromName.Modality)).distinct.mkString("   ")
    Modality + " : " + SeriesDescription
  }

  private def makePage(dicom: DicomFile, urlOfFile: String, title: String, dir: File, image: Option[BufferedImage], badPixelList: IndexedSeq[Point]) = {

    val fileBaseName = FileUtil.replaceInvalidFileNameCharacters(title, '_').replace(' ', '_')

    val downloadLink = { <a href={ urlOfFile } title="Download as DICOM">Download</a> }

    if (image.isDefined) badPixelList.map(bp => ImageUtil.annotatePixel(image.get, bp.getX, bp.getY, Color.yellow, bp.getX + ", " + bp.getY, true))

    val content = if (dicom.attributeList.isDefined) {
      val al = dicom.attributeList.get
      val text = DicomUtil.attributeListToString(dicom.attributeList.get)

      {
        <div>
          { downloadLink }
          <br></br>
          {
            if (image.isDefined) {
              val pngFile = new File(dir, fileBaseName + ".png")
              val fos = new FileOutputStream(pngFile)
              ImageIO.write(image.get, "png", fos)
            }
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

    val htmlFile = new File(dir, fileBaseName + ".html")
    val htmlText = WebUtil.wrapBody(html, title)
    val htmlOutStream = new FileOutputStream(htmlFile)
    htmlOutStream.write(htmlText.getBytes)
    htmlOutStream.close
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
