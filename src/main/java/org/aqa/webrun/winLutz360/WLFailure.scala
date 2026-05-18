package org.aqa.webrun.winLutz360

import edu.umro.ImageUtil.DicomImage
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLMessage
import org.aqa.Logging
import org.aqa.webrun.wl.WLgenHtml
import org.aqa.Util
import org.aqa.web.WebUtil

import java.io.File
import scala.xml.Elem

/**
  * Container for the information necessary to report an unexpected error to the user.
  *
  * @param extendedData Metadata
  * @param wlMessage Error log and image.
  * */

case class WLFailure(extendedData: ExtendedData, wlMessage: WLMessage) extends Logging {

  /**
    * Get the subdirectory that this should reside in.
    * @return
    */
  def subDir(): File = {
    val subDirName = wlMessage.runReq.subDirName(wlMessage.rtimage)
    new File(extendedData.output.dir, subDirName)
  }

  /**
   * Make an image file of the full image for diagnostic purposes.
   */
  private def makePng(): Unit = {
    val pngFile = new File(subDir(), WLgenHtml.ORIGINAL_FILE_NAME)
    val dicomImage = new DicomImage(wlMessage.rtimage)
    val bufImg = dicomImage.toDeepColorBufferedImage(0.001)
    Util.writePng(bufImg, pngFile)
  }

  /**
   * Convert the captured diagnostics (log messages) into HTML.
   * @return HTML
   */
  private def makeDiagnosticsContent(): Elem = {
    <div style="margin:20px;">
      <h3>Internal Failure During WL Analysis : Unexpected Exception</h3>
      <pre>
        {"\n\n" + wlMessage.toString}
      </pre>
    </div>
  }

  /**
   * Write the HTML content.
   */
  private def writeDiagnostics(): Unit = {
    val content = makeDiagnosticsContent()
    val text = WebUtil.wrapBody(ExtendedData.wrapExtendedData(extendedData, content), s"${wlMessage.imageName} Diagnostics")
    val file = new File(subDir(), WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME)
    Util.writeFile(file, text)
  }

  /**
   * Generate a web page to help diagnose a WL360 failure.
   *
   * This is wrapped in a try-catch to avoid exception on top of exception complications.
   */
  def makeFailureDiagnostics(): Unit = {

    // wrap this in a try+catch in case there is another exception
    try {
      val dir = subDir()
      dir.mkdirs()

      makePng()

      writeDiagnostics()

    } catch {
      case t: Throwable =>
        wlMessage.error(s"Unexpected exception while creating failure diagnostics: ${fmtEx(t)}")
    }

  }
}
