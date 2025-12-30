package org.aqa.webrun.wl.nonCardinal

import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.wl.WLgenHtml
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLMessage
import org.aqa.Logging

import java.io.File
import scala.xml.Elem

case class WLNonCardinalHTML(analysis: WLNonCardAnalysis, wlMessage: Option[WLMessage]) extends Logging {

  private def makeCloseupImage(): Unit = {
    val pngFile = new File(analysis.subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
    val bufImg = WLNonCardCompositeImage.makeCompositeImage(analysis)
    Util.writePng(bufImg, pngFile)
    wlMessage.foreach(_.info(s"Wrote file $pngFile"))
  }

  private def showWlMessage(): Elem = {
    if (wlMessage.isDefined) {
      <pre style="background: #eeeeee; font-size: small">
        {wlMessage.get.toString}
      </pre>
    } else {
      <span></span>
    }
  }

  private def dicomAsText(): Elem = {
    val file = new File(analysis.subDir, "dicom.html")

    val beamText = {
      if (analysis.beamName.isDefined)
        s"Beam {analysis.beamName} DICOM Metadata"
      else
        s"DICOM Metadata"
    }

    val content = {

      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>{beamText}</h2>
          <pre style="background: #eeeeee; font-size: small">
            {DicomUtil.attributeListToString(analysis.al)}
          </pre>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = beamText,
      runScript = None
    )

    Util.writeFile(file, text)
    logger.info(s"Wrote DICOM metadata file ${file.getAbsolutePath}")

    <a href={file.getName}>DICOM Metadata</a>
  }

  private def makeDiagnosticsHtml(): Unit = {

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>Details for Beam {analysis.beamName}</h2>
          {dicomAsText()}
          {showWlMessage()}
        </div>
        <div>
         <h1>More stuff to come. { /* TODO */ }</h1>
        </div>
      </div>
    }

    val runScript = "" // TODO  zoom needed?

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = s"Beam ${analysis.beamName}",
      c3 = true,
      runScript = Some(runScript)
    )

    val htmlFile = new File(analysis.subDir, WLgenHtml.DIAGNOSTICS_HTML_FILE_NAME)

    Util.writeFile(htmlFile, text)
  }

  def generate(): Unit = {
    makeCloseupImage()

    makeDiagnosticsHtml()
  }

}
