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

  private val beamName = {
    val n1 = { if (analysis.beamName.isDefined) analysis.beamName.get + " " else "" }.trim
    val n2 = { if (wlMessage.isDefined) " " + wlMessage.get.imageName else "" }.trim
    (n1 + " " + n2).trim
  }

  private def dicomAsText(): Elem = {
    val file = new File(analysis.subDir, "dicom.html")

    val content = {

      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>DICOM for {beamName}</h2>
          <pre style="background: #eeeeee; font-size: small">
            {DicomUtil.attributeListToString(analysis.al)}
          </pre>
        </div>
      </div>
    }

    val text = WebUtil.wrapBody( //
      content = ExtendedData.wrapExtendedData(analysis.extendedData, content),
      pageTitle = beamName,
      runScript = None
    )

    Util.writeFile(file, text)
    logger.info(s"Wrote DICOM metadata file ${file.getAbsolutePath}")

    <a href={file.getName}>DICOM Metadata</a>
  }

  private def makeDiagnosticsHtml(): Unit = {

    val coarseEdgeImageUrl = {
      val file = new File(analysis.subDir, "coarseEdge.png")
      Util.writePng(analysis.approxImg, file)
      file.getName
    }

    val fineEdgeImageUrl = {
      val file = new File(analysis.subDir, "fineEdge.png")
      Util.writePng(analysis.img, file)
      file.getName
    }

    val content = {
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <h2>Details for Beam {beamName}</h2>
          {dicomAsText()}
          {showWlMessage()}
        </div>
        <div>
          <h3>Coarse Edge</h3>
          <img src={coarseEdgeImageUrl}/>
        </div>
        <div>
          <h3>Fine Edge</h3>
          <img src={fineEdgeImageUrl}/>
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
