package org.aqa.webrun.wl.nonCardinal

import org.aqa.webrun.wl.WLgenHtml
import org.aqa.Util
import org.aqa.webrun.wl.WLMessage

import java.io.File

case class WLNonCardinalHTML(analysis: WLNonCardAnalysis, wlMessage: Option[WLMessage]) {

  private def makeCloseupImage(): Unit = {
    val pngFile = new File(analysis.subDir, WLgenHtml.BRIGHT_SUMMARY_FILE_NAME)
    val bufImg = WLNonCardCompositeImage.makeCompositeImage(analysis)
    Util.writePng(bufImg, pngFile)
    wlMessage.foreach(_.info(s"Wrote file $pngFile"))
  }

  def generate(): Unit = {
    makeCloseupImage()
  }

}
