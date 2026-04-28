package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Util
import org.aqa.webrun.stakitt.MakeImage

import java.io.File
import scala.xml.Elem

case class HtmlAnalysis(analysis: Analysis, dir: File, id: String, index: Int) {

  private val annotatedImageName = "annotatedImage.png"

  private def image(): Elem = {
    val bufImg = MakeImage.makeImage(analysis)
    val file = new File(dir, annotatedImageName)
    Util.writePng(bufImg, file)
    val url = dir.getName + "/" + file.getName
    WebUtil.makeZoom(url, 768, "Annotated Stakitt image.")
  }

  def makeHtml(): Elem = {
    val cls = if (index == 0)
      "tab-pane fade in active"
      else
        "tab-pane fade"
    <div id={id} class={cls}>
      <h4>The id:{id}</h4>
      <p> </p>
      {image()}
    </div>
  }
}
