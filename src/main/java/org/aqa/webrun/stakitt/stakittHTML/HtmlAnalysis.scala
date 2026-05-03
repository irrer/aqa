package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Util
import org.aqa.webrun.stakitt.MakeImage
import org.aqa.Logging
import org.aqa.web.WebUtil.ElemJS

import java.io.File
import scala.xml.Elem

/**
  * Make the content for the results of one set of Stakitt results.
  * @param analysis results of analysis.
  * @param dir destination directory.
  * @param id Tab ID
  * @param index Tab index.  0 is the first one and is set to active.
  */

case class HtmlAnalysis(analysis: Analysis, dir: File, id: String, index: Int) extends Logging {

  private val annotatedImageName = "annotatedImage.png"

  private def image(): Elem = {
    val bufImg = MakeImage.makeImage(analysis)
    val file = new File(dir, annotatedImageName)
    Util.writePng(bufImg, file)
    val url = dir.getName + "/" + file.getName
    WebUtil.makeZoom(url, 512, "Annotated Stakitt image.")
  }

  private val mainTable = HtmlTable(analysis).mainTable()

  private val charts: HtmlCharts = HtmlCharts(analysis)

  private val elem: Elem = {
    val cls =
      if (index == 0)
        "tab-pane fade in active"
      else
        "tab-pane fade"
    <div id={id} class={cls}>
      <div class="col-md-5">
        {image()}
        {charts.elemJs.elem}
      </div>
      <div class="col-md-7">
        {mainTable}
      </div>
    </div>
  }

  val elemJs: ElemJS = ElemJS(elem, charts.elemJs.js)
}
