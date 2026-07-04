package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.WebUtil
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Util
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil.ElemJS
import org.aqa.webrun.stakitt.MakeImage
import org.aqa.Logging

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
    WebUtil.makeZoom(url, width = "auto", "Annotated Stakitt image.")
  }

  private def twoDimensionalRepresentations: Elem = {

    val imageId = C3Chart.makeUniqueChartIdTag
    val tableId = C3Chart.makeUniqueChartIdTag

    val elem =
      <div>
        <ul class="nav nav-tabs">
          <li class="active" style="">
            <a data-toggle="tab" href={s"#$imageId"} style="text-align:center;"> Image </a>
          </li>
          <li class="" style="">
            <a data-toggle="tab" href={s"#$tableId"} style="text-align:center;"> Table </a>
          </li>
        </ul>

        <div class="tab-content" style="margin-right:20px;">
          <div id={imageId} class="tab-pane fade in active">
            {image()}
          </div>

          <div id={tableId} class="tab-pane fade in">
            {HtmlTable(analysis, dir).mainTable()}
          </div>
        </div>

    </div>
    elem
  }

  // private val charts: HtmlCharts = HtmlCharts(analysis)

  private val barCharts = StakittBarChart.makeBarHtml(analysis)
  private val lineCharts = StakittLineCharts(analysis)

  private val elem: Elem = {
    val cls =
      if (index == 0)
        "tab-pane fade in active"
      else
        "tab-pane fade"

    <div id={id} class={cls}>
      <div class="row">
        <div class="col-md-5">
          {barCharts.elem}
        </div>
        <div class="col-md-7">
          <div style="margin-left:5px;">
            <div class="row">
              {HtmlStats(analysis).elem}
            </div>
            <div class="row">
              {twoDimensionalRepresentations}
            </div>
          </div>
        </div>
      </div>
      <div class="row">
        {lineCharts.html}
      </div>
    </div>
  }

  private val js = Seq(lineCharts.js, barCharts.js).mkString("\n")

  val elemJs: ElemJS = ElemJS(elem, js)
}
