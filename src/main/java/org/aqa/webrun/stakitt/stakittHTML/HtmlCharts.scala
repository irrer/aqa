package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.web.WebUtil.ElemJS
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Logging

import scala.xml.Elem

/**
  * Construct the HTML charts for Stakitt.
  *
  * @param analysis Results of Stakitt analysis.
  */
case class HtmlCharts(analysis: Analysis) extends Logging {

  private val lineCharts = StakittLineCharts(analysis)

  private val barCharts = StakittBarChart.makeBarHtml(analysis)

  private val elem: Elem = {
    <div style="text-align: center;">
      {barCharts.elem}
      {lineCharts.html}
    </div>
  }

  /** When switching tabs, the C3 charts do not display correctly.  Doing periodic flushes fixes this. */
  private val flushJs: String = {

    val flushList = lineCharts.chartIdList.map(_ + ".flush();").mkString("\n    ")

    s"""
       |
       |// make sure that the charts are properly sized when switching tabs
       |function flushCharts() {
       |  setTimeout(() => {
       |    $flushList
       |    flushCharts();
       |  }, 1000);
       |}
       |
       |flushCharts();
       |
       |""".stripMargin.replaceAll("\r", "")
  }

  private val js = lineCharts.js + "\n" + barCharts.js + "\n" + flushJs

  /** HTML and JS for all charts. */
  val elemJs: ElemJS = ElemJS(elem, js)

}
