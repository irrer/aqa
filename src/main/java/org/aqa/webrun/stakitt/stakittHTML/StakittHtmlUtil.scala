package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.Logging
import org.aqa.web.C3Chart

object StakittHtmlUtil extends Logging {

  /**
    * Make a JS function that flushes (refreshes) a list of charts.
    *
    * When switching tabs, the C3 charts do not display correctly.  Doing periodic flushes fixes this.
    *
    * @param idList List of chart ids.
    * @return JS function.
    */
  def makeFlushJs(idList: Seq[String]): String = {

    val functionName = "FlushChart" + C3Chart.makeUniqueChartIdTag

    val flushList = idList.map(_ + ".flush();").mkString("\n    ")

    s"""
       |
       |// make sure that the charts are properly sized when switching tabs
       |function $functionName() {
       |  setTimeout(() => {
       |    $flushList
       |    $functionName();
       |  }, 1000);
       |}
       |
       |$functionName();
       |
       |""".stripMargin.replaceAll("\r", "")
  }
}
