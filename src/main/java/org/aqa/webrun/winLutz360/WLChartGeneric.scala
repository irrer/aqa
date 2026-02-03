package org.aqa.webrun.winLutz360

import org.aqa.web.C3ChartHistory

/**
 * Support both Winston Lutz and WinLutz360 charts.
 */
trait WLChartGeneric {
  val beamNameList: Seq[String]

  val chartList: Seq[C3ChartHistory]
}
