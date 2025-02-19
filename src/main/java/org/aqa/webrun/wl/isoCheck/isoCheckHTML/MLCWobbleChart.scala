package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ScatterPlot
import org.aqa.web.C3ScatterPlotDataPoint
import org.aqa.web.C3ScatterPlotDataSet
import org.aqa.webrun.wl.isoCheck.WLIsoCheck

object MLCWobbleChart {
  def makeChart(isoCheck: WLIsoCheck): C3ScatterPlot = {

    val dataList: Seq[C3ScatterPlotDataSet] = {
      val data = Seq(
        C3ScatterPlotDataPoint(isoCheck.mlcDxG__0_C_90, isoCheck.mlcDyG__0_C_90),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG__0_C270, isoCheck.mlcDyG__0_C270),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG_90_C_90, isoCheck.mlcDyG_90_C_90),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG_90_C270, isoCheck.mlcDyG_90_C270),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG180_C__0, isoCheck.mlcDyG180_C__0),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG180_C_90, isoCheck.mlcDyG180_C_90),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG180_C270, isoCheck.mlcDyG180_C270),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG270_C_90, isoCheck.mlcDyG270_C_90),
        C3ScatterPlotDataPoint(isoCheck.mlcDxG270_C270, isoCheck.mlcDyG270_C270)
      )

      Seq(C3ScatterPlotDataSet("MLC Wobble", data))
    }

    new C3ScatterPlot(
      dataList = dataList,
      xAxisLabel = "dZ (mm)",
      yAxisLabel = "dX (mm)",
      width = Some(600),
      height = Some(686),
      xAxisFormat = ".1g",
      yAxisFormat = ".1g",
      xMin = Some(-0.9),
      xMax = Some(0.9),
      yMin = Some(-0.9),
      yMax = Some(0.9),
      showPrecision = 10,
      showGrid = true
    )
  }
}
