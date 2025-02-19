package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.web.C3ScatterPlot
import org.aqa.web.C3ScatterPlotDataPoint
import org.aqa.web.C3ScatterPlotDataSet
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLIsoTable

object TableWobbleChart {

  def makeChart(isoTable: Option[WLIsoTable]): C3ScatterPlot = {

    val hasIt = isoTable.isDefined

    def dXOf(beam: WLBeam): Option[Double] = {
      isoTable.map(it => it.BB_Xp(beam, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized) - it.get_IsoTable_X_Optimized)
    }

    def dZOf(beam: WLBeam): Option[Double] = {
      isoTable.map(it => it.BB_Zpp(beam, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized, it.get_IsoTable_Z_Optimized))
    }

    def point(beam: WLBeam): Option[C3ScatterPlotDataPoint] = {
      if (hasIt)
        Some(C3ScatterPlotDataPoint(dXOf(beam).get, dZOf(beam).get))
      else None
    }

    val dataList: Seq[C3ScatterPlotDataSet] = {
      if (hasIt) {
        val data = isoTable.get.beamList.map(point)
        Seq(C3ScatterPlotDataSet("Table Wobble", data.flatten))
      } else
        Seq()
    }

    new C3ScatterPlot(
      dataList = dataList,
      xAxisLabel = "BB-X`` (mm)",
      yAxisLabel = "BB-Z`` (mm)",
      width = Some(600),
      height = Some(686),
      xAxisFormat = ".1g",
      yAxisFormat = ".1g",
      xMin = Some(-1.1),
      xMax = Some(1.1),
      yMin = Some(-0.9),
      yMax = Some(0.9),
      showPrecision = 10,
      showGrid = true
    )
  }

}
