package org.aqa.webrun.stakitt.stakittHTML

import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.web.C3Chart
import org.aqa.web.WebUtil.ElemJS
import org.aqa.webrun.stakitt.Analysis
import org.aqa.Logging
import org.aqa.Util

import scala.xml.Elem

/**
  * Construct the HTML charts for Stakitt.
  *
  * @param analysis Results of Stakitt analysis.
  */
case class HtmlCharts(analysis: Analysis) extends Logging {

  /**
    * Format a Double.  If it is a rounded value, then format it as an Int.
    * @param d Format this.
    * @return Text version.
    */
  private def fmt(d: Double) = {
    if (d.round == d)
      d.round.toString
    else
      Util.fmtDbl(d)
  }

  private val histogramChart = {

    val offsetList = analysis.stakittList.map(_.stakitt.leafEndOffset_mm)

    val minOffset = offsetList.min
    val maxOffset = offsetList.max
    val numberOfBins = ((maxOffset - minOffset) * 10).round.toInt + 1

    val offsetToBinA = (numberOfBins - 1) / (maxOffset - minOffset)
    val offsetToBinB = -(offsetToBinA * minOffset)

    def offsetToBinIndex(offset: Double): Int = {
      val index = Math.clamp(((offset * offsetToBinA) + offsetToBinB).round.toInt, 0, numberOfBins - 1)
      val binIndex = Math.clamp(index, 0, numberOfBins - 1)
      binIndex
    }

    val binnedList = offsetList.groupBy(offsetToBinIndex).values.map(_.size)
    val yValues = Seq(binnedList.toSeq.map(_.toDouble))

    val binToOffsetA = (maxOffset - minOffset) / (binnedList.size - 1)
    val binToOffsetB = minOffset
    def binIndexToOffset(binIndex: Int): Double = {
      val offset = (binIndex * binToOffsetA) + binToOffsetB
      val roundedOffset = (offset * 10).round.toDouble / 10.0
      roundedOffset
    }

    val xValueList = yValues.head.indices.map(binIndexToOffset)

    val chart = new C3Chart(
      width = None,
      height = None,
      xAxisLabel = "Offset mm",
      xDataLabel = "",
      xValueList = xValueList,
      xFormat = ".1g",
      yAxisLabels = Seq("Count"),
      yDataLabel = "Offset (Measured-Planned) mm",
      yValues = yValues,
      yFormat = ".3i",
      yColorList = Seq(),
      regionList = Seq(),
      chartType = "bar"
    )

    chart
  }

  /**
    * Chart comparing offsets horizontally.
    */
  private val horizontalChart = {
    logger.info("Generating Stakitt horizontalChart")
    val xValueList = analysis.resultColumns.map(_.head.stakitt.plannedEndPosition_mm)

    // xIndexList.map(xIndex => analysis.stakittList.find(r => r.stakittAOI.xIndex == xIndex).get).map(r => r.stakitt.plannedEndPosition_mm)
    val yAxisLabels = analysis.resultRows.indices.map(yIndex => (yIndex + 1).toString)

    val yValues = analysis.resultRows.map(_.map(_.stakitt.leafEndOffset_mm))

    val chart = new C3Chart(
      width = None,
      height = None,
      xAxisLabel = "mm",
      xDataLabel = "X Planned mm",
      xValueList = xValueList,
      xFormat = ".4g",
      yAxisLabels = yAxisLabels,
      yDataLabel = "Measured-Planned mm",
      yValues = yValues,
      yFormat = ".2g",
      yColorList = Seq(),
      regionList = Seq()
    )

    chart
  }

  /**
    * Chart comparing offsets vertically.
    */
  private val verticalChart = {
    logger.info("Generating Stakitt verticalChart")

    val yAxisLabels: Seq[String] = analysis.resultColumns.map(_.head.stakitt.plannedEndPosition_mm).map(fmt)

    val yValues = analysis.resultColumns.map(row => row.map(r => r.stakitt.leafEndOffset_mm))

    val chart = new C3Chart(
      width = None,
      height = None,
      xAxisLabel = "mm",
      xDataLabel = "leaf",
      xValueList = analysis.resultRows.indices.map(_ + 1.0),
      xFormat = ".4g",
      yAxisLabels = yAxisLabels,
      yDataLabel = "Measured-Planned mm",
      yValues = yValues,
      yFormat = ".2g",
      yColorList = Seq(),
      regionList = Seq()
    )

    chart
  }

  /**
    * Chart showing differences in leaf boundary measured and planned (measured - planned).
    */
  private val leafBoundaryChart = {
    logger.info("Generating Stakitt leaf boundary (sides) chart.")

    val trans = new IsoImagePlaneTranslator(analysis.rtimage)

    val yValues = {
      val measured = {
        analysis.yLeafBoundaries.yPointListLo_pix.measured_pix
          .zip(analysis.yLeafBoundaries.yPointListHi_pix.measured_pix)
          .map(lh => trans.pix2IsoCoordY((lh._1 + lh._2) / 2))
      }
      val diff = measured.zip(analysis.planBorders.yLeafBoundaryList).map(mp => mp._1 - mp._2)
      Seq(diff)
    }

    val chart = new C3Chart(
      width = None,
      height = None,
      xAxisLabel = "mm",
      xDataLabel = "leaf",
      xValueList = analysis.resultRows.indices.map(_ + 1.0),
      xFormat = ".4g",
      yAxisLabels = Seq("Leaf"),
      yDataLabel = "mm",
      yValues = yValues,
      yFormat = ".2g",
      yColorList = Seq(),
      regionList = Seq()
    )

    chart
  }

  private val elem: Elem = {
    val histogramTitle = s"Histogram of Offsets"
    val horzTitle = s"Horizontally (${analysis.resultColumns.size} x ${analysis.resultRows.size}) Sorted Leaf Offsets (mm)"
    val vertTitle = s"Vertically (${analysis.resultRows.size} x ${analysis.resultColumns.size}) Sorted Leaf Offsets (mm)"
    val leafBoundaryTitle = s"Leaf Boundary Offsets Measured - Planned (mm)"
    <div style="text-align: center;">
      <h3 style="margin-top:24px;">{histogramTitle}</h3>
      {histogramChart.html}
      <h3 style="margin-top:24px;">{horzTitle}</h3>
      {horizontalChart.html}
      <h3 style="margin-top:24px;">{vertTitle}</h3>
      {verticalChart.html}
      <h3 style="margin-top:24px;">{leafBoundaryTitle}</h3>
      {leafBoundaryChart.html}
    </div>
  }

  private val flushJs: String =
    s"""
       |
       |// make sure that the charts are properly sized when switching tabs
       |function flushCharts() {
       |  setTimeout(() => {
       |     ${histogramChart.chartIdTag}.flush();
       |     ${horizontalChart.chartIdTag}.flush();
       |     ${verticalChart.chartIdTag}.flush();
       |     ${leafBoundaryChart.chartIdTag}.flush();
       |     flushCharts();
       |  }, 1000);
       |}
       |
       |flushCharts();
       |
       |""".stripMargin

  private val js = Seq(histogramChart, horizontalChart, verticalChart, leafBoundaryChart).map(_.javascript).mkString("\n") + flushJs

  /** HTML and JS for all charts. */
  val elemJs: ElemJS = ElemJS(elem, js)

}
