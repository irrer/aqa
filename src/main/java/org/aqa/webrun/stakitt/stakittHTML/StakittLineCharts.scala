package org.aqa.webrun.stakitt.stakittHTML

import org.aqa.webrun.stakitt.Analysis
import org.aqa.Logging
import org.aqa.web.C3Chart
import org.aqa.Util
import org.aqa.webrun.stakitt.StakittGap

import java.awt.Color
import scala.xml.Elem

/**
  * Generate the HTML for the Stakitt line charts.  These show the errors plotted vertically and horizontally.
  *
  * @param analysis Results of Stakitt analysis
  */
case class StakittLineCharts(analysis: Analysis) extends Logging {

  /**
    * Format a Double.  If it is a rounded value, then format it as an Int.
    *
    * @param d Format this.
    * @return Text version.
    */
  private def fmt(d: Double) = {
    if (d.round == d)
      d.round.toString
    else
      Util.fmtDbl(d)
  }

  // list of colors that are easily differentiated visually
  private val yColorList = Seq(Color.RED, Color.GREEN, Color.BLACK, Color.ORANGE, Color.BLUE, Color.YELLOW, Color.MAGENTA, Color.GRAY, Color.CYAN, Color.PINK)

  /**
    * Chart comparing offsets horizontally.
    */
  private val horizontalLeafChart = {
    logger.info("Generating Stakitt horizontalLeafChart")
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
      yColorList = yColorList,
      regionList = Seq()
    )

    chart
  }

  /**
    * Chart comparing offsets vertically.
    */
  private val verticalLeafChart = {
    logger.info("Generating Stakitt verticalLeafChart")

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
      yColorList = yColorList,
      regionList = Seq()
    )

    chart
  }

  private val rightBoldArrow = "=>" // "&#x2794;"

  /**
    * Chart comparing offsets horizontally.
    */
  private val horizontalGapChart = {
    logger.info("Generating Stakitt horizontalGapChart")
    val xValueList = analysis.resultColumns.map(_.head.stakitt.plannedEndPosition_mm)

    /**
      * Make a header that shows the leaf range
      * @param gap Gap between opposing leaves.
      * @return Description of gaps for that column.
      */
    def toHeader(gap: StakittGap): String = {
      val x1 = Util.fmtDbl(gap.x1.stakitt.plannedEndPosition_mm)
      val x2 = Util.fmtDbl(gap.x2.stakitt.plannedEndPosition_mm)
      val arrow = rightBoldArrow
      s"$x1 $arrow $x2 mm"

      val top = Util.fmtDbl(gap.x1.stakitt.plannedMinorSide_mm)
      val bottom = Util.fmtDbl(gap.x1.stakitt.plannedMajorSide_mm)
      s"$top $arrow $bottom"
    }

    // val yAxisLabels: Seq[String] = analysis.gapRows.map(_.head).map(toHeader)
    val yAxisLabels = analysis.resultRows.indices.map(yIndex => (yIndex + 1).toString)

    val yValues = analysis.gapRows.map(col => col.map(_.error))

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
      yColorList = yColorList,
      regionList = Seq()
    )

    chart
  }

  /**
    * Chart comparing offsets vertically.
    */
  private val verticalGapChart = {
    logger.info("Generating Stakitt verticalGapChart")

    /**
      * Make a header that shows the leaf range
      * @param gap Gap between opposing leaves.
      * @return Description of gaps for that column.
      */
    def toHeader(gap: StakittGap): String = {
      val y1 = Util.fmtDbl(gap.x1.stakitt.plannedEndPosition_mm)
      val y2 = Util.fmtDbl(gap.x1.stakitt.plannedEndPosition_mm)
      val arrow = rightBoldArrow
      s"$y1 $arrow $y2 mm"
    }

    val yAxisLabels: Seq[String] = analysis.gapColumns.map(_.head).map(toHeader)

    val yValues = analysis.gapColumns.map(row => row.map(_.error))
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
      yColorList = yColorList,
      regionList = Seq()
    )

    chart
  }

  /**
    * Chart showing differences in leaf boundary measured and planned (measured - planned).
    */
  /*
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
   */

  val html: Elem = {

    def horz(name: String, colSize: Int): String = s"Horizontally (${analysis.resultColumns.size} x ${colSize}) Oriented $name (mm)"
    def vert(name: String, colSize: Int): String = s"Vertically (${colSize} x ${analysis.resultColumns.size}) Oriented $name (mm)"

    val sty = "margin-top:24px; margin-left:5px; border:1px solid lightgrey;"

    <div style="text-align: center;">
      <div class="row">
        <div class="col-md-6">
          <div style={sty}>
            <h3> {horz("Leaf Offsets", analysis.resultRows.size)} </h3>
            {horizontalLeafChart.html}
          </div>
        </div>

        <div class="col-md-6">
          <div style={sty}>
            <h3> {vert("Leaf Offsets", analysis.resultRows.size)} </h3>
            {verticalLeafChart.html}
          </div>
        </div>
      </div>
      <div class="row">
        <div class="col-md-6">
          <div style={sty}>
            <h3> {horz("Gap Errors", analysis.gapColumns.size)} </h3>
            {horizontalGapChart.html}
          </div>
        </div>

        <div class="col-md-6">
          <div style={sty}>
            <h3> {vert("Gap Errors", analysis.gapColumns.size)} </h3>
            {verticalGapChart.html}
          </div>
        </div>

      </div>
    </div>
  }

  /*

      <div style="margin-top:24px;">
        <h3> {leafBoundaryTitle} </h3>
        {leafBoundaryChart.html}
      </div>

   */

  private val chartList: Seq[C3Chart] = Seq(horizontalLeafChart, verticalLeafChart, horizontalGapChart, verticalGapChart /* , leafBoundaryChart */ )

  private val chartIdList: Seq[String] = chartList.map(_.chartIdTag)

  private val flush = StakittHtmlUtil.makeFlushJs(chartIdList)

  val js: String = chartList.map(_.javascript).mkString("\n") + "\n" + flush

}
