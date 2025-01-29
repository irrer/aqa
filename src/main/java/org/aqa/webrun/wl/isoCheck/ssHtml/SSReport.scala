package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._
import org.aqa.Util
import org.aqa.web.C3ScatterPlot
import org.aqa.web.C3ScatterPlotDataPoint
import org.aqa.web.C3ScatterPlotDataSet
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLIsoCheck

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  */
class SSReport(extendedData: ExtendedData, isoCheck: WLIsoCheck, isoTable: Option[WLIsoTable]) extends SSSheet {

  private val hasIt = isoTable.isDefined

  override val name: String = "Report"

  /** CSS styles for making cell borders  */
  private val border = "2px solid black"
  private val bL = Some(s"border-left: $border;")
  private val bLR = Some(s"border-left: $border;border-right: $border;")
  private val bTB = Some(s"border-Top: $border;border-bottom: $border;")
  private val bTBL = Some(s"border-Top: $border;border-left: $border;border-bottom: $border;")
  private val bTBLR = Some(s"border: $border;")

  private val MLCWobbleChart: C3ScatterPlot = {

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
      xMin = Some(-0.5),
      xMax = Some(0.5),
      yMin = Some(-0.5),
      yMax = Some(0.5),
      showPrecision = 10,
      showGrid = true
    )
  }

  private val IsoTableWobbleChart: C3ScatterPlot = {

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
      xMin = Some(-0.5),
      xMax = Some(0.5),
      yMin = Some(-0.5),
      yMax = Some(0.5),
      showPrecision = 10,
      showGrid = true
    )
  }

  private def makeRow1: Elem = {
    <tr>
      {makeRowIndex(1)}
      {toHtml("Date and Time") /*         A1 */}
      {toHtml("Machine") /*               B1 */}
      {toHtml("CBCT->", style = { bL }) /*C1 */}
      {toHtml("CBCT->") /*                D1 */}
      {toHtml("CBCT->") /*                E1 */}
      {toHtml("Table Axis->", style = { bL }) /*          F1 */}
      {toHtml("Table Axis->") /*          G1 */}
      {toHtml("Gantry flex", style = { bL }) /*           H1 */}
      {toHtml("Coll-Gantry-misalign", style = { bL }) /*  I1 */}
      {toHtml("MLC offset", style = { bL }) /*            J1 */}
      {toHtml("Table wobble", style = { bLR }) /*         K1 */}
      {blankCell /*                       L1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    <tr>
      {makeRowIndex(1)}
      {blankCells(2) /*               A2 to B2 */}
      {toHtml("Gantry Isocenter", style = { bL }) /*  C2 */}
      {toHtml("Gantry Isocenter") /*  D2 */}
      {toHtml("Gantry Isocenter") /*  E2 */}
      {toHtml("Gantry Isocenter", style = { bL }) /*  F2 */}
      {toHtml("Gantry Isocenter") /*  G2 */}
      {toHtml("", style = { bL }) /*                   H2 */}
      {toHtml("Gantry", style = { bL }) /*            I2 */}
      {toHtml("", style = { bL }) /*                   J2 */}
      {toHtml("diameter", style = { bLR }) /*          K2 */}
      {blankCell /*                   L2 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {makeRowIndex(2)}
      {blankCells(2) /*               A3 to B3 */}
      {toHtml("X (mm)", style = { bL }) /*            C3 */}
      {toHtml("Y (mm)") /*            D3 */}
      {toHtml("Z (mm)") /*            E3 */}
      {toHtml("X (mm)", style = { bL }) /*            F3 */}
      {toHtml("Z (mm)") /*            G3 */}
      {toHtml("(mm)", style = { bLR }) /*              H3 */}
      {toHtml("(mm)", style = { bLR }) /*              I3 */}
      {toHtml("(mm)", style = { bLR }) /*              J3 */}
      {toHtml("(mm)", style = { bLR }) /*              K3 */}
      {blankCell /*                   L3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    val dateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)
    <tr>
      {makeRowIndex(4)}
      {toHtml(dateText) /*                                           A4 */}
      {toHtml(extendedData.machine.getRealId) /*                     B4 */}
      {toHtmlYellow(0 - isoCheck.isoX, style = { bTBL }) /*                             C4 */}
      {toHtmlYellow(0 - isoCheck.isoY, style = { bTB }) /*                             D4 */}
      {toHtmlYellow(0 - isoCheck.isoZ, style = { bTB }) /*                             E4 */}
      {if (hasIt) toHtmlYellow(isoTable.get.get_IsoTable_X_Optimized - isoCheck.isoX, style = { bTBL }) else blankCell /*   F4 */}
      {if (hasIt) toHtmlYellow(isoTable.get.get_IsoTable_Z_Optimized - isoCheck.isoZ, style = { bTBL }) else blankCell /*   G4 */}
      {toHtmlYellow(isoCheck.gantryFlex, style = { bTBLR }) /*                           H4 */}
      {toHtmlYellow(isoCheck.collGantryMisalign, style = { bTBLR }) /*                   I4 */}
      {toHtmlYellow(isoCheck.mlcOffsetY, style = { bTBLR }) /*                           J4 */}
      {if (hasIt) toHtmlYellow(2 * Math.sqrt(isoTable.get.get_RSquared_Optimized), style = bTBLR) else blankCell /*  K4 */}
      {blankCell /*                                                  L2 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {blankCells(12) /* A5 to L5 */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6) /*                A4 to H4 */}
      <td colspan="5" style={s"border: $border;"}>
        <h4 style="text-align: center;">MLC Wobble about Collimator Axis</h4>
        {MLCWobbleChart.html}
      </td>
      <td colspan="6" style={s"border: $border;"}>
        <h4 style="text-align: center;">IsoTable Wobble about IsoTable Axis</h4>
        {IsoTableWobbleChart.html}
      </td>
      {blankCell}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCells(12) /* A7 to L7 */}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(12)}
        {makeRow1}
        {makeRow2}
        {makeRow3}
        {makeRow4}
        {makeRow5}
        {makeRow6}
        {makeRow7}
      </table>
    }

    content
  }

  val js: String = {
    s"""
       |<script>
       |
       |    ${MLCWobbleChart.javascript}
       |    
       |    ${IsoTableWobbleChart.javascript}
       | 
       | </script>""".stripMargin.replaceAll("\r", "")

  }
}
