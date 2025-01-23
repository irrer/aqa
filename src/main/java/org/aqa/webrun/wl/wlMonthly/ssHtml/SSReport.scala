package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLTable
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil._
import org.aqa.Util
import org.aqa.web.C3ScatterPlot
import org.aqa.web.C3ScatterPlotDataPoint
import org.aqa.web.C3ScatterPlotDataSet
import org.aqa.webrun.wl.wlMonthly.WLBeam
import org.aqa.webrun.wl.wlMonthly.WLMonthly

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  */
class SSReport(extendedData: ExtendedData, monthly: WLMonthly, table: WLTable) extends SSSheet {

  override val name: String = "Report"

  private val bL = Some("border-left: 2px solid black;")
  private val bR = Some("border-left: 2px solid black;")
  private val bLR = Some("border-left: 2px solid black;border-right: 2px solid black;")
  private val bTB = Some("border-Top: 2px solid black;border-bottom: 2px solid black;")
  private val bTBL = Some("border-Top: 2px solid black;border-left: 2px solid black;border-bottom: 2px solid black;")
  private val bTBR = Some("border-Top: 2px solid black;border-right: 2px solid black;border-bottom: 2px solid black;")
  private val bTBLR = Some("border: 2px solid black;")

  private val MLCWobbleChart: C3ScatterPlot = {

    val dataList: Seq[C3ScatterPlotDataSet] = {
      val data = Seq(
        C3ScatterPlotDataPoint(monthly.mlcDyG__0_C_90, monthly.mlcDxG__0_C_90),
        C3ScatterPlotDataPoint(monthly.mlcDyG__0_C270, monthly.mlcDxG__0_C270),
        C3ScatterPlotDataPoint(monthly.mlcDyG_90_C_90, monthly.mlcDxG_90_C_90),
        C3ScatterPlotDataPoint(monthly.mlcDyG_90_C270, monthly.mlcDxG_90_C270),
        C3ScatterPlotDataPoint(monthly.mlcDyG180_C__0, monthly.mlcDxG180_C__0),
        C3ScatterPlotDataPoint(monthly.mlcDyG180_C_90, monthly.mlcDxG180_C_90),
        C3ScatterPlotDataPoint(monthly.mlcDyG180_C270, monthly.mlcDxG180_C270),
        C3ScatterPlotDataPoint(monthly.mlcDyG270_C_90, monthly.mlcDxG270_C_90),
        C3ScatterPlotDataPoint(monthly.mlcDyG270_C270, monthly.mlcDxG270_C270)
      )

      Seq(C3ScatterPlotDataSet("MLC Wobble about Collimator Axis", data))
    }

    new C3ScatterPlot(
      width = Some(450),
      height = None,
      xAxisLabel = "X (mm)",
      xDataLabel = "Z (mm)",
      dataList = dataList,
      // xFormat
      yDataLabel = "(mm)"
      // pointFormat
    )
  }

  private val TableWobbleChart: C3ScatterPlot = {

    def dXOf(beam: WLBeam): Double = {
      table.BB_Xpp(beam, table.get_Table_X_Optimized, table.get_dZT__0_Optimized, table.get_Table_X_Optimized)
    }

    def dZOf(beam: WLBeam): Double = {
      table.BB_Zpp(beam, table.get_dXT__0_Optimized, table.get_dZT__0_Optimized, table.get_Table_Z_Optimized)
    }

    def point(beam: WLBeam): C3ScatterPlotDataPoint = {
      C3ScatterPlotDataPoint(dXOf(beam), dZOf(beam))
    }

    val dataList: Seq[C3ScatterPlotDataSet] = {
      val data = table.beamList.map(point)

      Seq(C3ScatterPlotDataSet("MLC Wobble about Table Axis", data))
    }

    new C3ScatterPlot(
      width = Some(450),
      height = None,
      xAxisLabel = "X (mm)",
      xDataLabel = "Z (mm)",
      dataList = dataList,
      // xFormat
      yDataLabel = "(mm)"
      // pointFormat
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
      {toHtml("Table wobble", style = { bLR }) /*          K1 */}
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
      {toHtmlYellow(0 - monthly.isoX, style = { bTBL }) /*                             C4 */}
      {toHtmlYellow(0 - monthly.isoY, style = { bTB }) /*                             D4 */}
      {toHtmlYellow(0 - monthly.isoZ, style = { bTB }) /*                             E4 */}
      {toHtmlYellow(table.get_Table_X_Optimized - monthly.isoX, style = { bTBL }) /*   F4 */}
      {toHtmlYellow(table.get_Table_Z_Optimized - monthly.isoZ, style = { bTBL }) /*   G4 */}
      {toHtmlYellow(monthly.gantryFlex, style = { bTBLR }) /*                           H4 */}
      {toHtmlYellow(monthly.collGantryMisalign, style = { bTBLR }) /*                   I4 */}
      {toHtmlYellow(monthly.mlcOffsetY, style = { bTBLR }) /*                           J4 */}
      {toHtmlYellow(2 * Math.sqrt(table.get_RSquared_Optimized), style = bTBLR) /*  K4 */}
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
      <td>
        <h4>MLC Wobble about Collimator Axis</h4>
        {MLCWobbleChart.html}
      </td>
      <td>
        <h4>Table Wobble about Table Axis</h4>
        {TableWobbleChart.html}
      </td>
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCells(12) /* A7 to L7 */}
      {blankCell /* JJJ */}
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
       |    ${TableWobbleChart.javascript}
       | 
       | </script>""".stripMargin

  }
}
