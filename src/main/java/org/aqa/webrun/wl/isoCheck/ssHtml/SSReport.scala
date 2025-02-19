package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._
import org.aqa.Util
import org.aqa.web.C3ScatterPlot
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.isoCheckHTML
import org.aqa.webrun.wl.isoCheck.isoCheckHTML.TableWobbleChart

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  */
class SSReport(extendedData: ExtendedData, isoCheck: WLIsoCheck, isoTable: Option[WLIsoTable]) extends SSSheet {

  /** true IsoTable is present. */
  private val hasIt = isoTable.isDefined

  override val name: String = "Report"

  /** CSS styles for making cell borders  */

  /** bold border style */
  private val border = "2px solid black"

  /** border Left */
  private val bL = Some(s"border-left: $border;")

  /** border Left Right */
  private val bLR = Some(s"border-left: $border;border-right: $border;")

  /** border Top Bottom */
  private val bTB = Some(s"border-Top: $border;border-bottom: $border;")

  /** border Top Bottom Left */
  private val bTBL = Some(s"border-Top: $border;border-left: $border;border-bottom: $border;")

  /** border Top Bottom Left Right */
  private val bTBLR = Some(s"border: $border;")

  private val MLCWobbleChart: C3ScatterPlot = isoCheckHTML.MLCWobbleChart.makeChart(isoCheck)

  private val IsoTableWobbleChart: C3ScatterPlot = TableWobbleChart.makeChart(isoTable)

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
      {blankCells(2) /*                     A3 to B3 */}
      {toHtml("X (mm)", style = { bL }) /*  C3 */}
      {toHtml("Y (mm)") /*                  D3 */}
      {toHtml("Z (mm)") /*                  E3 */}
      {toHtml("X (mm)", style = { bL }) /*  F3 */}
      {toHtml("Z (mm)") /*                  G3 */}
      {toHtml("(mm)", style = { bLR }) /*   H3 */}
      {toHtml("(mm)", style = { bLR }) /*   I3 */}
      {toHtml("(mm)", style = { bLR }) /*   J3 */}
      {toHtml("(mm)", style = { bLR }) /*   K3 */}
      {blankCell /*                         L3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    val dateText = Util.formatDate(Util.spreadsheetDateFormat, extendedData.output.dataDate.get)

    val F4 =
      if (hasIt)
        toHtmlYellow(isoTable.get.get_IsoTable_X_Optimized - isoCheck.isoX, style = { bTBL })
      else
        toHtml("", style = bTBL) /*   F4 */

    val G4 =
      if (hasIt)
        toHtmlYellow(isoTable.get.get_IsoTable_Z_Optimized - isoCheck.isoZ, style = { bTBL })
      else
        toHtml("", style = bTBL) /*   G4 */

    val K4 =
      if (hasIt)
        toHtmlYellow(isoTable.get.tableWobbleDiameter, style = bTBLR)
      else
        toHtml("", style = bTBLR) /*  K4 */

    <tr>
      {makeRowIndex(4)}
      {toHtml(dateText) /*                                              A4 */}
      {toHtml(extendedData.machine.getRealId) /*                        B4 */}
      {toHtmlYellow(0 - isoCheck.isoX, style = { bTBL }) /*             C4 */}
      {toHtmlYellow(0 - isoCheck.isoY, style = { bTB }) /*              D4 */}
      {toHtmlYellow(0 - isoCheck.isoZ, style = { bTB }) /*              E4 */}
      {F4 /*                                                            F4 */}
      {G4 /*                                                            G4 */}
      {toHtmlYellow(isoCheck.gantryFlex, style = { bTBLR }) /*          H4 */}
      {toHtmlYellow(isoCheck.collGantryMisalign, style = { bTBLR }) /*  I4 */}
      {toHtmlYellow(isoCheck.mlcOffsetY, style = { bTBLR }) /*          J4 */}
      {K4 /*                                                            K4 */}
      {blankCell /*                                                     L2 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {blankCells(12) /* A5 to L5 */}
    </tr>
  }

  private def makeRow6: Elem = {

    /**
      * Make the table wobble chart if the data is available, otherwise return a blank space.
      * @return Table wobble chart.
      */
    val tableWobbleChartCell: Seq[Elem] = {
      if (hasIt)
        Seq(
          <td colspan="6" style={s"border: $border;"}>
             <h4 style="text-align: center;">IsoTable Wobble about IsoTable Axis</h4>
            {IsoTableWobbleChart.html}
          </td>
        )
      else
        blankCells(6)
    }

    <tr>
      {makeRowIndex(6) /*                A4 to H4 */}
      <td colspan="5" style={s"border: $border;"}>
        <h4 style="text-align: center;">MLC Wobble about Collimator Axis</h4>
        {MLCWobbleChart.html}
      </td>
      {tableWobbleChartCell}
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
