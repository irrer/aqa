package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLTable
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil._
import org.aqa.Util
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
      {blankCells(12) /* JJJ */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6) /*                A4 to H4 */}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {makeRowIndex(8)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {makeRowIndex(9)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {makeRowIndex(10)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {makeRowIndex(11)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {makeRowIndex(12)}
      {blankCell /* JJJ */}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {makeRowIndex(13)}
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
        {makeRow8}
        {makeRow9}
        {makeRow10}
        {makeRow11}
        {makeRow12}
        {makeRow13}
      </table>
    }

    content
  }
}
