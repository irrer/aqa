package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLBeam
import org.aqa.webrun.wl.wlMonthly.WLMonthly
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessRight

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param monthly Monthly data
  */
class SSAnalysis(extendedData: ExtendedData, monthly: WLMonthly) extends SSSheet {

  override val name: String = "Analysis"

  private def toHtml(text: String, alignLeft: Boolean = true): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    <td class={c}>
      {text}
    </td>
  }

  private def toHtml(dbl: Double): Elem = {
    toHtml(dbl.formatted("%12.2f").trim)
  }

  private def toHtml(dbl: Option[Double]): Elem = {
    if (dbl.isDefined)
      toHtml(dbl.get)
    else
      toHtml("")
  }

  private def toHtml(int: Int): Elem = {
    toHtml(int.toString)
  }

  private def blankCells(count: Int): Seq[Elem] = {
    (0 until count).map(_ => toHtml(""))
  }

  private def gantryAnglePrefix(beam: WLBeam): Seq[Elem] = {
    Seq(
      toHtml(beam.gantryAngle), /* A */
      toHtml(beam.collimatorAngle), /* B */
      toHtml(beam.tableAngle), /* C */
      toHtml(beam.wl.errorX_mm), /* D */
      toHtml(beam.wl.errorY_mm), /* E */
      toHtml(beam.caX), /* F */
      toHtml(beam.caY), /* G */
      toHtml(beam.caZ) /* H */
    )
  }

  private def makeRow1: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(1)}
      <td>Gantry</td>
      {(0 until 17).map(_ => <td></td>)}
    </tr>
  }

  private def makeRow2: Elem = {

    val mlcOffsetX: String = monthly.mlcOffsetX.formatted("%10.2f").trim
    val mlcOffsetY: String = monthly.mlcOffsetY.formatted("%10.2f").trim

    val titleList1: Seq[String] = Seq(
      "Gantry angle",
      "Collimator angle",
      "Table angle",
      "X offset corrected box-ball",
      "Y offset corrected box-ball",
      "CA-X",
      "CA-Y",
      "CA-Z",
      "Coll-X",
      "Coll-Y",
      "Coll-Z",
      "ISO-X",
      "ISO-Y",
      "ISO-Z",
      "Gantry flex",
      "Coll-Gantry-misalign",
      "MLC-dx",
      "MLC-dy",
      ""
    )

    val titleList2 = Seq("X (mm)", "Y (mm)", "Z (mm)", "")

    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {titleList1.map(toHtml(_))}
      {toHtml(mlcOffsetX, alignLeft = false)}
      {toHtml(mlcOffsetY, alignLeft = false)}
      {titleList2.map(toHtml(_))}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(3)}
      {gantryAnglePrefix(monthly.G__0_C_90_T__0)}
      {toHtml(monthly.collXG__0)}
      {blankCells(1)}
      {toHtml(monthly.collZG__0)}
      {toHtml(monthly.isoX)}
      {toHtml(monthly.isoY)}
      {toHtml(monthly.isoZ)}
      {toHtml(monthly.gantryFlex)}
      {toHtml(monthly.collGantryMisalign)}
      {toHtml(monthly.mlcDxG__0_C_90)}
      {toHtml(monthly.mlcDyG__0_C_90)}
      {toHtml(monthly.mlcOffsetX)}
      {toHtml(monthly.mlcOffsetY)}
      {toHtml("CBCT origin relative to BB at table zero")}
      {toHtml("0.00", alignLeft = false)}
      {toHtml("0.00", alignLeft = false)}
      {toHtml("0.00", alignLeft = false)}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(4)}
      {gantryAnglePrefix(monthly.G__0_C270_T__0)}
      {blankCells(8) /* I4 thru P4 */}
      {toHtml(monthly.mlcDxG__0_C270)}
      {toHtml(monthly.mlcDyG__0_C270)}
      {toHtml(monthly.mlcOffsetX_270)}
      {toHtml(monthly.mlcOffsetY_270)}
      {toHtml("Gantry isocenter relative to BB at table zero")}
      {toHtml(monthly.isoX)}
      {toHtml(monthly.isoY)}
      {toHtml(monthly.isoZ)}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(5)}
      {gantryAnglePrefix(monthly.G_90_C_90_T__0)}
      {blankCells(1) /* I5 */}
      {toHtml(monthly.collYG_90) /* J5 */}
      {toHtml(monthly.collZG_90) /* K5 */}
      {blankCells(5)}
      { /* M5 */ }
      { /* N5 */ }
      {blankCells(2)}
      {toHtml("Table axis relative to BB at table zero")}
      {toHtml("V5 TODO") /* V5 */ /* TODO */}
      {blankCells(1)}
      {toHtml("X5 TODO") /* X5 */ /* TODO */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(6)}
      {gantryAnglePrefix(monthly.G_90_C270_T__0) /* I6 */}
      {blankCells(4) /* I6 thru L6 */}
      {toHtml("ΔX") /* M6 */}
      {toHtml("ΔY") /* N6 */}
      {toHtml("ΔZ") /* O6 */}
      {blankCells(1) /* P6 */}
      {toHtml(monthly.mlcDxG_90_C270) /* Q6 */}
      {toHtml(monthly.mlcDyG_90_C270) /* R6 */}
      {blankCells(6)}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(7)}
      {gantryAnglePrefix(monthly.G180_C__0_T__0) /* A7 thru G7 */}
      {blankCells(4) /* I7 thru L7 */}
      {toHtml(monthly.isoXRange)/* M7 */}
      {toHtml(monthly.isoYRange)/* N7 */}
      {toHtml(monthly.isoZRange)/* O7 */}
      {blankCells(1) /* P7 */}
      {toHtml(monthly.mlcDxG180_C__0) /* Q7 */}
      {toHtml(monthly.mlcDyG180_C__0) /* R7 */}
      {blankCells(6)}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(8)}
      {gantryAnglePrefix(monthly.G180_C_90_T__0) /* I8 */}
      {toHtml(monthly.collXG180)}
      {blankCells(1)}
      {toHtml(monthly.collZG180)}
      {blankCells(3)}
      {toHtml(monthly.gantryIsocentricity)}
      {toHtml("Gantry Isocentricity")}
      {toHtml(monthly.mlcDxG180_C_90)}
      {toHtml(monthly.mlcDyG180_C_90)}
      {blankCells(6)}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(9)}
      {gantryAnglePrefix(monthly.G180_C270_T__0)}
      {blankCells(8) /* I9 thru P9 */}
      {toHtml(monthly.mlcDxG180_C270)}
      {toHtml(monthly.mlcDyG180_C270)}
      {blankCells(6)}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(10)}
      {gantryAnglePrefix(monthly.G270_C_90_T__0)}
      {blankCells(1) /* I10 */}
      {toHtml(monthly.collYG270) /* J10 */}
      {toHtml(monthly.collZG270) /* K10 */}
      {blankCells(6)}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(11)}
      {gantryAnglePrefix(monthly.G270_C270_T__0)}
      {blankCells(8) /* I11 thru P11 */}
      {toHtml(monthly.mlcDxG270_C270) /* J10 */}
      {toHtml(monthly.mlcDyG270_C270) /* K10 */}
      {blankCells(6)}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(12)}
      {blankCells(10)}
      {toHtml("TODO K12") /* TODO */}
      {blankCells(3)}
      {toHtml("Max square of BB displacement")}
      {toHtml("->")}
      {toHtml("TODO R12") /* TODO */}
      {toHtml("Solve for smallest max (Couch Isocentricity)")}
      {blankCells(5)}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(13)}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {WLXlsxUtil.makeAlphaRow(24)}
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
