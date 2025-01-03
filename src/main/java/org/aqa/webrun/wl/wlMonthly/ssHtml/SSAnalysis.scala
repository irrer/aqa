package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.web.WebUtil
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

  private def fmt(d: Double): String = d.formatted("%12.2f").trim

  private def toHtml(text: String, alignLeft: Boolean = true): Elem = {
    val c = if (alignLeft) cssPreprocessLeft else cssPreprocessRight
    <td class={c}>
      {text}
    </td>
  }

  private def toHtmlPeach(dbl: Double): Elem = {
    <td class={cssPreprocessRight} style="border:2px solid black;background:#F8CBAD;">
      {fmt(dbl)}
    </td>
  }

  private def toHtmlPowderBlue(text: String): Elem = {
    <td class={cssPreprocessLeft} style="border:2px solid black;background:#DDEBF7;">
      {text}
    </td>
  }

  private def toHtmlPowderBlue(dbl: Double): Elem = {
    <td class={cssPreprocessRight} style="border:2px solid black;background:#DDEBF7;">
      {fmt(dbl)}
    </td>
  }

  private def toHtmlYellow(text: String): Elem = {
    <td class={cssPreprocessLeft} style="border:2px solid black;background:#FFFF00;">
      {text}
    </td>
  }

  private def toHtmlYellow(dbl: Double): Elem = {
    <td class={cssPreprocessRight} style="border:2px solid black;background:#FFFF00;">
      {fmt(dbl)}
    </td>
  }

  private def toHtml(dbl: Double): Elem = {
    toHtml(fmt(dbl), alignLeft = false)
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

  /**
    * Makes cells A to H which are common to all gantry beams.
    * @param beam Beam to show.
    * @return cells for common content.
    */
  private def gantryAnglePrefix(beam: WLBeam): Seq[Elem] = {
    Seq(
      toHtml(beam.gantryAngle), /*     A */
      toHtml(beam.collimatorAngle), /* B */
      toHtml(beam.tableAngle), /*      C */
      toHtml(beam.wl.errorX_mm), /*    D */
      toHtml(beam.wl.errorY_mm), /*    E */
      toHtml(beam.caX), /*             F */
      toHtml(beam.caY), /*             G */
      toHtml(beam.caZ) /*              H */
    )
  }

  private def makeRow1: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(1)}
      {toHtml("Gantry") /*      A1 */}
      {blankCells(17) /*        B1 to R1 */}
      {toHtml("MLC offset") /*  S1 */}
      {toHtml("TODO T1") /*     TODO T1 */}
      {blankCells(4) /*         U1 to X1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    val titleList1: Seq[String] = Seq(
      "Gantry angle", /* A2 */
      "Collimator angle", /* B2 */
      "Table angle", /* C2 */
      "X offset corrected box-ball", /* D2 */
      "Y offset corrected box-ball", /* E2 */
      "CA-X", /* F2 */
      "CA-Y", /* G2 */
      "CA-Z", /* H2 */
      "Coll-X", /* I2 */
      "Coll-Y", /* J2 */
      "Coll-Z", /* K2 */
      "ISO-X", /* L2 */
      "ISO-Y", /* M2 */
      "ISO-Z", /* N2 */
      "Gantry flex", /* O2 */
      "Coll-Gantry-misalign", /* P2 */
      "MLC-dx", /* Q2 */
      "MLC-dy" /* R2 */
    )

    val titleList2 = Seq("X (mm)", "Y (mm)", "Z (mm)") /* V2 to X2 */

    <tr>
      {WLXlsxUtil.makeRowIndex(2)}
      {titleList1.map(toHtml(_)) /* A2 to R2 */}
      {toHtml(monthly.mlcOffsetX) /* S2 */}
      {toHtml(monthly.mlcOffsetY) /* T2 */}
      {blankCells(1) /* U2 */}
      {titleList2.map(toHtml(_)) /* V2 to X2 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(3)}
      {gantryAnglePrefix(monthly.G__0_C_90_T__0) /*          A3 to G3 */}
      {toHtml(monthly.collXG__0) /*                          I3 */}
      {blankCells(1) /*                                      J3 */}
      {toHtml(monthly.collZG__0) /*                          K3 */}
      {toHtml(monthly.isoX) /*                               L3 */}
      {toHtml(monthly.isoY) /*                               M3 */}
      {toHtml(monthly.isoZ) /*                               N3 */}
      {toHtml(monthly.gantryFlex) /*                         O3 */}
      {toHtml(monthly.collGantryMisalign) /*                 P3 */}
      {toHtml(monthly.mlcDxG__0_C_90) /*                     Q3 */}
      {toHtml(monthly.mlcDyG__0_C_90) /*                     R3 */}
      {toHtml(monthly.mlcOffsetX_090) /*                     S3 */}
      {toHtml(monthly.mlcOffsetY_090) /*                     T3 */}
      {toHtml("CBCT origin relative to BB at table zero") /* U3 */}
      {toHtml("0.00", alignLeft = false) /*                  V3 */}
      {toHtml("0.00", alignLeft = false) /*                  W3 */}
      {toHtml("0.00", alignLeft = false) /*                  X3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(4)}
      {gantryAnglePrefix(monthly.G__0_C270_T__0) /* A4 to H4 */}
      {blankCells(8) /* I4 thru P4 */}
      {toHtml(monthly.mlcDxG__0_C270) /* Q4 */}
      {toHtml(monthly.mlcDyG__0_C270) /* R4 */}
      {toHtml(monthly.mlcOffsetX_270) /* S4 */}
      {toHtml(monthly.mlcOffsetY_270) /* T4 */}
      {toHtml("Gantry isocenter relative to BB at table zero") /* U4 */}
      {toHtml(monthly.isoX) /* V4 */}
      {toHtml(monthly.isoY) /* W4 */}
      {toHtml(monthly.isoZ) /* X4 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(5)}
      {gantryAnglePrefix(monthly.G_90_C_90_T__0)}
      {blankCells(1) /* I5 */}
      {toHtml(monthly.collYG_90) /* J5 */}
      {toHtml(monthly.collZG_90) /* K5 */}
      {blankCells(5) /* L5 thru P5 */}
      {toHtml(monthly.mlcDxG_90_C_90) /* Q5 */}
      {toHtml(monthly.mlcDyG_90_C_90) /* R5 */}
      {blankCells(2) /* S5 to T5 */}
      {toHtml("Table axis relative to BB at table zero") /* U5 */}
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
      {blankCells(4) /* I7 to L7 */}
      {toHtml(monthly.isoXRange) /* M7 */}
      {toHtml(monthly.isoYRange) /* N7 */}
      {toHtml(monthly.isoZRange) /* O7 */}
      {blankCells(1) /* P7 */}
      {toHtml(monthly.mlcDxG180_C__0) /* Q7 */}
      {toHtml(monthly.mlcDyG180_C__0) /* R7 */}
      {blankCells(6)}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(8)}
      {gantryAnglePrefix(monthly.G180_C_90_T__0) /* A8 to H8 */}
      {toHtml(monthly.collXG180) /* I8 */}
      {blankCells(1) /* J8 */}
      {toHtml(monthly.collZG180) /* K8 */}
      {blankCells(3) /* L8 to N8 */}
      {toHtmlPeach(monthly.gantryIsocentricity) /* O8 */}
      {toHtml("Gantry Isocentricity") /* P8 */}
      {toHtml(monthly.mlcDxG180_C_90) /* Q8 */}
      {toHtml(monthly.mlcDyG180_C_90) /* R8 */}
      {blankCells(6) /* S8 to X8 */}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(9)}
      {gantryAnglePrefix(monthly.G180_C270_T__0) /* A9 to H9 */}
      {blankCells(8) /* I9 thru P9 */}
      {toHtml(monthly.mlcDxG180_C270) /* Q9 */}
      {toHtml(monthly.mlcDyG180_C270) /* R9 */}
      {blankCells(6) /* S9 to X9 */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(10)}
      {gantryAnglePrefix(monthly.G270_C_90_T__0) /* A9 to H9 */}
      {blankCells(1) /* I10 */}
      {toHtml(monthly.collYG270) /* J10 */}
      {toHtml(monthly.collZG270) /* K10 */}
      {blankCells(5) /* L10 thru P10 */}
      {toHtml(monthly.mlcDxG270_C_90) /* Q10 */}
      {toHtml(monthly.mlcDyG270_C_90) /* R10 */}
      {blankCells(6) /* S10 to X10 */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(11)}
      {gantryAnglePrefix(monthly.G270_C270_T__0)}
      {blankCells(8) /* I11 thru P11 */}
      {toHtml(monthly.mlcDxG270_C270) /* Q11 */}
      {toHtml(monthly.mlcDyG270_C270) /* R11 */}
      {blankCells(6) /* S11 to X11 */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(12)}
      {blankCells(10)}
      {toHtml("TODO K12") /* TODO */}
      {blankCells(3)}
      {toHtmlPowderBlue("Max square of BB displacement")}
      {toHtmlPowderBlue(WebUtil.rightBoldArrow)}
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
