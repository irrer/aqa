package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.wlMonthly.WLBeam
import org.aqa.webrun.wl.wlMonthly.WLMonthly
import org.aqa.webrun.wl.wlMonthly.WLTable
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessLeft
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.cssPreprocessRight
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.flip
import org.aqa.webrun.wl.wlMonthly.WLXlsxUtil.numericFormat

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param monthly Monthly data
  */
class SSAnalysis(extendedData: ExtendedData, monthly: WLMonthly, table: WLTable) extends SSSheet {

  override val name: String = "Analysis"

  private def fmt(d: Double): String = d.formatted(numericFormat).trim

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

  private def toHtmlYellow(dbl: Double): Elem = {
    <td class={cssPreprocessRight} style="background:#FFFF00;">
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

  private def blankCell: Elem = {
    blankCells(1).head
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

  /**
    * Makes cells A to G which are common to all gantry beams.
    * @param beamOpt Beam to show.
    * @return cells for common content.
    */
  private def tableAnglePrefix(beamOpt: Option[WLBeam]): Seq[Elem] = {

    if (beamOpt.isEmpty) {
      toHtml("NA") +: blankCells(24)
    } else {
      val beam = beamOpt.get

      val dX = table.dXOf(beam, table.dXT__0_Optimized, table.dZT__0_Optimized)
      val dZ = table.dZOf(beam, table.dXT__0_Optimized, table.dZT__0_Optimized)

      val tableAngle0 = beam.tableAngle == 0

      val L = //                             L dX
        if (tableAngle0)
          toHtmlYellow(dX)
        else
          toHtmlPowderBlue(dX)

      val M = //                             M dX
        if (tableAngle0)
          toHtmlYellow(dZ)
        else
          toHtmlPowderBlue(dZ)

      val N = //                             N Table-X
        if (tableAngle0)
          toHtmlYellow(table.Table_X_Optimized)
        else
          blankCell

      val O = //                             N Table-Z
        if (tableAngle0)
          toHtmlYellow(table.Table_Z_Optimized)
        else
          blankCell

      Seq(
        toHtml(beam.gantryAngle), /*             A */
        toHtml(beam.collimatorAngle), /*         B */
        toHtml(flip(beam.tableAngle)), /*        C */
        toHtml(beam.wl.errorX_mm), /*            D X offset corrected box-ball */
        toHtml(beam.wl.errorY_mm), /*            E X offset corrected box-ball */
        toHtml(WLTable.CA_X(beam)), /*           F CA-X */
        toHtml(WLTable.CA_Z(beam)), /*           G CA-Z */
        toHtml(table.BB_X(beam)), /*             H BB-X */
        toHtml(table.BB_Z(beam)), /*             I BB-Z */
        toHtml(table.BB_Xp(beam, table.dXT__0_Optimized, table.dZT__0_Optimized)), /*            J BB-X' */
        toHtml(table.BB_Zp(beam, table.dXT__0_Optimized, table.dZT__0_Optimized)), /*            K BB-Z' */
        L, /*                                    L dX */
        M, /*                                    M dZ */
        N, /*                                    N Table-X */
        O, /*                                    O Table-Z */
        toHtmlPowderBlue(table.BB_Xpp(beam, table.Table_X_Optimized, table.dZT__0_Optimized, table.Table_X_Optimized)), /* P BB-X" */
        toHtmlPowderBlue(table.BB_Zpp(beam, table.dXT__0_Optimized, table.dZT__0_Optimized, table.Table_Z_Optimized)), /* Q BB-Z" */
        toHtmlPowderBlue(table.BB_Rpp(beam, table.dXT__0_Optimized, table.dZT__0_Optimized, table.Table_X_Optimized, table.Table_Z_Optimized)) /*  R BB-R"^2 */
      ) ++ blankCells(6) /*                      S to X */
    }
  }

  private def makeRow1: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(1)}
      {toHtml("Gantry") /*           A1 */}
      {blankCells(17) /*             B1 to R1 */}
      {toHtml("MLC offset") /*       S1 */}
      {toHtml(monthly.mlcOffsetY) /* T1 */}
      {blankCells(4) /*              U1 to X1 */}
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
      {titleList1.map(toHtml(_)) /*  A2 to R2 */}
      {toHtml(monthly.mlcOffsetX) /* S2 */}
      {toHtml(monthly.mlcOffsetY) /* T2 */}
      {blankCell /*                  U2 */}
      {titleList2.map(toHtml(_)) /*  V2 to X2 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(3)}
      {gantryAnglePrefix(monthly.G__0_C_90_T__0) /*          A3 to G3 */}
      {toHtml(monthly.collXG__0) /*                          I3 */}
      {blankCell /*                                          J3 */}
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
      {gantryAnglePrefix(monthly.G__0_C270_T__0) /*               A4 to H4 */}
      {blankCells(8) /*                                           I4 to P4 */}
      {toHtml(monthly.mlcDxG__0_C270) /*                          Q4 */}
      {toHtml(monthly.mlcDyG__0_C270) /*                          R4 */}
      {toHtml(monthly.mlcOffsetX_270) /*                          S4 */}
      {toHtml(monthly.mlcOffsetY_270) /*                          T4 */}
      {toHtml("Gantry isocenter relative to BB at table zero") /* U4 */}
      {toHtml(monthly.isoX) /* V4 */}
      {toHtml(monthly.isoY) /* W4 */}
      {toHtml(monthly.isoZ) /* X4 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(5)}
      {gantryAnglePrefix(monthly.G_90_C_90_T__0) /*         A4 to H4 */}
      {blankCell /*                                         I5 */}
      {toHtml(monthly.collYG_90) /*                         J5 */}
      {toHtml(monthly.collZG_90) /*                         K5 */}
      {blankCells(5) /*                                     L5 to P5 */}
      {toHtml(monthly.mlcDxG_90_C_90) /*                    Q5 */}
      {toHtml(monthly.mlcDyG_90_C_90) /*                    R5 */}
      {blankCells(2) /*                                     S5 to T5 */}
      {toHtml("Table axis relative to BB at table zero") /* U5 */}
      {toHtml(table.Table_X_Optimized) /*                             V5 */}
      {blankCell /*                                         S5 to T5 */}
      {toHtml(table.Table_Z_Optimized) /*                             X5 */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(6) /*                A4 to H4 */}
      {gantryAnglePrefix(monthly.G_90_C270_T__0) /* I6 */}
      {blankCells(4) /*                             I6 to L6 */}
      {toHtml("ΔX") /*                              M6 */}
      {toHtml("ΔY") /*                              N6 */}
      {toHtml("ΔZ") /*                              O6 */}
      {blankCell /*                                 P6 */}
      {toHtml(monthly.mlcDxG_90_C270) /*            Q6 */}
      {toHtml(monthly.mlcDyG_90_C270) /*            R6 */}
      {blankCells(6) /*                             R6 */}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(7)}
      {gantryAnglePrefix(monthly.G180_C__0_T__0) /* A7 to G7 */}
      {blankCells(4) /* I7 to L7 */}
      {toHtml(monthly.isoXRange) /* M7 */}
      {toHtml(monthly.isoYRange) /* N7 */}
      {toHtml(monthly.isoZRange) /* O7 */}
      {blankCell /* P7 */}
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
      {blankCell /* J8 */}
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
      {blankCells(8) /* I9 to P9 */}
      {toHtml(monthly.mlcDxG180_C270) /* Q9 */}
      {toHtml(monthly.mlcDyG180_C270) /* R9 */}
      {blankCells(6) /* S9 to X9 */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(10)}
      {gantryAnglePrefix(monthly.G270_C_90_T__0) /* A9 to H9 */}
      {blankCell /* I10 */}
      {toHtml(monthly.collYG270) /* J10 */}
      {toHtml(monthly.collZG270) /* K10 */}
      {blankCells(5) /* L10 to P10 */}
      {toHtml(monthly.mlcDxG270_C_90) /* Q10 */}
      {toHtml(monthly.mlcDyG270_C_90) /* R10 */}
      {blankCells(6) /* S10 to X10 */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(11)}
      {gantryAnglePrefix(monthly.G270_C270_T__0) /* A11 to H11 */}
      {blankCells(8) /* I11 to P11 */}
      {toHtml(monthly.mlcDxG270_C270) /* Q11 */}
      {toHtml(monthly.mlcDyG270_C270) /* R11 */}
      {blankCells(6) /* S11 to X11 */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(12)}
      {blankCells(10) /* A12 to J12*/}
      {toHtml(table.K12(table.dXT__0_Optimized, table.dZT__0_Optimized)) /* K12 */}
      {blankCells(5) /* L12 to P12*/}
      {toHtmlPowderBlue("Max square of BB displacement" + WebUtil.rightBoldArrow) /* Q12 */}
      {toHtmlPeach(table.minSquareOfBBDisplacement(table.dXT__0_Optimized, table.dZT__0_Optimized, table.Table_X_Optimized, table.Table_Z_Optimized)) /* R12 */}
      {toHtml("Solve for smallest max (Couch Isocentricity)") /* S12 */}
      {blankCells(4) /* T12 to X12*/}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(13)}
      {toHtml("Table") /* A13 */}
      {blankCells(4) /* B13 to E13*/}
      {toHtml("CA-X") /* F13 */}
      {toHtml("CA-Z") /* G13 */}
      {toHtml("BB-X") /* H13 */}
      {toHtml("BB-Z") /* I13 */}
      {toHtml("BB-X'") /* J13 */}
      {toHtml("BB-Z'") /* K13 */}
      {toHtml("dX") /* L13 */}
      {toHtml("dZ") /* M13 */}
      {toHtml("Table-X") /* N13 */}
      {toHtml("Table-Z") /* O13 */}
      {toHtml("BB-X\"") /* P13 */}
      {toHtml("BB-Z\"") /* Q13 */}
      {toHtml("BB-R\"^2") /* R13 */}
      {blankCells(6) /* S13 to X13*/}
    </tr>
  }

  private def makeRow14: Elem = {

    <tr>
      {WLXlsxUtil.makeRowIndex(14)}
      {tableAnglePrefix(table.T__0) /* A14 to G14 */}
    </tr>
  }

  private def makeRow15: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(15)}
      {tableAnglePrefix(table.T330) /* A15 to G15 */}
    </tr>
  }

  private def makeRow16: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(16)}
      {tableAnglePrefix(table.T300) /* A16 to G16 */}
    </tr>
  }

  private def makeRow17: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(17)}
      {tableAnglePrefix(table.T270) /* A17 to G17 */}
    </tr>
  }

  private def makeRow18: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(18)}
      {tableAnglePrefix(table.T_90) /* A18 to G18 */}
    </tr>
  }

  private def makeRow19: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(19)}
      {tableAnglePrefix(table.T_60) /* A19 to G19 */}
    </tr>
  }

  private def makeRow20: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(20)}
      {tableAnglePrefix(table.T_30) /* A20 to G20 */}
    </tr>
  }

  private def makeRow21: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(21)}
      {blankCells(11)}
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;">
        Coordinates in rotated system
      </td>
      {blankCells(2)}
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;">
        BB coordinates after minimization
      </td>
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;">
        sum of squares x^2 +y^2 for each table angle
      </td>
      {blankCells(5)}
    </tr>
  }

  private def makeRow22: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(22)}
      {blankCells(18)}
    </tr>
  }

  private def makeRow23: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(23)}
      {blankCells(24)}
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
        {makeRow14}
        {makeRow15}
        {makeRow16}
        {makeRow17}
        {makeRow18}
        {makeRow19}
        {makeRow20}
        {makeRow21}
        {makeRow22}
        {makeRow23}
      </table>
    }

    content
  }
}
