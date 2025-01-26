package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param isoCheck IsoCheck data
  */
class SSAnalysis(extendedData: ExtendedData, isoCheck: WLIsoCheck, isoTable: WLIsoTable) extends SSSheet {

  override val name: String = "Analysis"

  /**
    * Makes cells A to H which are common to all gantry beams.
    * @param beam Beam to show.
    * @return cells for common content.
    */
  private def gantryAnglePrefix(beam: WLBeam): Seq[Elem] = {
    Seq(
      toHtml(beam.gantryAngle), /*     A */
      toHtml(beam.collimatorAngle), /* B */
      toHtml(beam.isoTableAngle), /*   C */
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
  private def isoTableAnglePrefix(beamOpt: Option[WLBeam]): Seq[Elem] = {

    if (beamOpt.isEmpty) {
      toHtml("NA") +: blankCells(24)
    } else {
      val beam = beamOpt.get

      val dX = isoTable.dXOf(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized)
      val dZ = isoTable.dZOf(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized)

      val isoTableAngle0 = beam.isoTableAngle == 0

      val L = //                             L dX
        if (isoTableAngle0)
          toHtmlYellow(isoTable.get_dXT__0_Optimized)
        else
          toHtmlPowderBlue(dX)

      val M = //                             M dX
        if (isoTableAngle0)
          toHtmlYellow(isoTable.get_dZT__0_Optimized)
        else
          toHtmlPowderBlue(dZ)

      val N = //                             N IsoTable-X
        if (isoTableAngle0)
          toHtmlYellow(isoTable.get_IsoTable_X_Optimized)
        else
          blankCell

      val O = //                             N IsoTable-Z
        if (isoTableAngle0)
          toHtmlYellow(isoTable.get_IsoTable_Z_Optimized)
        else
          blankCell

      Seq(
        toHtml(beam.gantryAngle), /*             A */
        toHtml(beam.collimatorAngle), /*         B */
        toHtml(flip(beam.isoTableAngle)), /*     C */
        toHtml(beam.wl.errorX_mm), /*            D X offset corrected box-ball */
        toHtml(beam.wl.errorY_mm), /*            E X offset corrected box-ball */
        toHtml(WLIsoTable.CA_X(beam)), /*        F CA-X */
        toHtml(WLIsoTable.CA_Z(beam)), /*        G CA-Z */
        toHtml(isoTable.BB_X(beam)), /*          H BB-X */
        toHtml(isoTable.BB_Z(beam)), /*          I BB-Z */
        toHtml(isoTable.BB_Xp(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized)), /* J BB-X' */
        toHtml(isoTable.BB_Zp(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized)), /* K BB-Z' */
        L, /*                                    L dX */
        M, /*                                    M dZ */
        N, /*                                    N IsoTable-X */
        O, /*                                    O IsoTable-Z */
        toHtmlPowderBlue(isoTable.BB_Xp(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized) - isoTable.get_IsoTable_X_Optimized), /* P BB-X" */
        toHtmlPowderBlue(isoTable.BB_Zpp(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized, isoTable.get_IsoTable_Z_Optimized)), /* Q BB-Z" */
        toHtmlPowderBlue(isoTable.BB_Rpp(beam, isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized, isoTable.get_IsoTable_X_Optimized, isoTable.get_IsoTable_Z_Optimized)) /*  R BB-R"^2 */
      ) ++ blankCells(6) /*                      S to X */
    }
  }

  private def makeRow1: Elem = {
    <tr>
      {makeRowIndex(1)}
      {toHtml("Gantry") /*            A1 */}
      {blankCells(17) /*              B1 to R1 */}
      {toHtml("MLC offset") /*        S1 */}
      {toHtml(isoCheck.mlcOffsetY) /* T1 */}
      {blankCells(4) /*               U1 to X1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    val titleList1: Seq[String] = Seq(
      "Gantry angle", /*                A2 */
      "Collimator angle", /*            B2 */
      "Table angle", /*                 C2 */
      "X offset corrected box-ball", /* D2 */
      "Y offset corrected box-ball", /* E2 */
      "CA-X", /*                        F2 */
      "CA-Y", /*                        G2 */
      "CA-Z", /*                        H2 */
      "Coll-X", /*                      I2 */
      "Coll-Y", /*                      J2 */
      "Coll-Z", /*                      K2 */
      "ISO-X", /*                       L2 */
      "ISO-Y", /*                       M2 */
      "ISO-Z", /*                       N2 */
      "Gantry flex", /*                 O2 */
      "Coll-Gantry-misalign", /*        P2 */
      "MLC-dx", /*                      Q2 */
      "MLC-dy" /*                       R2 */
    )

    val titleList2 = Seq("X (mm)", "Y (mm)", "Z (mm)") /* V2 to X2 */

    <tr>
      {makeRowIndex(2)}
      {titleList1.map(toHtml(_)) /*   A2 to R2 */}
      {toHtml(isoCheck.mlcOffsetX) /* S2 */}
      {toHtml(isoCheck.mlcOffsetY) /* T2 */}
      {blankCell /*                   U2 */}
      {titleList2.map(toHtml(_)) /*   V2 to X2 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {makeRowIndex(3)}
      {gantryAnglePrefix(isoCheck.G__0_C_90_T__0) /*            A3 to G3 */}
      {toHtml(isoCheck.collXG__0) /*                            I3 */}
      {blankCell /*                                             J3 */}
      {toHtml(isoCheck.collZG__0) /*                            K3 */}
      {toHtml(isoCheck.isoX) /*                                 L3 */}
      {toHtml(isoCheck.isoY) /*                                 M3 */}
      {toHtml(isoCheck.isoZ) /*                                 N3 */}
      {toHtml(isoCheck.gantryFlex) /*                           O3 */}
      {toHtml(isoCheck.collGantryMisalign) /*                   P3 */}
      {toHtml(isoCheck.mlcDxG__0_C_90) /*                       Q3 */}
      {toHtml(isoCheck.mlcDyG__0_C_90) /*                       R3 */}
      {toHtml(isoCheck.mlcOffsetX_090) /*                       S3 */}
      {toHtml(isoCheck.mlcOffsetY_090) /*                       T3 */}
      {toHtml("CBCT origin relative to BB at Table zero") /*    U3 */}
      {toHtml(0.0) /*                                           V3 */}
      {toHtml(0.0) /*                                           W3 */}
      {toHtml(0.0) /*                                           X3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {makeRowIndex(4)}
      {gantryAnglePrefix(isoCheck.G__0_C270_T__0) /*                 A4 to H4 */}
      {blankCells(8) /*                                              I4 to P4 */}
      {toHtml(isoCheck.mlcDxG__0_C270) /*                            Q4 */}
      {toHtml(isoCheck.mlcDyG__0_C270) /*                            R4 */}
      {toHtml(isoCheck.mlcOffsetX_270) /*                            S4 */}
      {toHtml(isoCheck.mlcOffsetY_270) /*                            T4 */}
      {toHtml("Gantry isocenter relative to BB at Table zero") /*    U4 */}
      {toHtml(isoCheck.isoX) /* V4 */}
      {toHtml(isoCheck.isoY) /* W4 */}
      {toHtml(isoCheck.isoZ) /* X4 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {gantryAnglePrefix(isoCheck.G_90_C_90_T__0) /*         A4 to H4 */}
      {blankCell /*                                          I5 */}
      {toHtml(isoCheck.collYG_90) /*                         J5 */}
      {toHtml(isoCheck.collZG_90) /*                         K5 */}
      {blankCells(5) /*                                      L5 to P5 */}
      {toHtml(isoCheck.mlcDxG_90_C_90) /*                    Q5 */}
      {toHtml(isoCheck.mlcDyG_90_C_90) /*                    R5 */}
      {blankCells(2) /*                                      S5 to T5 */}
      {toHtml("Table axis relative to BB at Table zero") /*  U5 */}
      {toHtml(isoTable.get_IsoTable_X_Optimized) /*          V5 */}
      {blankCell /*                                          S5 to T5 */}
      {toHtml(isoTable.get_IsoTable_Z_Optimized) /*          X5 */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6) /*                            A4 to H4 */}
      {gantryAnglePrefix(isoCheck.G_90_C270_T__0) /* I6 */}
      {blankCells(4) /*                              I6 to L6 */}
      {toHtml("ΔX") /*                               M6 */}
      {toHtml("ΔY") /*                               N6 */}
      {toHtml("ΔZ") /*                               O6 */}
      {blankCell /*                                  P6 */}
      {toHtml(isoCheck.mlcDxG_90_C270) /*            Q6 */}
      {toHtml(isoCheck.mlcDyG_90_C270) /*            R6 */}
      {blankCells(6) /*                              S6 to X6 */}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {gantryAnglePrefix(isoCheck.G180_C__0_T__0) /* A7 to G7 */}
      {blankCells(4) /*                              I7 to L7 */}
      {toHtml(isoCheck.isoXRange) /*                 M7 */}
      {toHtml(isoCheck.isoYRange) /*                 N7 */}
      {toHtml(isoCheck.isoZRange) /*                 O7 */}
      {blankCell /*                                  P7 */}
      {toHtml(isoCheck.mlcDxG180_C__0) /*            Q7 */}
      {toHtml(isoCheck.mlcDyG180_C__0) /*            R7 */}
      {blankCells(6) /*                              S7 to X7 */}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {makeRowIndex(8)}
      {gantryAnglePrefix(isoCheck.G180_C_90_T__0) /* A8 to H8 */}
      {toHtml(isoCheck.collXG180) /*                 I8 */}
      {blankCell /*                                  J8 */}
      {toHtml(isoCheck.collZG180) /*                 K8 */}
      {blankCells(3) /*                              L8 to N8 */}
      {toHtmlPeach(isoCheck.gantryIsocentricity) /*  O8 */}
      {toHtml("Gantry Isocentricity") /*             P8 */}
      {toHtml(isoCheck.mlcDxG180_C_90) /*            Q8 */}
      {toHtml(isoCheck.mlcDyG180_C_90) /*            R8 */}
      {blankCells(6) /*                              S8 to X8 */}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {makeRowIndex(9)}
      {gantryAnglePrefix(isoCheck.G180_C270_T__0) /* A9 to H9 */}
      {blankCells(8) /*                              I9 to P9 */}
      {toHtml(isoCheck.mlcDxG180_C270) /*            Q9 */}
      {toHtml(isoCheck.mlcDyG180_C270) /*            R9 */}
      {blankCells(6) /*                              S9 to X9 */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {makeRowIndex(10)}
      {gantryAnglePrefix(isoCheck.G270_C_90_T__0) /* A9 to H9 */}
      {blankCell /*                                  I10 */}
      {toHtml(isoCheck.collYG270) /*                 J10 */}
      {toHtml(isoCheck.collZG270) /*                 K10 */}
      {blankCells(5) /*                              L10 to P10 */}
      {toHtml(isoCheck.mlcDxG270_C_90) /*            Q10 */}
      {toHtml(isoCheck.mlcDyG270_C_90) /*            R10 */}
      {blankCells(6) /*                              S10 to X10 */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {makeRowIndex(11)}
      {gantryAnglePrefix(isoCheck.G270_C270_T__0) /* A11 to H11 */}
      {blankCells(8) /*                              I11 to P11 */}
      {toHtml(isoCheck.mlcDxG270_C270) /*            Q11 */}
      {toHtml(isoCheck.mlcDyG270_C270) /*            R11 */}
      {blankCells(6) /*                              S11 to X11 */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {makeRowIndex(12)}
      {blankCells(10) /*                                                                      A12 to J12*/}
      {toHtml(isoTable.K12(isoTable.get_dXT__0_Optimized, isoTable.get_dZT__0_Optimized)) /*  K12 */}
      {blankCells(5) /*                                                                       L12 to P12*/}
      {toHtmlPowderBlue("Max square of BB displacement" + WebUtil.rightBoldArrow) /*          Q12 */}
      {toHtmlPeach(isoTable.get_RSquared_Optimized) /*                                        R12 */}
      {toHtml("Solve for smallest max (Couch Isocentricity)") /*                              S12 */}
      {blankCells(4) /*                                                                       T12 to X12*/}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {makeRowIndex(13)}
      {toHtml("Table") /*    A13 */}
      {blankCells(4) /*      B13 to E13*/}
      {toHtml("CA-X") /*     F13 */}
      {toHtml("CA-Z") /*     G13 */}
      {toHtml("BB-X") /*     H13 */}
      {toHtml("BB-Z") /*     I13 */}
      {toHtml("BB-X'") /*    J13 */}
      {toHtml("BB-Z'") /*    K13 */}
      {toHtml("dX") /*       L13 */}
      {toHtml("dZ") /*       M13 */}
      {toHtml("Table-X") /*  N13 */}
      {toHtml("Table-Z") /*  O13 */}
      {toHtml("BB-X\"") /*   P13 */}
      {toHtml("BB-Z\"") /*   Q13 */}
      {toHtml("BB-R\"^2") /* R13 */}
      {blankCells(6) /*      S13 to X13*/}
    </tr>
  }

  private def makeRow14: Elem = {

    <tr>
      {makeRowIndex(14)}
      {isoTableAnglePrefix(isoTable.T__0) /* A14 to G14 */}
    </tr>
  }

  private def makeRow15: Elem = {
    <tr>
      {makeRowIndex(15)}
      {isoTableAnglePrefix(isoTable.T330) /* A15 to G15 */}
    </tr>
  }

  private def makeRow16: Elem = {
    <tr>
      {makeRowIndex(16)}
      {isoTableAnglePrefix(isoTable.T300) /* A16 to G16 */}
    </tr>
  }

  private def makeRow17: Elem = {
    <tr>
      {makeRowIndex(17)}
      {isoTableAnglePrefix(isoTable.T270) /* A17 to G17 */}
    </tr>
  }

  private def makeRow18: Elem = {
    <tr>
      {makeRowIndex(18)}
      {isoTableAnglePrefix(isoTable.T_90) /* A18 to G18 */}
    </tr>
  }

  private def makeRow19: Elem = {
    <tr>
      {makeRowIndex(19)}
      {isoTableAnglePrefix(isoTable.T_60) /* A19 to G19 */}
    </tr>
  }

  private def makeRow20: Elem = {
    <tr>
      {makeRowIndex(20)}
      {isoTableAnglePrefix(isoTable.T_30) /* A20 to G20 */}
    </tr>
  }

  private def makeRow21: Elem = {
    <tr>
      {makeRowIndex(21)}
      {blankCells(11) /*                                                                   A21 to K21 */}
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;"> { /* L21 + M21 */ }
        Coordinates in rotated system
      </td>
      {blankCells(2) /*                                                                    N21 to O21 */}
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;"> { /* P21 + Q21 */ }
        BB coordinates after minimization
      </td>
      <td colSpan="2" rowSpan="2"  style="border:2px solid black;margin-bottom:2px;"> { /* R21 + S21 */ }
        sum of squares x^2 +y^2 for each isoTable angle
      </td>
      {blankCells(5) /*                                                                    T21 to X21 */}
    </tr>
  }

  private def makeRow22: Elem = {
    <tr>
      {makeRowIndex(22)}
      {blankCells(18) /*               Fill in columns that are not occupied by line 21.    A22 to X22 */}
    </tr>
  }

  /**
    * Extra blank row for aesthetics.
    * @return HTML for row
    */
  private def makeRow23: Elem = {
    <tr>
      {makeRowIndex(23)}
      {blankCells(24)}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(24)}
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
