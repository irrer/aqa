package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLBeam
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.blankCell
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil.toHtml

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  * @param collimator Collimator data
  */
class SSCollimator(extendedData: ExtendedData, collimator: WLCollimator) extends SSSheet {

  override val name: String = "Collimator"

  /**
    * Makes cells A to L which are common to all collimator beams.
    * @param beam Beam to show.
    * @return cells for common content.
    */
  private def isoTableAngleCells(beam: WLBeam): Seq[Elem] = {

    val isoTableAngle0 = beam.collimatorAngle == 0

    val coll_X =
      if (isoTableAngle0)
        toHtmlYellow(collimator.getColl_X_Optimized)
      else
        blankCell

    val coll_Z =
      if (isoTableAngle0)
        toHtmlYellow(collimator.getColl_Z_Optimized)
      else
        blankCell

    Seq(
      toHtml(beam.gantryAngle), /*        A */
      toHtml(beam.collimatorAngle), /*    B */
      toHtml(beam.isoTableAngle), /*      C */
      toHtml(beam.wl.errorX_mm), /*       D */
      toHtml(beam.wl.errorY_mm), /*       E */
      toHtml(beam.caX), /*                F */
      toHtml(beam.caZ), /*                G */
      coll_X, /*                          H */
      coll_Z, /*                          I */
      toHtml(collimator.CA_Xpp(beam, collimator.getColl_X_Optimized)), /*                                 J */
      toHtml(collimator.CA_Zpp(beam, collimator.getColl_Z_Optimized)), /*                                 K */
      toHtml(collimator.CA_Rpp(beam, collimator.getColl_X_Optimized, collimator.getColl_Z_Optimized)), /* L */
      blankCell, /*                        M */
      blankCell /*                         N */
    )
  }

  private def makeRow1: Elem = {
    <tr>
      {makeRowIndex(1)}
      {blankCells(14) /*  A1 to N1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    <tr>
      {makeRowIndex(2)}
      {blankCells(7) /*                                                A2 to G2 */}
      <td colSpan="4" style="background:#DDEBF7;"> { /*                H2 to K2 */ }
        Max square of CA displacement   ->
      </td>
      {toHtmlPeach(collimator.getCA_Rpp_Optimized) /*                  L2 */}
      {toHtml("Solve for smallest max (Collimator Isocentricity)") /*  M2 */}
      {blankCell /*                                                    N2 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {makeRowIndex(3)}
      {toHtml("Collimator") /*  A3 */}
      {blankCells(4) /*         B3 to E3 */}
      {toHtml("CA-X") /*        F3 */}
      {toHtml("CA-Z") /*        G3 */}
      {toHtml("Coll-X") /*      H3 */}
      {toHtml("Coll-Z") /*      I3 */}
      {toHtml("CA-X\"") /*      J3 */}
      {toHtml("CA-Z\"") /*      K3 */}
      {toHtml("CA-R\"^2") /*    L3 */}
      {blankCells(2) /*         M3 to N3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {makeRowIndex(4)}
      {isoTableAngleCells(collimator.T__0) /*  A4 to N4 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {isoTableAngleCells(collimator.T_90) /*  A5 to N5 */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6)}
      {isoTableAngleCells(collimator.T270) /*  A6 to N6 */}
    </tr>
  }

  /**
    * Make an extra blank row.
    * @return HTML
    */
  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCells(14) /*                       A7 to N7 */}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(14)}
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
}
