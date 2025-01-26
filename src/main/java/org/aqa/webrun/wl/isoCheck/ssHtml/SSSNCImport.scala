package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.isoCheck.WLCollimator
import org.aqa.webrun.wl.isoCheck.WLIsoCheck
import org.aqa.webrun.wl.isoCheck.WLIsoTable
import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  * @param extendedData Metadata
  */
class SSSNCImport(extendedData: ExtendedData, isoCheck: WLIsoCheck, collimator: WLCollimator, isoTable: WLIsoTable) extends SSSheet {

  override val name: String = "SNCImport"

  private def makeRow1: Elem = {
    <tr>
      {makeRowIndex(1)}
      {blankCells(4) /*                            A1 to D1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    <tr>
      {makeRowIndex(2)}
      {blankCell /*                                A2  */}
      {toHtml(s"CBCT - Gantry Iso X (mm)") /*      B2  */}
      {toHtml(0 - isoCheck.isoX) /*                 C2  */}
      {blankCell /*                                D2  */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {makeRowIndex(3)}
      {blankCell /*                                A3  */}
      {toHtml(s"CBCT - Gantry Iso Y (mm)") /*      B3  */}
      {toHtml(0 - isoCheck.isoY) /*                 C3  */}
      {blankCell /*                                D3  */}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {makeRowIndex(4)}
      {blankCell /*                                A4  */}
      {toHtml("CBCT - Gantry Iso Z (mm)") /*       B4  */}
      {toHtml(0 - isoCheck.isoZ) /*                 C4  */}
      {blankCell /*                                D4  */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {blankCell /*                                           A5  */}
      {toHtml("Table - Gantry Iso X (mm)") /*                 B5  */}
      {toHtml(isoTable.get_IsoTable_X_Optimized - isoCheck.isoX) /*  C5  */}
      {blankCell /*                                           D5  */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6)}
      {blankCell /*                                                  A6  */}
      {toHtml("Table - Gantry Iso Z (mm)") /*                        B6  */}
      {toHtml(isoTable.get_IsoTable_Z_Optimized - isoCheck.isoZ) /*  C6  */}
      {blankCell /*                                                  D6  */}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCell /*                                           A7  */}
      {toHtml("Gantry Flex (mm)") /*                          B7  */}
      {toHtml(isoCheck.gantryFlex) /*                         C7  */}
      {blankCell /*                                           D7  */}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {makeRowIndex(8)}
      {blankCell /*                                           A8  */}
      {toHtml("Col-Gantry misalignment (mm)") /*              B8  */}
      {toHtml(isoCheck.collGantryMisalign) /*                 C8  */}
      {blankCell /*                                           D8  */}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {makeRowIndex(9)}
      {blankCell /*                                           A9  */}
      {toHtml("MLC offset (mm)") /*                           B9  */}
      {toHtml(isoCheck.mlcOffsetY) /*                         C9  */}
      {blankCell /*                                           D9  */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {makeRowIndex(10)}
      {blankCell /*                                           A10  */}
      {toHtml("Table Isocentricity (mm)") /*                  B10  */}
      {toHtml(Math.sqrt(isoTable.get_RSquared_Optimized)) /*  C10  */}
      {blankCell /*                                           D10  */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {makeRowIndex(11)}
      {blankCell /*                                           A11  */}
      {toHtml("Gantry Isocentricity (mm)") /*                 B11  */}
      {toHtml(isoCheck.gantryIsocentricity) /*                C11  */}
      {blankCell /*                                           D11  */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {makeRowIndex(12)}
      {blankCell /*                                           A12  */}
      {toHtml("Collimator Isocentricity (mm)") /*             B12  */}
      {toHtml(collimator.getCA_Rpp_Optimized) /*              C12  */}
      {blankCell /*                                           D12  */}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {makeRowIndex(13)}
      {blankCells(4) /* JJJ */}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(4)}
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
