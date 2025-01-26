package org.aqa.webrun.wl.isoCheck.ssHtml

import org.aqa.webrun.wl.isoCheck.WLXlsxUtil._

import scala.xml.Elem

/**
  * Construct HTML showing the Data sheet.
  *
  */
class SSInstructions() extends SSSheet {

  override val name: String = "Instructions"

  private def makeRow1: Elem = {
    <tr>
      {makeRowIndex(1)}
      {blankCells(7) /*             A1 to G1 */}
    </tr>
  }

  private def makeRow2: Elem = {
    <tr>
      {makeRowIndex(2)}
      {blankCells(2) /*             A2 to B2 */}
      {toHtml("Insert Data:") /*    C3       */}
      {blankCells(4) /*             D3 to G3 */}
    </tr>
  }

  private def makeRow3: Elem = {
    <tr>
      {makeRowIndex(3)}
      {blankCells(3) /*                 A3 to C3 */}
      {toHtml("1") /*                   D3       */}
      {toHtml("Use Data worksheet") /*  E3       */}
      {blankCells(2) /*                 F3 to G3 */}
    </tr>
  }

  private def makeRow4: Elem = {
    <tr>
      {makeRowIndex(4)}
      {blankCells(3) /*                 A4 to C4 */}
      {toHtml("2") /*                   D4       */}
      {toHtml("use QASRSWL patient; take CBCT and make shifts.  After initial CBCT take a second CBCT") /*  E4 */}
      {blankCells(2) /*                 F4 to G4 */}
    </tr>
  }

  private def makeRow5: Elem = {
    <tr>
      {makeRowIndex(5)}
      {blankCells(3) /*                 A5 to C5 */}
      {toHtml("3") /*                   D5       */}
      {toHtml("Shoot MV images in the order shown on the DATA worksheet") /* E5 */}
      {blankCells(2) /*                 F5 to G5 */}
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {makeRowIndex(6)}
      {blankCells(3) /*                 A6 to C6 */}
      {toHtml("6") /*                   D6       */}
      {toHtml("Find the centroid of the radiation target ball in the contouring spreadsheet.  ") /* E6 */}
      {blankCells(2) /*                 F6 to G6 */}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {makeRowIndex(7)}
      {blankCells(4) /*                               A7 to D7 */}
      {toHtml("4a") /*                                E7       */}
      {toHtml("Make a high resolution structure") /*  F7       */}
      {blankCell /*                                   G7 */}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {makeRowIndex(8)}
      {blankCells(4) /*                 A8 to D8 */}
      {toHtml("4b") /*                  E8       */}
      {toHtml("Set the \"apply area\" around the ball pretty tight") /*  F8 */}
      {blankCell /*                     G8 */}
    </tr>
  }

  private def makeRow9: Elem = {
    <tr>
      {makeRowIndex(9)}
      {blankCells(4) /*                 A9 to D9 */}
      {toHtml("4c") /*                  E9       */}
      {toHtml("Use thresholding contouring; for ceramic ball use threshold of 1000-7000, for tungsten ball use 6000-7000 threshold") /*  F9  */}
      {blankCell /*                     G9 */}
    </tr>
  }

  private def makeRow10: Elem = {
    <tr>
      {makeRowIndex(10)}
      {blankCells(4) /*                 A10 to D10 */}
      {toHtml("4d") /*                  E10        */}
      {
      toHtml(
        "Use centroid report tool in eclipse to find center of ball and origin of acquired image; Make sure to use the correct image, the tool will usually report the data from the first CBCT, and you want data from the last (probably)"
      ) /*                              F10        */
    }
      {blankCell /*                     G10        */}
    </tr>
  }

  private def makeRow11: Elem = {
    <tr>
      {makeRowIndex(11)}
      {blankCells(3) /*                 A11 to C11 */}
      {toHtml("5") /*                   D11        */}
      {toHtml("Find center of ball in second CBCT (after move) and insert ball centroid and origin in A23-C24") /*  E11       */}
      {blankCells(2) /*                 F11 to G11 */}
    </tr>
  }

  private def makeRow12: Elem = {
    <tr>
      {makeRowIndex(12)}
      {blankCells(3) /*                 A12 to C12 */}
      {toHtml("6") /*                   D12        */}
      {toHtml("Copy the data from A1-Y17 area, the order of Table, gantry and collimator must be maintained") /*  E12       */}
      {blankCells(2) /*                 F12 to G12 */}
    </tr>
  }

  private def makeRow13: Elem = {
    <tr>
      {makeRowIndex(13)}
      {blankCells(3) /*                 A13 to C13 */}
      {toHtml("7") /*                   D13        */}
      {toHtml("In Analysis worksheet go to the data menu in excel and select solver.  If Solver is not installed, install it.  Press the solve button.") /*  E13  */}
      {blankCells(2) /*                 F13 to G13 */}
    </tr>
  }
  private def makeRow14: Elem = {
    <tr>
      {makeRowIndex(14)}
      {blankCells(3) /*                 A14 to C14 */}
      {toHtml("8") /*                   D14        */}
      {toHtml("In the Collimator worksheet go to the data menu in excel and select solver.  Press the solve button.") /*  E14  */}
      {blankCells(2) /*                 F14 to G14 */}
    </tr>
  }

  private def makeRow15 = {
    <tr>
      {makeRowIndex(15)}
      {blankCells(3) /*                 A15 to C15 */}
      {toHtml("9") /*                   D15        */}
      {toHtml("If everything has gone well the report worksheet should contain the results of the analysis") /*  E15  */}
      {blankCells(2) /*                 F15 to G15 */}
    </tr>
  }

  private def makeRow16: Elem = {
    <tr>
      {makeRowIndex(16)}
      {blankCells(3) /*                 A16 to C16 */}
      {toHtml("10") /*                  D16        */}
      {toHtml("Go to the first sheet, labelled \"SNCImport\", save it as a CSV, and upload it into SunCHECK") /*  E16  */}
      {blankCells(2) /*                 F16 to G16 */}
    </tr>
  }

  private def makeRow17: Elem = {
    <tr>
      {makeRowIndex(17)}
      {blankCells(7) /*                 A17 to G17 */}
    </tr>
  }

  override def make(): Elem = {
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(7)}
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
      </table>
    }

    content
  }
}
