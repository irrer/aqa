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
class SSSNCImport(extendedData: ExtendedData, isoCheck: WLIsoCheck, collimator: WLCollimator, isoTable: Option[WLIsoTable]) extends SSSheet {

  override val name: String = "SNCImport"

  private val buffer = new StringBuffer()
  private def appendBuf(text1: String, text2: String): Unit = {
    val row = Seq("", text1, text2).mkString(",") + "\n"
    buffer.append(row)
  }

  private def makeCsvRow(row: Int, text: String, value: Double): Elem = {
    if (value.isNaN) {
      appendBuf(text, "")

      <tr>
        {makeRowIndex(row)}
        {blankCell /*     A  */}
        {toHtml(text) /*  B  */}
        {toHtml("") /*    C  */}
        {blankCell /*     D  */}
      </tr>
    } else {
      appendBuf(text, value.toString)
      <tr>
        {makeRowIndex(row)}
        {blankCell /*      A  */}
        {toHtml(text) /*   B  */}
        {toHtml(value) /*  C  */}
        {blankCell /*      D  */}
      </tr>
    }
  }

  private def makeRow1: Elem = {
    appendBuf("", "")
    <tr>
      {makeRowIndex(1)}
      {blankCells(4) /*                            A1 to D1 */}
    </tr>
  }

  private def makeRow15: Elem = {
    <tr>
      {makeRowIndex(15)}
      {blankCells(4) /*                            A1 to D1 */}
    </tr>
  }

  /** SNCImport C13    Maximum Winston-Lutz error. */
  val maxR: Double = {
    val biggest =
      if (isoTable.isDefined)
        Math.max(isoCheck.maxR, isoTable.get.maxR)
      else
        isoCheck.maxR
    biggest
  }

  override def make(): Elem = {
    val hasTable = isoTable.isDefined
    val content = {
      <table class="table table-bordered">
        {makeAlphaRow(4)}
        {makeRow1}
        {makeCsvRow(2, "CBCT - Gantry Iso X (mm)", -isoCheck.isoX)}
        {makeCsvRow(3, s"CBCT - Gantry Iso Y (mm)", -isoCheck.isoY)}
        {makeCsvRow(4, "CBCT - Gantry Iso Z (mm)", -isoCheck.isoZ)}
        {makeCsvRow(5, "Table + Gantry Iso X (mm)", if (hasTable) isoTable.get.get_IsoTable_X_Optimized + isoCheck.isoX else Double.NaN)}
        {makeCsvRow(6, "Table + Gantry Iso Z (mm)", if (hasTable) isoTable.get.get_IsoTable_Z_Optimized + isoCheck.isoZ else Double.NaN)}
        {makeCsvRow(7, "Gantry Flex (mm)", isoCheck.gantryFlex)}
        {makeCsvRow(8, "Col-Gantry misalignment (mm)", isoCheck.collGantryMisalign)}
        {makeCsvRow(9, "MLC offset (mm)", isoCheck.mlcOffsetY)}
        {makeCsvRow(10, "Table Isocentricity (mm)", if (hasTable) Math.sqrt(isoTable.get.get_RSquared_Optimized) else Double.NaN)}
        {makeCsvRow(11, "Gantry Isocentricity (mm)", isoCheck.gantryIsocentricity)}
        {makeCsvRow(12, "Collimator Isocentricity (mm)", collimator.get_CA_Rpp_Optimized)}
        {makeCsvRow(13, "Maximum R (mm)", maxR)}
        {makeCsvRow(14, "Maximum R Table 0 (mm)", isoCheck.maxR)}
        {makeRow15}
      </table>
    }

    content
  }

  /**
    * Get the CSV content for the SNC import.
    * @return CSV content as text.
    */
  def csvContent: String = buffer.toString

}
