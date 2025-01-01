package org.aqa.webrun.wl.wlMonthly.ssHtml

import org.aqa.webrun.ExtendedData
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

  override val name: String = "Preprocess"

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
      "",
      "",
      "",
      "",
      "",
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
      {toHtml(monthly.G__0_C_90_T__0.gantryAngle)}
      {toHtml(monthly.G__0_C_90_T__0.collimatorAngle)}
      {toHtml(monthly.G__0_C_90_T__0.tableAngle)}
      {toHtml(monthly.G__0_C_90_T__0.wl.errorX_mm)}
      {toHtml(monthly.G__0_C_90_T__0.wl.errorY_mm)}
      {toHtml(monthly.G__0_C_90_T__0.caX)}
      {toHtml(monthly.G__0_C_90_T__0.caY)}
      {toHtml(monthly.G__0_C_90_T__0.caZ)}
      {toHtml(monthly.collXG__0)}
      {toHtml("")}
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
      {toHtml(monthly.G__0_C270_T__0.gantryAngle)}
      {toHtml(monthly.G__0_C270_T__0.collimatorAngle)}
      {toHtml(monthly.G__0_C270_T__0.tableAngle)}
      {toHtml(monthly.G__0_C270_T__0.wl.errorX_mm)}
      {toHtml(monthly.G__0_C270_T__0.wl.errorY_mm)}
      {toHtml(monthly.G__0_C270_T__0.caX)}
      {toHtml(monthly.G__0_C270_T__0.caY)}
      {toHtml(monthly.G__0_C270_T__0.caZ)}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
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
    </tr>
  }

  private def makeRow6: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(6)}
    </tr>
  }

  private def makeRow7: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(7)}
    </tr>
  }

  private def makeRow8: Elem = {
    <tr>
      {WLXlsxUtil.makeRowIndex(8)}
      {toHtml(monthly.G180_C_90_T__0.gantryAngle)}
      {toHtml(monthly.G180_C_90_T__0.collimatorAngle)}
      {toHtml(monthly.G180_C_90_T__0.tableAngle)}
      {toHtml(monthly.G180_C_90_T__0.wl.errorX_mm)}
      {toHtml(monthly.G180_C_90_T__0.wl.errorY_mm)}
      {toHtml(monthly.G180_C_90_T__0.caX)}
      {toHtml(monthly.G180_C_90_T__0.caY)}
      {toHtml(monthly.G180_C_90_T__0.caZ)}
      {toHtml(monthly.collXG180)}
      {toHtml("")}
      {toHtml(monthly.collZG180)}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml(monthly.gantryIsocentricity)}
      {toHtml("Gantry Isocentricity")}
      {toHtml(monthly.mlcDxG180_C_90)}
      {toHtml(monthly.mlcDyG180_C_90)}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
      {toHtml("")}
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
      </table>
    }

    content
  }
}
