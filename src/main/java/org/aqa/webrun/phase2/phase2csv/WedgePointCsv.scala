package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.DbSetup
import org.aqa.db.Output
import org.aqa.db.WedgePoint

class WedgePointCsv extends Phase2Csv[WedgePoint.WedgePointHistory] {

  // abbreviation for the long name
  type WH = WedgePoint.WedgePointHistory

  override val dataName: String = "WedgePoint"

  /**
    * Indicate how and if this is used as a baseline.  Will be one of:
    * <p/>
    * explicit : The user has explicitly indicated that this is to be used as a baseline.
    * <p/>
    * implicit : In lieu of an explicit baseline, this is being used.  Note that the
    * chronologically first reading will always be used as a baseline.
    * <p/>
    * [blank] : This is not a baseline.
    *
    * @param wh WedgePoint to be examined.
    * @return Baseline designation.
    */
  private def baselineDesignation(wh: WH): String = {
    (wh.wedgePoint.isBaseline, wh.wedgePoint.wedgePointPK.get == wh.baselineWedgePoint.wedgePointPK.get) match {
      case _ if wh.wedgePoint.isBaseline                                                 => "explicit"
      case _ if wh.wedgePoint.wedgePointPK.get == wh.baselineWedgePoint.wedgePointPK.get => "implicit"
      case _                                                                             => ""
    }

  }

  override protected def makeColList: Seq[CsvCol[WH]] = {
    Seq(
      CsvCol("Beam Name", "Common name of RTPLAN beam.", (wh: WH) => wh.wedgePoint.wedgeBeamName),
      CsvCol(
        "Baseline Designation",
        "explicit: Designated by user as a baseline.  implicit: Used as baseline when an explicit one is not defined.  If blank, then it is not used as a baseline.",
        (wh: WH) => baselineDesignation(wh)
      ),
      CsvCol("Wedge CU", "Average CU of pixels in the center of the wedge image.", (wh: WH) => wh.wedgePoint.wedgeValue_cu),
      CsvCol("Background Beam Name", "Common name of RTPLAN for background beam.", (wh: WH) => wh.wedgePoint.backgroundBeamName),
      CsvCol("Background CU", "For background beam image, average CU of pixels in the center.", (wh: WH) => wh.wedgePoint.backgroundValue_cu),
      CsvCol("Percent of Background", "Wedge CU / Background CU * 100", (wh: WH) => wh.wedgePoint.percentOfBackground_pct),
      CsvCol("Baseline Wedge CU", "For baseline wedge image, average CU of pixels in the center.", (wh: WH) => wh.baselineWedgePoint.wedgeValue_cu),
      CsvCol("Baseline Background CU", "For background baseline wedge image, average CU of pixels in the center.", (wh: WH) => wh.baselineWedgePoint.backgroundValue_cu),
      CsvCol("Baseline Percent of Background", "For background, wedge CU / background CU * 100", (wh: WH) => wh.baselineWedgePoint.percentOfBackground_pct)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[WH] = {
    val wedgeList = WedgePoint.history(machinePK)
    wedgeList
  }

  override def getOutput(data: WH): Output = data.output

  /**
    * Get the SOP of the DICOM for this data set.
    *
    * @param data   Data using DICOM data.
    * @param prefix Prefix column headers with this.
    * @return SOP instance UID.
    */
  override protected def getSopUID(data: WH, prefix: Option[String]): String = data.wedgePoint.wedgeSOPInstanceUID
}

object WedgePointCsv {

  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new WedgePointCsv).writeToFile()
  }

}