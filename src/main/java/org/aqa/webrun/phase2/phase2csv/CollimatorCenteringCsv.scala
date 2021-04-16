package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.CollimatorCentering
import org.aqa.db.DbSetup
import org.aqa.db.Output

class CollimatorCenteringCsv extends Phase2Csv[CollimatorCentering.ColCentHistory] {

  // abbreviation for the long name
  type CCH = CollimatorCentering.ColCentHistory

  override val dataName: String = "Collimator Centering"

  private def offset(colCent: CollimatorCentering): Double = {
    val x = colCent.xCollimatorCenter_mm
    val y = colCent.yCollimatorCenter_mm
    Math.sqrt(x * x + y * y)
  }

  override protected def makeColList: Seq[CsvCol[CCH]] = {
    Seq(
      CsvCol("X Center", "X position of center of rotation in mm", (cch: CCH) => cch.colCent.xCollimatorCenter_mm),
      CsvCol("Y Center", "Y position of center of rotation in mm", (cch: CCH) => cch.colCent.yCollimatorCenter_mm),
      CsvCol("Total Offset", "Total offset from center: sqrt(X*X + Y*Y) in mm", (cch: CCH) => offset(cch.colCent)),
      CsvCol("X1 90", "X1 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.X1_090_mm),
      CsvCol("X2 90", "X2 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.X2_090_mm),
      CsvCol("Y1 90", "Y1 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.Y1_090_mm),
      CsvCol("Y2 90", "Y2 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.Y2_090_mm),
      CsvCol("X1 270", "X1 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.X1_270_mm),
      CsvCol("X2 270", "X2 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.X2_270_mm),
      CsvCol("Y1 270", "Y1 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.Y1_270_mm),
      CsvCol("Y2 270", "Y2 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.Y2_270_mm)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[CCH] = {
    val cchList = CollimatorCentering.history(machinePK)
    cchList
  }

  override def getOutput(data: CCH): Output = data.output

  /**
    * Get the SOP of the DICOM for this data set.
    *
    * @param data Data using DICOM data.
    * @return SOP instance UID.
    */
  override protected def getSopUID(data: CCH): String = data.colCent.SOPInstanceUID270

  override protected val dicomHeaderPrefix: Option[String] = Some("C 90")
  override protected val dicom2HeaderPrefix: Option[String] = Some("C 270")

  override protected def getSopUID2(data: CCH): String = data.colCent.SOPInstanceUID090
}

object CollimatorCenteringCsv {
  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new CollimatorCenteringCsv).updateFiles()
    Phase2Csv.generateIndex()
  }

}
