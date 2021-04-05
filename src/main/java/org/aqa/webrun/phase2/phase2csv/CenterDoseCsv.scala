package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.CenterDose
import org.aqa.db.DbSetup
import org.aqa.db.Output

class CenterDoseCsv extends Phase2Csv[CenterDose.CenterDoseHistory] {

  // abbreviation for the long name
  type CD = CenterDose.CenterDoseHistory

  override val dataName: String = "Center Dose"

  override protected def makeColList: Seq[CsvCol[CD]] = {
    Seq(
      CsvCol("Beam Name", "Common name of RTPLAN beam.", (cd: CD) => cd.centerDose.beamName),
      CsvCol("Dose", "Average dose of pixels in the center of the image.  Center is offset by results from collimator offset test.", (cd: CD) => cd.centerDose.dose),
      CsvCol("Units", "Units that dose is in.", (cd: CD) => cd.centerDose.units)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[CD] = {
    val cdHistory = CenterDose.history(machinePK)
    cdHistory
  }

  override def getSopUID(data: CD): String = data.centerDose.SOPInstanceUID

  override def getOutput(data: CD): Output = data.output
}
object CenterDoseCsv {

  def main(args: Array[String]): Unit = {
    DbSetup.init
    (new CenterDoseCsv).updateFiles()
  }

}
