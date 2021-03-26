package org.aqa.webrun.phase2.phase2csv

import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output

case class DicomInstance(SOPInstanceUID: String) {}

/**
  * Populate all DICOM csv files that need it.
  *
  * This creates files in the cache directory for the creation of CSV files.
  */
class PopulateDicomCsv extends Phase2Csv[DicomInstance] {

  // abbreviation for the long name
  type DI = DicomInstance

  override val dataName: String = "PopulateDicom"

  override protected def makeColList: Seq[Col] = {
    Seq(
      Col("SOPInstanceUID", (di: DI) => di.SOPInstanceUID)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[DI] = {
    val seriesList = DicomSeries.getByMachine(machinePK)
    val sopUidList = seriesList.flatMap(s => s.sopInstanceUIDList.split(" ")).filter(_.nonEmpty)
    sopUidList.map(DicomInstance)
  }

  override def getOutput(data: DI): Output = {
    if (true) throw new RuntimeException("Should never call getOutput method.")
    null.asInstanceOf[Output]
  }

  /**
    * Get the SOP of the DICOM for this data set.
    *
    * @param data   Data using DICOM data.
    * @param prefix Prefix column headers with this.
    * @return SOP instance UID.
    */
  override protected def getSopUID(data: DI, prefix: Option[String]): String = data.SOPInstanceUID
}

object PopulateDicomCsv {
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val populateDicomCsv = new PopulateDicomCsv
    val machineList = Machine.list

    def populate(m: Machine): Unit = {
      val seriesList = DicomSeries.getByMachine(m.machinePK.get)

      def doSeries(series: DicomSeries): Unit = {
        val di = series.sopInstanceUIDList.split(" ").filter(_.nonEmpty).map(DicomInstance).head
        populateDicomCsv.getDicomText(di, m)
      }

      seriesList.foreach(series => doSeries(series))
    }

    machineList.foreach(m => populate(m))
    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed time: " + Util.elapsedTimeHumanFriendly(elapsed))
  }
}
