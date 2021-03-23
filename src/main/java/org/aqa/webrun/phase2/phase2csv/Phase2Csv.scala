package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output

import java.io.File

abstract class Phase2Csv[T] extends Logging {

  // ----------------------------------------------------------------------------
  // Override these functions for specific data types:

  /**
    * Make a list of CSV columns.
    * @return List of columns.
    */
  protected def makeColList: Seq[Col]

  /**
    * Get the DICOM data for the given data set.
    * @param data Get for this data.
    * @return DICOM data.
    */
  protected def getAl(data: T): AttributeList

  /**
    * Get the SOP of the DICOM for this data set.
    * @param data Data using DICOM data.
    * @return SOP instance UID.
    */
  protected def getSopUID(data: T): String

  /**
    * Get the output associated with the data.
    * @param data Data associated with the output.
    * @return A DB output.
    */
  protected def getOutput(data: T): Output

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  protected def getData(machinePK: Long): Seq[T]

  /**
    * Used to name CSV file.
    */
  protected val dataName: String

  // ----------------------------------------------------------------------------

  /**
    * Define a column in a Phase2 CSV.
    *
    * @param header Name of column.
    * @param toText Converts given data to text that will be put in the cell.
    */
  case class Col(header: String, toText: T => Any) {}

  private val colList: Seq[Col] = makeColList

  private val metadataCache = new MetadataCache

  private val maintenanceCsv = new MaintenanceCsv(metadataCache)

  private val maintenanceRecordList = maintenanceCsv.retrieveGroupedAndOrderedList()

  private val prefixCsv = new PrefixCsv(metadataCache)
  private val machineDescriptionCsv = new MachineDescriptionCsv(metadataCache)
  private val dicomCsv = new DicomCsv

  //noinspection SpellCheckingInspection
  private val dicomCsvCacheDirName = "DICOMCSV"

  /**
    * Make the column header line of a CSV file.
    *
    * @return Single string of data headers.
    */
  def makeHeader(): String = {
    val dataHeaderList = colList.map(col => Util.textToCsv(col.header)).mkString(",")
    prefixCsv.headerText + "," + dataHeaderList + "," + machineDescriptionCsv.headerText + "," + dicomCsv.headerText
  }

  /**
    * Get the DICOM CSV text for the given data set.  Try getting if from the cache dir first.  If that fails,
    * then create it and save it in cache.
    *
    * @param dataSet Get CSV for DICOM referenced by this.
    * @param machine Machine that produced data.
    * @return CSV text for DICOM.
    */
  private def getDicomText(dataSet: T, machine: Machine): String = {
    val machDir = {
      val instDir = new File(Config.cacheDirFile, metadataCache.institutionNameMap(machine.institutionPK))
      val dicomDir = new File(instDir, dicomCsvCacheDirName)
      new File(dicomDir, machine.id)
    }
    val file = new File(machDir, getSopUID(dataSet) + ".csv")
    if (file.exists())
      Util.readTextFile(file).right.get
    else {
      val text = dicomCsv.dicomToText(getAl(dataSet))
      machDir.mkdirs()
      Util.writeFile(file, text)
      text
    }
  }

  private def msOfData(dataSet: T): Long = getOutput(dataSet).dataDate.get.getTime
  private def maintenanceListToCsv(maintenanceList: Seq[MaintenanceRecord]): Seq[String] = maintenanceList.map(maintenanceCsv.toCsvText)

  private def maintenanceBefore(dataSet: Option[T], mtMachList: Seq[MaintenanceRecord]): Seq[String] = {
    if (dataSet.isEmpty)
      Seq() // no maintenance records
    else {
      val csvList = maintenanceListToCsv(mtMachList.filter(_.creationTime.getTime <= msOfData(dataSet.get)))
      csvList
    }
  }

  private def maintenanceAfter(dataSet: Option[T], mtMachList: Seq[MaintenanceRecord]): Seq[String] = {
    if (dataSet.isEmpty)
      Seq() // no maintenance records
    else {
      val csvList = maintenanceListToCsv(mtMachList.filter(_.creationTime.getTime > msOfData(dataSet.get)))
      csvList
    }
  }

  /**
    * Make CSV content for maintenance records between the given data sets.
    * @param dataSetA Use for one time stamp.
    * @param dataSetB Use for the other time stamp.
    * @param mtMachList List of all maintenance records for a given machine.
    * @return
    */
  private def maintenanceBetween(dataSetA: T, dataSetB: T, mtMachList: Seq[MaintenanceRecord]): Seq[String] = {
    val loTime = Math.min(msOfData(dataSetA), msOfData(dataSetB))
    val hiTime = Math.max(msOfData(dataSetA), msOfData(dataSetB))
    val between = mtMachList.filter(mt => (mt.creationTime.getTime > loTime) && (mt.creationTime.getTime <= hiTime))
    val csvList = maintenanceListToCsv(between)
    csvList
  }

  /** List of all machines, sorted by institution so that all of the rows
    * for a given institution will be consecutive. */
  private val machineList = Machine.list
    .sortBy(_.institutionPK)
  // .filter(_.machinePK.get == 27) // put in to do specific machine

  /**
    * Make one row of a CSV file.
    *
    * @param dataList Contains data to format.
    * @param dataIndex Index of item in data list to format.
    * @param machine Treatment machine that generated the data.
    * @param mtMachList List of maintenance records for the given machine.
    * @return One line of the CSV
    */
  private def makeCsvRow(dataList: Seq[T], dataIndex: Int, machine: Machine, mtMachList: Seq[MaintenanceRecord]): String = {

    val dataSet = dataList(dataIndex)
    // Make all of the columns.
    val dataText = {
      // Make into a new string to avoid references to other classes.  This helps free memory.
      def colToText(col: Col) = new String(Util.textToCsv(col.toText(dataSet).toString))
      colList.map(colToText).mkString(",")
    }

    val prefixText = prefixCsv.toCsvText(getOutput(dataList(dataIndex)))
    val machineDescriptionText = machineDescriptionCsv.toCsvText(getOutput(dataSet))
    val dicomText = getDicomText(dataSet, machine)
    val csvRow = Seq(prefixText, dataText, machineDescriptionText, dicomText).mkString(",")

    val maintenanceText: Seq[String] = {
      if ((dataIndex + 1) < dataList.size) maintenanceBetween(dataSet, dataList(dataIndex + 1), mtMachList)
      else Seq()
    }

    val fullText: String = Seq(Seq(csvRow), maintenanceText).flatten.mkString("\n")
    fullText
  }

  /**
    * Generate the CSV text for one machine.
    * @param machine Given machine.
    * @return Single string of CSV text.
    */
  private def machineToCsv(machine: Machine): String = {
    Phase2Csv.clearAlCache()
    val mtMachList = maintenanceRecordList.filter(_.machinePK == machine.machinePK.get) // list of maintenance records for just this machine

    val dataList = getData(machine.machinePK.get) // data for this machine

    val precedingMaintenance = maintenanceBefore(dataList.headOption, mtMachList)
    val followingMaintenance = maintenanceAfter(dataList.lastOption, mtMachList)

    // make the row list for this one machine
    val machineRowList = dataList.indices.map(dataIndex => makeCsvRow(dataList, dataIndex, machine, mtMachList))
    val all = (precedingMaintenance ++ machineRowList ++ followingMaintenance).mkString(",\n")
    Phase2Csv.clearAlCache()
    all
  }

  /**
    * Get the CSV content for all institutions.
    *
    * @return The CSV content as a single string.
    */
  def csvContent: String = makeHeader() + "\n" + machineList.map(machine => machineToCsv(machine)).filter(_.nonEmpty).mkString("\n\n")

  def writeToFile(): Unit = {
    val start = System.currentTimeMillis()
    Phase2Csv.csvDir.mkdirs()
    val file = new File(Phase2Csv.csvDir, dataName + ".csv")
    Util.writeFile(file, csvContent)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath + " in " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }

}

object Phase2Csv {

  /** Keep a cache of recently fetched attribute lists. */
  private val alCache = {
    new scala.collection.mutable.HashMap[String, AttributeList]()
  }

  private def clearAlCache(): Unit = {
    alCache.clear()
  }

  /**
    * Location of cross-institutional CSV files.
    */
  val csvDir = new File(Config.cacheDirFile, "CSV")

  /**
    * Get the attribute list.  First try the cache.  If not there, then get it from the
    * database, put it in the cache, and then use it.
    *
    * @param SOPInstanceUID Unique slice identifier.
    * @return The corresponding attribute list.
    */
  def getAlBySop(SOPInstanceUID: String): AttributeList = {
    if (alCache.contains(SOPInstanceUID))
      alCache(SOPInstanceUID)
    else {
      val alList = DicomSeries.getBySopInstanceUID(SOPInstanceUID).flatMap(ds => ds.attributeListList)
      alList.foreach(al => alCache.put(Util.sopOfAl(al), al))

      // If for some reason there is a problem getting the DICOM from the database,
      // then assume it is not available.  Create an empty attribute list which will
      // result in a bunch of NA's being displayed.  This is not ideal, but better
      // than failing.  Technically even the SOPInstanceUID is not needed, but it
      // seems like it ought to be.
      if (!alCache.contains(SOPInstanceUID)) {
        val al = new AttributeList
        val attr = AttributeFactory.newAttribute(TagByName.SOPInstanceUID)
        attr.addValue(SOPInstanceUID)
        alCache.put(SOPInstanceUID, al)
      }
      alCache(SOPInstanceUID)
    }
  }
}
