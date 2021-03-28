package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.web.WebUtil
import org.aqa.webrun.phase2.phase2csv.Phase2Csv.csvDir

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

abstract class Phase2Csv[T] extends Logging {

  // ----------------------------------------------------------------------------
  // Override these functions for specific data types:

  /**
    * Make a list of CSV columns.
    * @return List of columns.
    */
  protected def makeColList: Seq[CsvCol[T]]

  /**
    * Get the SOP of the DICOM for this data set.
    * @param data Data using DICOM data.
    * @param prefix Prefix column headers with this.
    * @return SOP instance UID.
    */
  protected def getSopUID(data: T, prefix: Option[String] = None): String

  /**
    * Get the SOP of the second DICOM for this data set.
    * @param data Data using DICOM data.
    * @param prefix Prefix column headers with this.
    * @return SOP instance UID.
    */
  protected def getSopUID2(data: T, prefix: Option[String] = None): Option[String] = None

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

  private def fileBaseName = dataName.replaceAll(" ", "")

  private val colList: Seq[CsvCol[T]] = makeColList

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
  protected def getDicomText(dataSet: T, machine: Machine): String = {
    val machDir = {
      val instDir = new File(Config.cacheDirFile, metadataCache.institutionNameMap(machine.institutionPK))
      val dicomDir = new File(instDir, dicomCsvCacheDirName)
      new File(dicomDir, machine.id)
    }

    def fileOf(sop: String): File = new File(machDir, sop + ".csv")

    val file = fileOf(getSopUID(dataSet))
    if (file.exists())
      Util.readTextFile(file).right.get
    else {
      val alList = DicomSeries.getBySopInstanceUID(getSopUID(dataSet)).flatMap(ds => ds.attributeListList)

      def makeCsv(al: AttributeList): Unit = {
        val f = fileOf(Util.sopOfAl(al))
        if (!f.exists()) {
          machDir.mkdirs()
          val text = dicomCsv.dicomToText(al)
          Util.writeFile(f, text)
          Trace.trace("wrote file " + file.getAbsolutePath)
        }
      }

      alList.foreach(makeCsv)
      Util.garbageCollect()
      Util.readTextFile(fileOf(getSopUID(dataSet))).right.get
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
    try {
      val dataSet = dataList(dataIndex)
      // Make all of the columns.
      val dataText = {
        // Make into a new string to avoid references to other classes.  This helps free memory.
        def colToText(col: CsvCol[T]) = new String(Util.textToCsv(col.toText(dataSet).toString))
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
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected exception: " + fmtEx(t))
        ""
    }
  }

  /**
    * Generate the CSV text for one machine.
    * @param machine Given machine.
    * @return Single string of CSV text.
    */
  private def machineToCsv(machine: Machine): String = {
    val mtMachList = maintenanceRecordList.filter(_.machinePK == machine.machinePK.get) // list of maintenance records for just this machine

    val dataList = getData(machine.machinePK.get) // data for this machine

    val precedingMaintenance = maintenanceBefore(dataList.headOption, mtMachList)
    val followingMaintenance = maintenanceAfter(dataList.lastOption, mtMachList)

    // make the row list for this one machine
    val machineRowList = dataList.indices.map(dataIndex => makeCsvRow(dataList, dataIndex, machine, mtMachList)).filter(_.nonEmpty)
    val all = (precedingMaintenance ++ machineRowList ++ followingMaintenance).mkString(",\n")
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
    val file = new File(Phase2Csv.csvDir, fileBaseName + ".csv")
    Util.writeFile(file, csvContent)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath + " in " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }

  /**
    * Write a file documenting columns of this CSV.
    */
  def writeDoc(): Unit = {

    val colList: Seq[CsvCol[_]] = prefixCsv.colList ++ makeColList ++ machineDescriptionCsv.colList ++ dicomCsv.colList

    val name = "Definitions for " + dataName + " CSV columns."
    val content: Elem = {
      <div class="col-md-10 col-md-offset-1 ">
        <h2>{name}</h2>
        <table class="table table-bordered">
          {colList.map(col => col.doc)}
        </table>
      </div>
    }
    val text = WebUtil.wrapBody(content, name)
    Phase2Csv.csvDir.mkdirs()
    val file = new File(Phase2Csv.csvDir, fileBaseName + ".html")
    Util.writeFile(file, text)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath)
  }

  def generateIndex() : Unit = {
      val csvList = Util.listDirFiles(csvDir).filter(_.getName.endsWith(".csv"))

      def fileToRow(file: File): Elem = {

        val date = {
          val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm")
          dateFormat.format(new Date(file.lastModified()))
        }

        <tr>
          <td>
            {file.getName.replaceAll(".csv$", "")}
          </td>
          <td>
            <a href={file.getName}>Download CSV</a>
          </td>
          <td>
            {date}
          </td>
          <td>
            <a href={file.getName.replaceAll(".csv$", ".html")}>Column Definitions</a>
          </td>
        </tr>
      }

      val rowList = csvList.map(fileToRow)

      val header = {
        <tr>
          <td><b>Name</b></td>
          <td><b>CSV</b></td>
          <td><b>Generated</b></td>
          <td><b>Column Definitions</b></td>
        </tr>
      }

      val content = {
        <div class="col-md-8 col-md-offset-2">
          <center>
            <h2>Index of CSV Files</h2>
            <em>These files are periodically generated and represent data from all institutions.</em>
          </center>
          <table class="table table-bordered" style="margin:30px;">
            {csvList.map(fileToRow)}
          </table>
        </div>
      }

      val text = WebUtil.wrapBody(content, "CSV Index")
      Phase2Csv.csvDir.mkdirs()
      val file = new File(Phase2Csv.csvDir, "index.html")
      Util.writeFile(file, text)
      logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath)
  }

  def updateFiles: Unit = {
    try {
      writeDoc()
      writeToFile()
      generateIndex()
    } catch {
      case t: Throwable => logger.warn("Error updating CSV: " + fmtEx(t))
    }
  }

}

object Phase2Csv extends Logging {

  /**
    * Location of cross-institutional CSV files.
    */
  val csvDir = new File(Config.resultsDirFile, "CSV")


}
