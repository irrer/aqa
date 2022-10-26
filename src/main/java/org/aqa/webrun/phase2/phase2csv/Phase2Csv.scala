/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2.phase2csv

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.MachineUpdate
import org.aqa.web.WebServer
import org.aqa.web.WebUtil

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
    * @return SOP instance UID.
    */
  protected def getSopUID(data: T): Option[String]

  /**
    * If true, the DICOM metadata associated with <code>getSopUID</code> will be added.
    */
  protected val showDicomMetadata: Boolean = true

  /**
    * Allow the subclass to specify the prefix for each DICOM CSV column header.
    */
  protected val dicomHeaderPrefix: Option[String] = None

  /**
    * If there is a second DICOM file involved for the data set, then the subclass should
    * specify the prefix for each DICOM CSV column header to distinguish it from the first set of DICOM data.
    */
  protected val dicom2HeaderPrefix: Option[String] = None

  /**
    * Get the SOP of the second DICOM image for this data set.
    *
    * This is only used if <code>dicom2HeaderPrefix</code> is defined.
    *
    * @param data Data using DICOM data.
    * @return Header prefix and second SOP instance UID.
    */
  protected def getSopUID2(data: T): String = ""

  /**
    * Get the output associated with the data.
    *
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

  def getDataName: String = dataName

  private val colList: Seq[CsvCol[T]] = makeColList

  private val metadataCache = MetadataCache.metadataCache

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
    prefixCsv.headerText + "," + dataHeaderList + "," + machineDescriptionCsv.headerText + "," + dicomCsv.headerText() + {
      if (dicom2HeaderPrefix.isEmpty)
        ""
      else {
        "," + dicomCsv.headerText(dicom2HeaderPrefix)
      }
    }
  }

  /**
    * Get the DICOM CSV text for the given data set.  Try getting if from the cache dir first.  If that fails,
    * then create it and save it in cache.
    *
    * @param sopUid Get CSV for DICOM referenced by this.
    * @param machine Machine that produced data.
    * @return CSV text for DICOM.
    */
  protected def getDicomText(sopUid: String, machine: Machine): String = {
    val machDir = {
      val instDir = new File(Config.cacheDirFile, metadataCache.institutionNameMap(machine.institutionPK))
      val dicomDir = new File(instDir, dicomCsvCacheDirName)
      new File(dicomDir, machine.id)
    }

    def fileOf(sop: String): File = new File(machDir, sop + ".csv")

    val file = fileOf(sopUid)
    if (file.exists())
      Util.readTextFile(file).right.get
    else {
      val alList = DicomSeries.getBySopInstanceUID(sopUid).flatMap(ds => ds.attributeListList)

      def makeCsv(al: AttributeList): Unit = {
        val f = fileOf(Util.sopOfAl(al))
        if (!f.exists()) {
          Util.mkdirs(machDir)
          val text = dicomCsv.dicomToText(al)
          Util.writeFile(f, text)
        }
      }

      alList.foreach(makeCsv)
      Util.garbageCollect()
      Util.readTextFile(fileOf(sopUid)).right.get
    }
  }

  private def msOfData(dataSet: T): Long = getOutput(dataSet).dataDate.get.getTime
  private def maintenanceListToCsv(maintenanceList: Seq[MaintenanceRecord]): Seq[String] = maintenanceList.map(MaintenanceCsv.toCsvText)

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
  // private val machineList = Machine.list
  // .sortBy(_.institutionPK)
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

      val dicomText: Option[String] = {
        val sop = getSopUID(dataSet)
        if (sop.isDefined)
          Some(getDicomText(sop.get, machine))
        else
          None
      }

      val dicomText2: Option[String] = {
        if (dicom2HeaderPrefix.isEmpty)
          None
        else {
          Some(getDicomText(getSopUID2(dataSet), machine))
        }
      }

      val csvRow = {
        val text = Seq(Some(prefixText), Some(dataText), Some(machineDescriptionText), dicomText).flatten.mkString(",")
        if (dicomText2.isEmpty)
          text
        else
          text + "," + dicomText2.get
      }

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
    val start = System.currentTimeMillis()
    val prefix = dataName + " / " + machine.id + " "
    logger.info(prefix + "starting")
    // changed this so it would not put maintenance records in data.
    val mtMachList: Seq[MaintenanceRecord] = Seq() // MaintenanceRecord.getByMachine(machine.machinePK.get) // list of maintenance records for just this machine

    logger.info(prefix + "getting data")
    val dataList = getData(machine.machinePK.get) // data for this machine
    logger.info(prefix + "data size " + dataList.size)

    logger.info(prefix + "getting maintenance records")
    val precedingMaintenance = maintenanceBefore(dataList.headOption, mtMachList)
    val followingMaintenance = maintenanceAfter(dataList.lastOption, mtMachList)

    logger.info(prefix + "making rows")
    // make the row list for this one machine
    val machineRowList = dataList.indices.map(dataIndex => makeCsvRow(dataList, dataIndex, machine, mtMachList)).filter(_.nonEmpty)
    logger.info(prefix + "number of rows " + machineRowList.size)
    val all = (precedingMaintenance ++ machineRowList ++ followingMaintenance).mkString(",\n")
    logger.info(prefix + "done. Elapsed time: " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
    all
  }

  /**
    * Get the CSV content for all institutions.
    *
    * @return The CSV content as a single string.
    */
  private def csvContent(machineSeq: Seq[Machine]): String = {
    val machineList = machineSeq.sortBy(_.institutionPK)
    val content = machineList.map(machine => machineToCsv(machine)).filter(_.nonEmpty).mkString("\n\n")
    makeHeader() + "\n" + content
  }

  def writeDoc(): Unit = {
    val colList = {
      val cl = prefixCsv.colList ++ makeColList ++ machineDescriptionCsv.colList
      if (showDicomMetadata)
        cl :+ dicomCsv.colList
      else
        cl
    }
    Phase2Csv.writeDoc(colList.asInstanceOf[Seq[CsvCol[Any]]], dataName)
  }

  def writeToFile(csvDir: File, machineSeq: Seq[Machine]): Unit = {
    Phase2Csv.writeToFile(csvDir, csvContent(machineSeq), dataName)
  }
}

object Phase2Csv extends Logging {

  /**
    * Location of CSV files that include data from all institutions in the consortium.
    */
  val consortiumCsvDir = new File(Config.resultsDirFile, "CSV")

  val metadataCache: MetadataCache = MetadataCache.metadataCache

  private def fileBaseName(dataName: String) = dataName.replaceAll(" ", "")

  private def csvDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")

  def csvFileName(fileBaseName: String): String = fileBaseName + "_" + csvDateFormat.format(new Date()) + ".csv"

  /** Name of file that contains all of the CSV files. */
  //noinspection SpellCheckingInspection
  val zipFileName = "AQAcsv.zip"

  /** Name of file containing documentation on CSV files. */
  val notesFileName = "CSVNotes.html"

  /** Tag in original HTML to be replaced by notes. */
  val notesTag: String = "@@@@" + notesFileName + "@@@@"

  /**
    * Get the notes for CSV from a static HTML file.
    *
    * @return On success, the contents of the file, otherwise an empty string.
    */
  private def readNotes(): String = {
    val notesFile = new File(Config.staticDirFile, notesFileName)
    Util.readTextFile(notesFile) match {
      case Right(text) => text
      case Left(t) =>
        logger.warn("Could not read CSV notes file " + notesFile.getAbsolutePath + " : " + fmtEx(t))
        ""
    }
  }

  private def machineAliasTable(institutionPK: Long): Elem = {
    val machineSeq = metadataCache.machineMap.values.filter(m => m.institutionPK == institutionPK).toSeq

    def sortMach(a: Machine, b: Machine): Boolean = {
      val aName = a.getRealId
      val bName = b.getRealId

      val aNum = aName.replaceAll("[^0-9]", "")
      val bNum = bName.replaceAll("[^0-9]", "")

      if (aNum.isEmpty || bNum.isEmpty) aName.compareTo(bName) <= 0
      else aNum.toLong <= bNum.toLong
    }

    val orderedSeq = machineSeq.sortWith(sortMach)

    def toRow(machine: Machine): Elem = {
      <tr>
        <td>{MachineUpdate.linkToMachineUpdate(machine.machinePK.get, machine.id)}</td>
        <td>{machine.id}</td>
      </tr>
    }

    val content = {
      <div class="row">
        <h3 style="margin-top:30px;">Machine Alias Table</h3>
        <div class="col-md-6 col-md-offset-3">
          <table class="table table-bordered">
            <tr>
              <th title="Click for details.">Machine</th>
              <th>Alias</th>
            </tr>{orderedSeq.map(toRow)}
          </table>
        </div>
      </div>
    }
    content
  }

  /**
    * Generate a user friendly web page to act as an index and write it as a file.  This
    * also creates the ZIP file which contains all the CSV files.
    */
  def generateIndex(institutionPK: Long): Unit = {
    val csvDir = institutionCsvDir(institutionPK)
    val csvList = Util.listDirFiles(csvDir).filter(_.getName.endsWith(".csv"))

    val dataCount = Output.getCount(institutionPK, Procedure.ProcOfPhase2.get.procedurePK.get)

    def fileToRow(csvFile: File): Elem = {

      val date = {
        val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm")
        dateFormat.format(new Date(csvFile.lastModified()))
      }

      val suffixPattern = ".....................csv$"

      val docFileReference = {
        val docFile = new File(Phase2Csv.consortiumCsvDir, csvFile.getName.replaceAll(suffixPattern, ".html"))
        WebServer.urlOfResultsFile(docFile)
      }

      <tr>
        <td>
          <a href={csvFile.getName}>{csvFile.getName.replaceAll(suffixPattern, "")}</a>
        </td>
        <td>
          {date}
        </td>
        <td>
          <a href={docFileReference}>Column Definitions</a>
        </td>
      </tr>
    }

    val content = {
      <div class="col-md-8 col-md-offset-2">
        <center>
          <h2>Index of CSV Files</h2>
          <em>These files are periodically generated and contain data from <span aqaalias="">{metadataCache.institutionNameMap(institutionPK)}</span>.</em>
          <em>Number of data sets: {dataCount.toString}</em>
        </center>
        <table class="table table-bordered" style="margin:30px;">
          <tr>
            <th>
              Type of data.  Click to download
            </th>
            <th>
              Date generated
            </th>
            <th>
              Description of Columns
            </th>
          </tr>
          {csvList.map(fileToRow)}
        </table>
        <center>
          <a href={zipFileName}>Download zipped version of all CSV files.</a>
        </center>
        <center>
          {machineAliasTable(institutionPK)}
        </center>
        {notesTag}
      </div>
    }

    val zipFile = new File(csvDir, zipFileName)
    FileUtil.readFileTreeToZipFile(csvList, excludePatternList = Seq(), excludeFileList = Seq(), zipFile)

    val text = WebUtil.wrapBody(content, "CSV Index").replace(notesTag, readNotes())
    Util.mkdirs(csvDir)
    val file = new File(csvDir, "index.html")
    Util.writeFile(file, text)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath)
  }

  def institutionCsvDir(institutionPK: Long): File = {
    val d = new File(Config.resultsDirFile, MetadataCache.metadataCache.institutionNameMap(institutionPK))
    val dir = new File(d, "CSV")
    dir
  }

  /**
    * Write a file documenting columns of this CSV.  All documentation files are put
    * in the consortium CSV directory.
    */
  def writeDoc(colList: Seq[CsvCol[Any]], dataName: String): Unit = {
    val name = "Definitions for " + dataName + " CSV columns."
    val content: Elem = {
      <div class="col-md-10 col-md-offset-1 ">
        <h2>{name}</h2>
        <table class="table table-bordered">
          <tr>
            <th>Column</th>
            <th>Column Title</th>
            <th>Description</th>
          </tr>
          {colList.zipWithIndex.map(colIndex => colIndex._1.doc(colIndex._2))}
        </table>
      </div>
    }
    val text = WebUtil.wrapBody(content, name)
    Util.mkdirs(Phase2Csv.consortiumCsvDir)
    val file = new File(Phase2Csv.consortiumCsvDir, fileBaseName(dataName) + ".html")
    Util.writeFile(file, text)
    logger.info("Wrote " + file.length() + " bytes to file " + file.getAbsolutePath)
  }

  private def deleteCsvFiles(csvDir: File, dataName: String): Unit = {
    val toDeleteList = Util.listDirFiles(csvDir).filter(f => f.getName.matches(fileBaseName(dataName) + "_" + ".*.csv$"))
    toDeleteList.map(f => f.delete())
  }

  /**
    * Write the CSV content to a file.
    */
  def writeToFile(csvDir: File, csvContent: String, dataName: String): Unit = {
    val start = System.currentTimeMillis()
    Util.mkdirs(csvDir)
    deleteCsvFiles(csvDir, dataName)
    val csvFile = new File(csvDir, Phase2Csv.csvFileName(fileBaseName(dataName)))
    Util.writeFile(csvFile, csvContent)
    logger.info("Wrote " + csvFile.length() + " bytes to file " + csvFile.getAbsolutePath + " in " + Util.elapsedTimeHumanFriendly(System.currentTimeMillis() - start))
  }
}
