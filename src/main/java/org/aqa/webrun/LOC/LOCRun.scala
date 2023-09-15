package org.aqa.webrun.LOC

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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.apache.poi.ss.usermodel.Workbook
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.LOCBaseline.LOCBaselineRunReq
import org.aqa.webrun.LOCBaseline.LOCFindRunReq
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem
import scala.xml.XML

/**
  * Run LOC code.
  */
class LOCRun(procedure: Procedure) extends WebRunProcedure with RunTrait[LOCRunReq] {

  /** Name of file that Matlab program creates and puts data in. */
  private val outputXmlFileName = "output.xml"

  /** Name of subdirectory under the output directory containing baseline files. */
  private val baselineDirName = "baseline"

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(Util.isRtimage)

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  override def getProcedure: Procedure = procedure

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val rtimageList = getRtimageList(alList)

    def getTimestamp(dateTag: AttributeTag, timeTag: AttributeTag): Option[Timestamp] = {
      val msList = rtimageList.flatMap(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).map(dt => dt.getTime)
      if (msList.isEmpty)
        None
      else
        Some(new Timestamp(msList.min))
    }

    val contentTime = getTimestamp(TagFromName.ContentDate, TagFromName.ContentTime)
    if (contentTime.isDefined)
      contentTime
    else
      getTimestamp(TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)

  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    val list = getRtimageList(alList).map(al => Util.patientIdOfAl(al)).distinct
    list.headOption
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val dsnList = getRtimageList(alList).flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  /**
    * Make the run requirements from the attribute lists.
    */
  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], output: Option[Output]): LOCRunReq = {
    val rtimageList = getRtimageList(alList)
    val deviceSerialNumber = getMachineDeviceSerialNumberList(rtimageList, Seq()).head
    val locBaselineRunReq = getLocBaselineRunReq(deviceSerialNumber)
    val result = LOCRunReq(rtimageList, locBaselineRunReq.get)
    result
  }

  override def validateRedo(outputPK: Long): Option[String] = {
    val fail = Some("Redo not possible because a LOC Baseline can not be found for this machine.  This can be caused by the user having deleted the LOC Baseline analysis.")
    try {
      val machinePK = Output.get(outputPK).get.machinePK
      val deviceSerialNumber = Machine.get(machinePK.get).get.serialNumber.get
      if (getLocBaselineRunReq(deviceSerialNumber).isDefined)
        None
      else
        fail
    } catch {
      case _: Throwable => fail
    }
  }

  /**
    * Look in the machine configuration directory for this machine and then ensure that there are OPEN and TRANS files.
    *
    * Note: This function should be deprecated when all LOC baseline files are put in the database.
    *
    * @param deviceSerialNumber Device serial number
    * @return True if there are baseline files.
    */
  private def getBaselineByFile(deviceSerialNumber: String): Option[LOCBaselineRunReq] = {
    val machineList = Machine.findMachinesBySerialNumber(deviceSerialNumber)
    if (machineList.isEmpty)
      None
    else {
      val machineConfigDir = machineList.head.configDir
      val dirList = Util.listDirFiles(machineConfigDir.get)
      val hasFiles = {
        machineConfigDir.isDefined &&
        machineConfigDir.get.isDirectory &&
        dirList.exists(_.getName.contains("OPEN")) &&
        dirList.exists(_.getName.contains("TRANS"))
      }
      if (hasFiles) {
        val open = dirList.find(_.getName.contains("OPEN")).get
        val trans = dirList.find(_.getName.contains("TRANS")).get
        val openAl = new DicomFile(open).attributeList.get
        val transAl = new DicomFile(trans).attributeList.get
        val locBaselineRunReq = Some(LOCBaselineRunReq(openAl, transAl))
        locBaselineRunReq
      } else
        None
    }
  }

  /**
    * Get the most recent LOC baseline files from the database.
    * @param serialNumber Device serial number uniquely defines the machine.
    *
    * @return Either an error message or the baseline.
    */
  private def getMostRecentBaselineFromDb(serialNumber: String): Either[String, LOCBaselineRunReq] = {
    try {
      val machine = Machine.findMachinesBySerialNumber(serialNumber).head
      val output = Output.getMostRecentLOCBaselineOutput(machine.machinePK.get).get
      val ds = DicomSeries.getByInputPK(output.inputPK)
      val alList = ds.head.attributeListList
      val baselineRunReq = LOCFindRunReq.constructRunReq(alList)
      logger.info("Retrieved LOC Baseline from database.")
      baselineRunReq
    } catch {
      case _: Throwable => Left("Unable to get LOC baseline from database.")
    }
  }

  private def getLocBaselineRunReq(deviceSerialNumber: String): Option[LOCBaselineRunReq] = {
    try {
      val rr = getMostRecentBaselineFromDb(deviceSerialNumber) match {
        case Right(rr) => Some(rr)
        case _         => getBaselineByFile(deviceSerialNumber)
      }
      rr
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, LOCRunReq] = {
    val rtimageList = getRtimageList(alList)

    def epidSeriesList = rtimageList.map(epid => getSeries(epid)).distinct

    logger.info("Number of RTIMAGE files uploaded: " + rtimageList.size)
    val locBaselineRunReq = {
      try {
        val deviceSerialNumber = getMachineDeviceSerialNumberList(rtimageList, Seq()).head
        getLocBaselineRunReq(deviceSerialNumber)
      } catch {
        case _: Throwable => None
      }
    }

    val numSeries = rtimageList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, LOCRunReq] = 0 match {
      case _ if rtimageList.isEmpty       => formError("No EPID files uploaded")
      case _ if rtimageList.size != 5     => formError("There should be exactly 5 EPID images but there are " + rtimageList.size)
      case _ if epidSeriesList.size > 1   => formError("EPID images are from " + numSeries + " different series.")
      case _ if locBaselineRunReq.isEmpty => formError("LOC baseline images have not been established for this machine.")
      case _                              => Right(LOCRunReq(rtimageList, locBaselineRunReq.get))
    }
    result
  }

  private def getExcelWorkbookList(dir: File): Seq[LOCFileWorkbook] = {
    val fileList = dir.listFiles.filter { f => f.getName.toLowerCase.contains(".xls") }
    logger.info("Number Excel spreadsheets found: " + fileList.length + fileList.map(f => "\n    " + f.getAbsolutePath).mkString)
    val list = fileList.map(f => (f, ExcelUtil.read(f))).filter { fWb => fWb._2.isRight }.map(fWb => (fWb._1, fWb._2.right.get)).toSeq.map(fWb => LOCFileWorkbook(fWb._1, fWb._2))
    val standard = list.find(s => s.file.getName.contains("spreadsheet"))
    if (standard.isDefined)
      Seq(standard.get)
    else
      list
  }

  private def excelToHtml(file: File, workbook: Workbook): Unit = {
    try {
      val html = WebUtil.excelToHtml(workbook)
      val htmlFile = new File(file.getParentFile, Util.fileBaseName(file) + ".html")
      logger.info("Writing html version of spreadsheet to " + htmlFile.getAbsolutePath)
      Util.writeFile(htmlFile, html)
    } catch {
      case t: Throwable => logger.warn("Unable to write workbook for file " + file.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  private def baselineDir(extendedData: ExtendedData) = new File(extendedData.output.dir, baselineDirName)

  /**
    * Write the baseline files to the output directory to make them available to the MATLAB code.
    *
    * @param extendedData Relevant metadata.
    * @param runReq Contain baseline DICOM.
    */
  private def writeBaselineFilesLocally(extendedData: ExtendedData, runReq: LOCRunReq): Unit = {
    val dir = baselineDir(extendedData)
    Util.mkdirs(dir)

    val openFile = new File(dir, "OPEN_Baseline.dcm")
    val transFile = new File(dir, "TRANS_Baseline.dcm")

    Util.writeAttributeListToFile(runReq.baseline.baselineOpen, openFile)
    Util.writeAttributeListToFile(runReq.baseline.baselineTrans, transFile)
  }

  /**
    * Write the delivery files to the local directory.
    * @param extendedData Metadata
    * @param runReq Contains DICOM files.
    */
  private def writeDeliveryDicomFiles(extendedData: ExtendedData, runReq: LOCRunReq): Unit = {

    def writeToOutputDir(al: AttributeList, index: Int): Unit = {
      val imageLabel = {
        val attr = al.get(TagByName.RTImageLabel)
        if (attr != null) "_" + attr.getSingleStringValueOrEmptyString()
        else ""
      }
      val name = FileUtil.replaceInvalidFileNameCharacters("delivery" + index + imageLabel + ".dcm", '_')
      val file = new File(extendedData.output.dir, name)
      Util.writeAttributeListToFile(al, file)
    }

    runReq.epidList.zipWithIndex.foreach(x => writeToOutputDir(x._1, x._2))
  }

  override def run(extendedData: ExtendedData, runReq: LOCRunReq, response: Response): ProcedureStatus.Value = {

    logger.info("Writing DICOM baseline to the local baseline directory " + extendedData.output.dir.getAbsolutePath)
    writeBaselineFilesLocally(extendedData, runReq)
    logger.info("Writing DICOM delivery files the output directory " + extendedData.output.dir.getAbsolutePath)
    writeDeliveryDicomFiles(extendedData, runReq)
    logger.info("Running LOC Matlab program...")
    val status = LOCMatlab.executeMatlab(extendedData, baselineDir(extendedData))
    logger.info("LOC Matlab program finished.  Status: " + status)

    if (ProcedureStatus.eq(status, ProcedureStatus.done)) {
      val xmlFile = new File(extendedData.output.dir, outputXmlFileName)
      logger.info("Reading Matlab program created file " + xmlFile.getAbsolutePath + " of size " + xmlFile.length() + " bytes.")
      val locToXml = new LOCToXml(xmlFile)

      val doc = XML.loadFile(xmlFile)
      logger.info("LOC inserting data into database...")
      LOCInsertIntoDb.insert(doc, extendedData.output.outputPK.get)

      logger.info("Creating spreadsheet...")
      LOCMakeSpreadsheet(extendedData.output.dir, locToXml, response).write()
      val excelWorkbookList = {
        val list = getExcelWorkbookList(extendedData.output.dir)
        val standard = list.find(s => s.file.getName.contains("spreadsheet"))
        if (standard.isDefined)
          Seq(standard.get)
        else list
      }
      excelWorkbookList.foreach(fWb => excelToHtml(fWb.file, fWb.workbook))
      logger.info("Finished spreadsheets")
      logger.info("Making LOC HTML")
      val htmlText = LOCMakeHtml.makeDisplay(extendedData.output, locToXml, excelWorkbookList)
      val htmlFile = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
      logger.info("Writing LOC HTML to " + htmlFile.getAbsolutePath)
      Util.writeFile(htmlFile, htmlText)
      logger.info("Finished writing LOC HTML")
    } else {
      logger.error("Failed to run LOC Matlab executable.")
    }
    status
  }

  // override def postRun(extendedData: ExtendedData, runReq: LOC2RunReq): Unit = {}

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
