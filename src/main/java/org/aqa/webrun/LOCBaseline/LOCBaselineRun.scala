package org.aqa.webrun.LOCBaseline

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
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import edu.umro.ScalaUtil.DicomUtil
import org.apache.poi.ss.usermodel.Workbook
import org.aqa.Util
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
import org.aqa.webrun.LOC.LOCFileWorkbook
import org.aqa.webrun.LOC.LOCInsertIntoDb
import org.aqa.webrun.LOC.LOCMakeHtml
import org.aqa.webrun.LOC.LOCMakeSpreadsheet
import org.aqa.webrun.LOC.LOCMatlab
import org.aqa.webrun.LOC.LOCToXml
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem
import scala.xml.XML

/**
  * Establish baseline files for LOC.
  */
class LOCBaselineRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[LOCBaselineRunReq] {

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
  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], output: Option[Output]): LOCBaselineRunReq = {
    val rtimageList = getRtimageList(alList)
    val result = LOCBaselineRunReq(???, ???)
    result
  }

  /**
    * Look in the machine configuration directory for this machine and then ensure that there are OPEN and TRANS files.
    *
    * @param rtimageList List of image files uploaded by user.
    * @return True if there are baseline files.
    */
  private def hasBaseline(rtimageList: Seq[AttributeList]): Boolean = {
    val dsnList = getMachineDeviceSerialNumberList(rtimageList, Seq())
    if (dsnList.isEmpty)
      false
    else {
      val machineList = Machine.findMachinesBySerialNumber(dsnList.head)
      if (machineList.isEmpty)
        false
      else {
        val machineConfigDir = machineList.head.configDir
        val has = {
          machineConfigDir.isDefined &&
          machineConfigDir.get.isDirectory &&
          Util.listDirFiles(machineConfigDir.get).exists(f => f.getName.contains("OPEN")) &&
          Util.listDirFiles(machineConfigDir.get).exists(f => f.getName.contains("TRANS"))
        }
        has
      }
    }
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, LOCBaselineRunReq] = {
    val rtimageList = getRtimageList(alList)

    def epidSeriesList = rtimageList.map(epid => getSeries(epid)).distinct

    logger.info("Number of RTIMAGE files uploaded: " + rtimageList.size)

    val numSeries = rtimageList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, LOCBaselineRunReq] = 0 match {
      case _ if rtimageList.isEmpty       => formError("No EPID files uploaded")
      case _ if rtimageList.size != 5     => formError("There should be exactly 5 EPID images but there are " + rtimageList.size)
      case _ if epidSeriesList.size > 1   => formError("EPID images are from " + numSeries + " different series.")
      case _ if !hasBaseline(rtimageList) => formError("LOC baseline images have not been established for this machine.")
      case _ =>
        val runReq = LOCBaselineRunReq(???, ???)
        Right(runReq)
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

  override def run(extendedData: ExtendedData, runReq: LOCBaselineRunReq, response: Response): ProcedureStatus.Value = {

    logger.info("Running LOC Matlab program...")
    val status = LOCMatlab.executeMatlab(extendedData)
    logger.info("LOC Matlab program finished.  Status: " + status)

    if (ProcedureStatus.eq(status, ProcedureStatus.done)) {
      val xmlFile = new File(extendedData.output.dir, ???)
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
