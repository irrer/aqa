package org.aqa.webrun.LOC2

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
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.run.StdLogger
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.io.FileInputStream
import java.sql.Timestamp
import scala.sys.process.Process

/**
  * Run LOC code.
  */
class LOC2Run(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[LOC2RunReq] {

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(Util.isRtimage)

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  override def getProcedure: Procedure = procedure

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
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

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String] = {
    val list = getRtimageList(alList).map(al => Util.patientIdOfAl(al)).distinct
    list.headOption
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String] = {
    val dsnList = getRtimageList(alList).flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  /**
    * Make the run requirements from the attribute lists.
    */
  override def makeRunReqForRedo(alList: Seq[AttributeList], output: Option[Output]): LOC2RunReq = {
    val rtimageList = getRtimageList(alList)
    val result = LOC2RunReq(rtimageList)
    result
  }

  private def hasBaseline(rtimageList: Seq[AttributeList]): Boolean = {
    val dsnList = getMachineDeviceSerialNumberList(rtimageList)
    if (dsnList.isEmpty)
      false
    else {
      val machineList = Machine.findMachinesBySerialNumber(dsnList.head)
      if (machineList.isEmpty)
        false
      else {
        val machineConfigDir = machineList.head.configDir
        machineConfigDir.isDefined && machineConfigDir.get.isDirectory && (Util.listDirFiles(machineConfigDir.get).size == 2)
      }
    }
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, LOC2RunReq] = {
    val rtimageList = getRtimageList(alList)

    def epidSeriesList = rtimageList.map(epid => getSeries(epid)).distinct

    logger.info("Number of RTIMAGE files uploaded: " + rtimageList.size)

    val numSeries = rtimageList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, LOC2RunReq] = 0 match {
      case _ if rtimageList.isEmpty       => formError("No EPID files uploaded")
      case _ if rtimageList.size != 5     => formError("There should be exactly 5 EPID images but there are " + rtimageList.size)
      case _ if epidSeriesList.size > 1   => formError("EPID images are from " + numSeries + " different series.")
      case _ if !hasBaseline(rtimageList) => formError("LOC baseline images have not been established for this machine.")
      case _ =>
        val runReq = LOC2RunReq(rtimageList)
        Right(runReq)
    }
    result
  }

  private def executeMatlab(extendedData: ExtendedData): Unit = {

    def setEnv(name: String, value: String): String = {
      s"SET $name='$value"
    }

    val matlabExecutableFile = {
      val locDir = new File(Config.staticDirFile, "LOC")
      val exe = new File(locDir, extendedData.procedure.fileName + ".exe")
      if (!exe.canExecute) {
        throw new RuntimeException("Can not execute LOC MATLAB file " + exe.getAbsolutePath)
      }
      exe
    }


    val commandFileName = "run.cmd"

    val commandFile = new File(extendedData.output.dir, commandFileName)

    val commandList = Seq(
      "CD /D " + extendedData.output.dir.getAbsolutePath,
      """copy /Y ..\*.dcm .""",
      setEnv("institution_id", extendedData.institution.name),
      setEnv("machine_configDir", extendedData.machine.configDir.get.getAbsolutePath),
      setEnv("machine_id", extendedData.machine.id),
      setEnv("mlc_model", extendedData.multileafCollimator.model),
      setEnv("outputPK", extendedData.output.outputPK.get.toString),
      matlabExecutableFile.getAbsolutePath
    )

    val commandFileContent = commandList.mkString("", System.lineSeparator(), System.lineSeparator())
    Util.writeFile(commandFile, commandFileContent)

    val fileInputStream = new FileInputStream(commandFile)

    val pb = Process(Seq("cmd.exe")) #< fileInputStream
    val processLogger = new StdLogger(extendedData.output)
    pb.run(processLogger, connectInput = true)
  }

  override def run(extendedData: ExtendedData, runReq: LOC2RunReq, response: Response): ProcedureStatus.Value = {
    executeMatlab(extendedData)

    val statusText = {
      val result = Util.readTextFile(new File(extendedData.output.dir, "status.txt"))
      result.right.get
    }
    val status = ProcedureStatus.stringToProcedureStatus(statusText)

    status.get
  }

  // override def postRun(extendedData: ExtendedData, runReq: LOC2RunReq): Unit = {}

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
