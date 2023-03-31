/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.focalSpot

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.apache.commons.io.FileUtils
import org.aqa.Util
import org.aqa.db.DicomSeries
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.emptyValueMap
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem

/**
  * This is a temporary class to allow running of just the focal spot test by a user.
  * @param procedure Procedure identifier.
  */
class FSRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[FSRunReq] {

  /**
    * Get the map of images to be used.  If there is more than one image that reference the same beam number, then
    * use the latest (chronologically) one.  Choosing one image for each beam addresses the problem of ambiguity.
    * Choosing the latest one allows the operator to fix mistakes (re-capture images).
    * @param rtplan DICOM RTPLAN.
    * @param rtimageList List of RTIMAGES.
    * @return
    */
  private def makeRtimageMap(rtplan: AttributeList, rtimageList: Seq[AttributeList]): Map[String, AttributeList] = {
    // sort chronologically
    val sortedRtimageList = rtimageList.sortBy(r => Util.extractDateTimeAndPatientIdFromDicomAl(r)._1.head.getTime)
    sortedRtimageList.map(rtImg => (Phase2Util.getBeamNameOfRtimage(rtplan, rtImg).get, rtImg)).toMap
  }

  /**
    * Get the list of RTPLAN SOPs referenced by the given attribute list.
    *
    * @param al DICOM referencing one or more RTPLAN.
    * @return List of RTPLAN SOPs.
    */
  private def getRtplanRefList(al: AttributeList): Seq[String] = {
    val attrList = DicomUtil.seqToAttr(al, TagByName.ReferencedRTPlanSequence).flatMap(al => DicomUtil.findAllSingle(al, TagByName.ReferencedSOPInstanceUID))
    attrList.flatMap(_.getStringValues).distinct
  }

  /**
    * Get the RTPLAN referenced with the given SOP UID.  Try the passed parameters first, then the database.
    * @param rtplanSop SOPInstanceUID of RTPLAN.
    * @param rtplanList List of RTPLAN(s) uploaded.
    * @return
    */
  private def getRtplan(rtplanSop: String, rtplanList: Seq[AttributeList]): Option[AttributeList] = {
    rtplanList.find(al => Util.sopOfAl(al).equals(rtplanSop)) match {
      case Some(al) => Some(al) // found it in the RTPLAN passed in
      case _ => // look in the database
        val ds = DicomSeries.getBySopInstanceUID(rtplanSop).headOption
        if (ds.isDefined) {
          ds.get.attributeListList.find(al => Util.sopOfAl(al).equals(rtplanSop))
        } else
          None
    }
  }

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  override def run(extendedData: ExtendedData, fsRunReq: FSRunReq, response: Response): ProcedureStatus.Value = {
    FSAnalysis.runProcedure(extendedData, fsRunReq)

    // Copy all files into the output directory.
    val focalSpotDir = FSHTML.focalSpotDir(extendedData.output)
    val outputDir = extendedData.output.dir
    Util.listDirFiles(focalSpotDir).foreach(fsFile => FileUtils.copyFile(fsFile, new File(outputDir, fsFile.getName)))

    // Copy the main focal spot file into the display.html file so tha the framework will find it.
    FileUtils.copyFile(new File(outputDir, FSHTML.htmlFileName), new File(outputDir, Output.displayFilePrefix + ".html"))

    ProcedureStatus.done
  }

  /**
    * Validate the data and either return the data packaged up for processing, or, messages indicating the problem.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    val rtimageList = alList.filter(Util.isRtimage)
    val rtplanList = alList.filter(Util.isRtplan)

    // list of RTPLANS referenced by RTIMAGE list
    val rtplanRefList = rtimageList.flatMap(getRtplanRefList).distinct
    val machineSerialNumberList = rtimageList.flatMap(rtimage => Util.getAttrValue(rtimage, TagByName.DeviceSerialNumber)).distinct

    def allHaveSerialNumber() = rtimageList.map(rtimage => rtimage.get(TagByName.DeviceSerialNumber)).map(_ != null).reduce(_ && _)

    def rtplanIsAvailable(): Boolean = {
      val uploaded = rtplanList.exists(plan => Util.sopOfAl(plan).equals(rtplanRefList.head))
      def inDatabase() = DicomSeries.getBySopInstanceUID(rtplanRefList.head).nonEmpty

      uploaded || inDatabase()
    }

    /**
      * Determine if at least one of the uploaded beams has a name that is in the configured list of beams.
      * @return None if ok, message on failure.
      */

    val result: Either[StyleMapT, RunReqClass] = 0 match {
      case _ if rtplanRefList.size > 1           => formError("The RTIMAGE(s) reference more than one RTPLAN.")
      case _ if rtimageList.isEmpty              => formError("No RTIMAGE files given.")
      case _ if rtplanRefList.isEmpty            => formError("Can not find the referenced RTPLAN.  Retry and upload the RTPLAN with the images. ")
      case _ if machineSerialNumberList.size > 1 => formError("The RTIMAGE(s) reference more than one treatment machine.  There must be exactly one.")
      case _ if !rtplanIsAvailable()             => formError("Can not find the referenced RTPLAN.  Retry and upload the RTPLAN with the images.")
      case _ if !allHaveSerialNumber()           => formError("At least one RTIMAGE file does not have a device serial number defining which machine created it.")
      case _ if machineSerialNumberList.isEmpty =>
        formError(
          "None of the " + rtimageList.size +
            " RTIMAGE(s) have a device serial number (0018,1000) tag.\\n" +
            "This can happen on a new machine or one that has been recently serviced.\\n" +
            "The device serial number is required by this software to identify the instance of the machine."
        )

      case _ => // success
        // the RTPLAN that will be used
        val rtplan = getRtplan(rtplanRefList.head, rtplanList).get
        Right(FSRunReq(rtplan, makeRtimageMap(rtplan, rtimageList)))
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = {
    val rtimageList = alList.filter(Util.isRtimage)
    val rtplanSop = getRtplanRefList(rtimageList.head).head
    val rtplan = getRtplan(rtplanSop, Seq()).get
    FSRunReq(rtplan, makeRtimageMap(rtplan, rtimageList))
  }

  /**
    * If possible, get the patient ID.
    */
  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    alList.filter(Util.isRtimage).map(Util.patientIdOfAl).headOption
  }

  /**
    * get the date that the data was acquired.
    */
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val min = alList.filter(Util.isRtimage).map(Util.extractDateTimeAndPatientIdFromDicomAl).flatMap(dp => dp._1.map(_.getTime)).min
    Some(new Timestamp(min))
  }

  /**
    * Get the procedure that this was constructed with.
    */
  override def getProcedure: Procedure = procedure

  /**
    * Get the machine's DeviceSerialNumber from the input files.  This is used to handle the
    * case where a new machine needs to have it's serial number established.
    */
  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val rtimageList = alList.filter(Util.isRtimage)
    val dsnList = rtimageList.flatMap(Util.attributeListToDeviceSerialNumber).distinct
    dsnList
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
