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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
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

import java.sql.Timestamp
import scala.xml.Elem

class GapSkewRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[GapSkewRunReq] {
  // private val machineSelector = new WebInputSelectMachine("Machine", 6, 0)
  /*
  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }
   */

  // private val runButton = makeButton("Run", primary = true, ButtonType.BtnDefault)
  // private val cancelButton = makeButton("Cancel", primary = false, ButtonType.BtnDefault)

  // private def form = new WebForm(procedure.webUrl, Some("MLC QA"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  // private def formErr(msg: String): Either[StyleMapT, RunReqClass] = Left(Error.make(form.uploadFileInput.get, msg))

  private def makeRtimageMap(rtplan: AttributeList, rtimageList: Seq[AttributeList]): Map[String, AttributeList] = {
    rtimageList.map(rtImg => (Phase2Util.getBeamNameOfRtimage(rtplan, rtImg).get, rtImg)).toMap
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
  override def run(extendedData: ExtendedData, runReq: GapSkewRunReq, response: Response): ProcedureStatus.Value = {
    val fleList = runReq.rtimageMap.keys.toSeq.filter(beam => Config.GapSkewBeamNameList.contains(beam)).sorted.map(runReq.rtimageMap)
    val fleResultList = fleList.map(rtImg => new FindLeafEnds(rtImg, runReq.rtplan).leafSet)

    // TODO should put data in database

    new GapSkewHtml(extendedData, runReq, fleResultList, ProcedureStatus.done).makeDisplay
    ProcedureStatus.done // TODO get status from fleList
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

      case _ =>
        // the RTPLAN that will be used
        val rtplan = getRtplan(rtplanRefList.head, rtplanList).get
        Right(GapSkewRunReq(rtplan, makeRtimageMap(rtplan, rtimageList)))
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = {
    val rtimageList = alList.filter(Util.isRtimage)
    val rtplanSop = getRtplanRefList(rtimageList.head).head
    val rtplan = getRtplan(rtplanSop, Seq()).get
    GapSkewRunReq(rtplan, makeRtimageMap(rtplan, rtimageList))
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
