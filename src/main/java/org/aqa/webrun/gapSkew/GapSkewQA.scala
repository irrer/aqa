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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

class GapSkewQA(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[GapSkewRunReq] {

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  override def run(extendedData: ExtendedData, runReq: GapSkewRunReq, response: Response): ProcedureStatus.Value = ???

  /**
    * Validate the data and either return the data packaged up for processing, or, messages indicating the problem.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = ???

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = ???

  /**
    * If possible, get the patient ID.
    */
  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    alList.filter(al => Util.isRtimage(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  /**
    * get the date that the data was acquired.
    */
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val min = alList.filter(al => Util.isRtimage(al)).map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al)).flatMap(dp => dp._1).minBy(_.getTime)
    Some(new Timestamp(min.getTime))
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
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }
}
