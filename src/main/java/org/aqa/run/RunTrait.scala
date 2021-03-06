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

package org.aqa.run

import com.pixelmed.dicom.AttributeList
import org.aqa.Logging
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.webrun.ExtendedData
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

trait RunTrait[RunReqClassType] extends Logging {

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  def run(extendedData: ExtendedData, runReq: RunReqClassType, response: Response): ProcedureStatus.Value

  /**
    * After processing has been completed, call this function.  This will be called after run or redo.
    * If not provided by the subclass, then it defaults to doing nothing.
    * @param extendedData Metadata
    * @param runReq Data used for processing.
    */
  def postRun(extendedData: ExtendedData, runReq: RunReqClassType): Unit = {}

  /**
    * Validate the data and either return the data packaged up for processing, or, messages indicating the problem.
    */
  def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass]

  /*
   * Construct the run requirements to perform a redo.  The implication of redo is that the
   * data has already been validated, so the data should be correct.
   */
  def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass

  /**
    * If possible, get the patient ID.
    */
  def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String]

  /**
    * get the date that the data was acquired.
    */
  def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp]

  /**
    * Get the procedure that this was constructed with.
    */
  def getProcedure: Procedure

  /**
    * Get the machine's DeviceSerialNumber from the input files.  This is used to handle the
    * case where a new machine needs to have it's serial number established.
    */
  def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String]

  /** Convenience function for constructing error messages to display to user on web page. */
  def formError(msg: String) = Left(WebUtil.Error.make(WebUtil.uploadFileLabel, msg))

  /**
    * Called when a redo is being performed.  If it is not ok to do the redo, then a message is
    * returned, otherwise None is returned.
    *
    * @param outputPK: Output PK of old output.
    */
  def validateRedo(outputPK: Long): Option[String] = None
}
