package org.aqa.run

import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.Logging
import org.aqa.web.WebUtil.WebForm
import org.aqa.webrun.ExtendedData
import java.sql.Timestamp
import org.aqa.db.Procedure
import org.aqa.web.WebUtil

trait RunTrait[RunReqClassType] extends Logging {

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  def run(extendedData: ExtendedData, runReq: RunReqClassType): ProcedureStatus.Value;

  //def redo(extendedData: ExtendedData, runReq: RunReqClassType): ProcedureStatus.Value;
  def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, RunReqClass];

  def makeRunReq(alList: Seq[AttributeList]): RunReqClass;
  def getMachine(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Machine];
  def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String];
  def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp];
  def getProcedure: Procedure;

  /**
   * Get the machine's DeviceSerialNumber from the input files.  This is used to handle the
   * case where a new machine needs to have it's serial number established.
   */
  def getMachineDeviceSerialNumber(runReq: RunReqClassType): String;

  /** Convenience function for constructing error messages to display to user on web page. */
  def formError(msg: String) = Left(WebUtil.Error.make(WebUtil.uploadFileLabel, msg))
}