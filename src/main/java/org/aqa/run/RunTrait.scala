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
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName

trait RunTrait[RunReqClassType] extends Logging {

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  def run(extendedData: ExtendedData, runReq: RunReqClassType, response: Response): ProcedureStatus.Value;

  //def redo(extendedData: ExtendedData, runReq: RunReqClassType): ProcedureStatus.Value;

  /**
   * Validate the data and either return the data packaged up for processing, or, messages indicating the problem.
   */
  def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, RunReqClass];

  /*
   * Construct the run requirements to perform a redo.  The implication of redo is that the
   * data has already been validated, so the data should be correct.
   */
  def makeRunReqForRedo(alList: Seq[AttributeList]): RunReqClass;

  /**
   * If possible, get the patient ID.
   */
  def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String];

  /**
   * get the date that the data was acquired.
   */
  def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp];

  /**
   * Get the procedure that this was constructed with.
   */
  def getProcedure: Procedure;

  /**
   * Get the machine's DeviceSerialNumber from the input files.  This is used to handle the
   * case where a new machine needs to have it's serial number established.
   */
  def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String];

  /** Convenience function for constructing error messages to display to user on web page. */
  def formError(msg: String) = Left(WebUtil.Error.make(WebUtil.uploadFileLabel, msg))

}