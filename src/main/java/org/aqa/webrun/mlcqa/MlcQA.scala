package org.aqa.webrun.mlcqa

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

class MlcQA(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[MlcQaRunReq] {

  /** Run the actual analysis.  This must create a display.html file in the output directory. */
  override def run(extendedData: ExtendedData, runReq: MlcQaRunReq, response: Response): ProcedureStatus.Value = ???

  /**
    * Validate the data and either return the data packaged up for processing, or, messages indicating the problem.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, RunReqClass] = ???

  override def makeRunReqForRedo(alList: Seq[AttributeList], oldOutput: Option[Output]): RunReqClass = ???

  /**
    * If possible, get the patient ID.
    */
  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String] = {
    alList.filter(al => Util.isRtimage(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  /**
    * get the date that the data was acquired.
    */
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
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
  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String] = {
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }
}
