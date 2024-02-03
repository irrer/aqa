package org.aqa.webrun.psm.cmn

import com.pixelmed.dicom.AttributeList
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.aqa.Util
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

class CmnRun(procedure: Procedure) extends WebRunProcedure with RunTrait[CmnRunReq] {

  override def run(extendedData: ExtendedData, runReq: CmnRunReq, response: Response): ProcedureStatus.Value = ???

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    /*
    flood field
    rtplan
    42 lng x lat
     */
    ???
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = ???

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    alList.filter(al => Util.isRtimage(al)).map(al => Util.patientIdOfAl(al)).headOption
  }
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = getDataDateFromRtimageList(alList)

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] =
    super.getMachineDeviceSerialNumberListFromRtimageUtil(alList, xmlList)
}