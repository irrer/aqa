package org.aqa.webrun.floodField

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
import org.aqa.db.FloodField
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

/**
  * Process a new flood field.
  *
  * @param procedure Flood Field procedure
  */
class FloodRun(procedure: Procedure) extends WebRunProcedure with RunTrait[FloodRunReq] {

  override def run(extendedData: ExtendedData, runReq: FloodRunReq, response: Response): ProcedureStatus.Value = {

    // create a new record
    val floodField = FloodField.makeFloodField(extendedData.outputPK, runReq.floodField)

    // insert into database
    floodField.insert


    logger.info(s"Inserted FloodField into database: $floodField")
    FloodHtml.makeHtml(extendedData: ExtendedData, runReq: FloodRunReq, response: Response)
    ProcedureStatus.done
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {
    val rtplanList = alList.filter(Util.isRtplan)
    val rtimageList = alList.filter(Util.isRtimage).filterNot(FloodUtil.isFloodField)
    val floodFieldList = alList.filter(FloodUtil.isFloodField)

    def alreadyInDatabase: Boolean = {
      if (floodFieldList.size == 1) {
        val ff = FloodField.makeFloodField(-1, floodFieldList.head)
        FloodField.getByImageHash(ff.imageHash_md5).nonEmpty
      } else
        false
    }

    val result = 0 match {
      case _ if alList.isEmpty           => formError("No DICOM files were uploaded.  There should be exactly one.")
      case _ if alList.size > 1          => formError("More than one DICOM file was uploaded.  There should be only one.")
      case _ if rtplanList.nonEmpty      => formError("One or more RTPLAN files was uploaded.  There should only be a flood field image.")
      case _ if rtimageList.nonEmpty     => formError("One or more RTIMAGE files was uploaded that are not flood field images.  There should only be a flood field image.")
      case _ if floodFieldList.isEmpty   => formError("No flood field was uploaded.  There should be exactly one flood field image.")
      case _ if floodFieldList.size > 1  => formError("More than one flood field was uploaded.  There should be exactly one flood field image.")
      case _ if alreadyInDatabase        => formError("That flood field has already been uploaded.")
      case _ if floodFieldList.size == 1 => Right(FloodRunReq(floodFieldList.head))
    }

    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): RunReqClass = {
    val floodFieldList = alList.filter(FloodUtil.isFloodField)
    FloodRunReq(floodFieldList.head)
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    alList.filter(FloodUtil.isFloodField).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    val dateList = alList.filter(FloodUtil.isFloodField).map(Util.extractDateTimeAndPatientIdFromDicomAl).flatMap(_._1.headOption).map(_.getTime).sorted.map(t => new Timestamp(t))
    dateList.headOption
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getMachineDeviceSerialNumberListFromRtimageUtil(alList, xmlList)

  override def alwaysRequireMachine = true
}
