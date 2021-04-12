package org.aqa.webrun.bbByEpid

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.sql.Timestamp

/**
  * Run BBbyEPID code.
  */
class BBbyEPIDRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[BBbyEPIDRunReq] {

  private def getEpidList(alList: Seq[AttributeList]) = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  override def getProcedure: Procedure = procedure

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
    val epidList = getEpidList(alList)

    def getTimestamp(dateTag: AttributeTag, timeTag: AttributeTag): Option[Timestamp] = {
      val msList = epidList.flatMap(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).map(dt => dt.getTime)
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
    val list = getEpidList(alList).map(al => Util.patientIdOfAl(al)).distinct
    list.headOption
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String] = {
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  /**
    * Make the run requirements from the attribute lists.
    */
  override def makeRunReqForRedo(alList: Seq[AttributeList], output: Option[Output]): BBbyEPIDRunReq = {
    val epidList = alList.filter(al => Util.isRtimage(al))
    val result = BBbyEPIDRunReq(epidList)
    result
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, BBbyEPIDRunReq] = {
    val epidList = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))

    def epidSeriesList = epidList.map(epid => getSeries(epid)).distinct

    def frameOfRefList = epidList.map(epid => epid.get(TagFromName.FrameOfReferenceUID)).filterNot(attr => attr == null).map(attr => attr.getSingleStringValueOrEmptyString).distinct

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, BBbyEPIDRunReq] = 0 match {
      case _ if epidList.isEmpty        => formError("No EPID files uploaded")
      case _ if frameOfRefList.isEmpty  => formError("EPIDs do not specify a frame of reference")
      case _ if epidSeriesList.size > 1 => formError("EPID images are from " + numSeries + " different series.")
      case _ if frameOfRefList.size > 1 => formError("EPIDs specify more than one frame of reference")
      case _ =>
        val runReq = BBbyEPIDRunReq(epidList)
        Right(runReq)
    }
    result
  }

  override def run(extendedData: ExtendedData, runReq: BBbyEPIDRunReq, response: Response): ProcedureStatus.Value = {
    val status = BBbyEPIDAnalyse.runProcedure(extendedData, runReq, response)
    status
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
