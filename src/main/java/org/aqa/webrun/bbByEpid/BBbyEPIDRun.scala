package org.aqa.webrun.bbByEpid

import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import java.io.File
import org.aqa.db.Procedure
import org.aqa.Util
import org.aqa.DicomFile
import com.pixelmed.dicom.TagFromName
import edu.umro.util.Utility
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebRunIndex
import org.aqa.run.PostProcess
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutputUtil
import scala.xml.XML
import org.aqa.db.MetadataCheck
import com.pixelmed.dicom.SOPClass
import org.aqa.db.Input
import java.sql.Timestamp
import java.util.Date
import org.aqa.run.Run
import org.aqa.run.ProcedureStatus
import org.aqa.db.Output
import org.aqa.db.Institution
import org.aqa.db.User
import org.aqa.web.ViewOutput
import org.aqa.web.WebServer
import com.pixelmed.dicom.TimeAttribute
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.WebRunProcedure
import org.aqa.webrun.LOCSpreadsheet
import org.aqa.webrun.LOCXml
import org.aqa.db.Machine
import org.aqa.webrun.RunRequirements
import org.aqa.Config
import edu.umro.ScalaUtil.Trace
import java.awt.Color
import edu.umro.ImageUtil.ImageUtil
import java.awt.Point
import scala.util.Try
import java.awt.geom.Point2D
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.aqa.web.WebUtil
import org.aqa.web.Session
import org.aqa.db.CachedUser
import org.aqa.web.OutputList
import org.aqa.webrun.ExtendedData
import org.aqa.ImageRegistration
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.BBbyEPID
import org.aqa.AngleType
import com.pixelmed.dicom.AttributeTag
import org.aqa.run.RunTrait
import org.aqa.run.RunProcedure
import org.aqa.Logging
import org.aqa.run.RunReqClass

/**
 * Run BBbyEPID code.
 */
class BBbyEPIDRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[BBbyEPIDRunReq] {

  private def getEpidList(alList: Seq[AttributeList]) = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  override def getProcedure = procedure

  override def getMachineDeviceSerialNumber(runReq: BBbyEPIDRunReq): String = {
    RunProcedure.getDeviceSerialNumber(runReq.epidList).head
  }

  override def getMachine(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Machine] = {
    val epidList = getEpidList(alList)
    val dsnList = epidList.map(al => al.get(TagFromName.DeviceSerialNumber)).filterNot(_ == null).map(a => a.getSingleStringValueOrNull).filterNot(_ == null).distinct
    val machList = dsnList.map(dsn => Machine.findMachinesBySerialNumber(dsn)).flatten

    machList.headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
    val epidList = getEpidList(alList)

    def getTimestamp(dateTag: AttributeTag, timeTag: AttributeTag): Option[Timestamp] = {
      val msList = epidList.map(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).flatten.map(dt => dt.getTime)
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

  /**
   * Make the run requirements from the attribute lists.
   */
  override def makeRunReq(alList: Seq[AttributeList]): BBbyEPIDRunReq = {
    val epidList = alList.filter(al => Util.isRtimage(al))
    new BBbyEPIDRunReq(epidList)
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, BBbyEPIDRunReq] = {
    val epidList = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))
    val angleList = epidList.map(epid => Util.gantryAngle(epid))
    def angleTextList = angleList.map(a => Util.fmtDbl(a)).mkString("  ")
    // true if all angles are valid
    val anglesTypeList = angleList.map(angle => AngleType.classifyAngle(angle)).flatten

    def epidSeriesList = epidList.map(epid => getSeries(epid)).distinct
    def frameOfRefList = epidList.map(epid => epid.get(TagFromName.FrameOfReferenceUID)).filterNot(attr => attr == null).map(attr => attr.getSingleStringValueOrEmptyString).distinct

    def machineCheck = RunProcedure.validateMachineSelection(valueMap, epidList)

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, BBbyEPIDRunReq] = 0 match {
      case _ if epidList.isEmpty => formError("No EPID files uploaded")
      case _ if frameOfRefList.isEmpty => formError("EPIDs do not specify a frame of reference")
      case _ if epidSeriesList.size > 1 => formError("EPID images are from " + numSeries + " different series.")
      case _ if frameOfRefList.size > 1 => formError("EPIDs specify more than one frame of reference")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ => {
        val runReq = new BBbyEPIDRunReq(epidList) //, machineCheck.right.get)
        Right(runReq)
      }
    }
    result
  }

  override def run(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): ProcedureStatus.Value = {
    val status = BBbyEPIDAnalyse.runProcedure(extendedData, runReq)
    status
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handle(valueMap, request, response, this.asInstanceOf[RunTrait[RunReqClass]])
  }

}
