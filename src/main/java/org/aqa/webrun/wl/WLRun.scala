package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
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
import org.aqa.run.RunProcedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.emptyValueMap
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem

class WLRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[WLRunReq] {

  private def dateTime(al: AttributeList) = {
    val dt = Seq(
      Util.dicomGetTimeAndDate(al, TagByName.ContentDate, TagByName.ContentTime),
      Util.dicomGetTimeAndDate(al, TagByName.AcquisitionDate, TagByName.AcquisitionTime)
    ).flatten.head
    dt.getTime
  }

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(al => Util.isRtimage(al)).sortBy(dateTime)

  override def run(extendedData: ExtendedData, runReq: WLRunReq, response: Response): ProcedureStatus.Value = {
    // Process in parallel for speed.  Afterwards, sort by data time.
    val results = runReq.epidList.zipWithIndex.par.map(rtimageIndex => new WLProcessImage(extendedData, rtimageIndex._1, rtimageIndex._2, runReq).process).toList
    //val results = runReq.epidList.zipWithIndex.map(rtimageIndex => new WLProcessImage(extendedData, rtimageIndex._1, rtimageIndex._2, runReq).process).toList // Use this to run non-parallel

    val resultHasData = results.filter(r => WLImageStatus.hasResult(r.imageStatus))

    resultHasData.map(_.toWinstonLutz).map(_.insert)
    logger.info(s"Inserted ${resultHasData.size} WinstonLutz rows into database.")
    val mainHtmlText = WLMainHtml.generateGroupHtml(extendedData, results, runReq)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, mainHtmlText)
    logger.info("Wrote main HTML file " + file.getAbsolutePath)

    // true if all images passed.
    val allPassed = results.map(r => r.imageStatus.toString).distinct.forall(text => text.equals(WLImageStatus.Passed.toString))

    WLUpdateRestlet.updateWL()
    if (allPassed)
      ProcedureStatus.pass
    else
      ProcedureStatus.fail
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {

    val epidList = getRtimageList(alList)

    // val epidSeriesList = epidList.map(Util.serInstOfAl).distinct

    val gantryAngleList = epidList.map(Util.gantryAngle).map(Util.angleRoundedTo90).distinct.sorted
    val orthogonalAngleList = gantryAngleList.map(_ % 180).distinct

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    // val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    // first try getting the RTPLAN from the uploaded values, then from the database.
    val rtplan = {
      epidList.filter(Util.isRtimage).map(Phase2Util.referencedPlanUID).headOption match {
        case Some(rtplanUid) => Phase2Util.fetchRtplan(rtplanUid, alList)
        case _               => None
      }
    }

    val result: Either[WebUtil.StyleMapT, WLRunReq] = 0 match {
      case _ if epidList.isEmpty => formError("No EPID files uploaded")
      // case _ if epidSeriesList.size > 1       => formError("EPID images are from " + numSeries + " different series.")
      case _ if orthogonalAngleList.size != 2 => formError("Need to have images with both vertical and horizontal gantry angles.  Given beam had " + gantryAngleList.mkString("  "))
      case _ =>
        val runReq = WLRunReq(epidList.sortBy(dateTime), rtplan)
        Right(runReq)
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): WLRunReq = {
    val rtplan = alList.filter(Util.isRtimage).map(Phase2Util.referencedPlanUID).headOption match {
      case Some(rtplanUid) => Phase2Util.fetchRtplan(rtplanUid, alList)
      case _               => None
    }
    val result = WLRunReq(getRtimageList(alList).sortBy(dateTime), rtplan)
    result
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    val list = getRtimageList(alList).map(al => Util.patientIdOfAl(al)).distinct
    list.headOption
  }

  /**
    * Get the earliest date+time stamp of the RTIMAGES by evaluating all occurrences of Content Date+Time and
    * Acquisition Date+Time.
    *
    * @param valueMap Not used.
    * @param alList Seq of incoming DICOM files.
    * @param xmlList Not used.
    * @return Earliest date+time if available.
    */
  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    getRtimageList(alList).map(dateTime).map(ms => new Timestamp(ms)).headOption
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getMachineDeviceSerialNumberListFromRtimageUtil(alList, xmlList)

  override def getRadiationMachineNameList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getRadiationMachineNameListFromRtimageUtil(alList, xmlList)

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }
}
