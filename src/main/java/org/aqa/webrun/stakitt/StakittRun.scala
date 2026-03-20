package org.aqa.webrun.stakitt

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
import org.aqa.webrun.wl.isoCheck.WLRunIsoCheck
import org.aqa.AnonymizeUtil
import org.aqa.web.WebServer
import org.aqa.AQAEventNetClient
import org.aqa.Config
import org.aqa.db.MachineWL
import org.aqa.webrun.winLutz360.Analysis
import org.aqa.webrun.wl.EventWLQASRSDone
import org.aqa.webrun.wl.WLImageStatus
import org.aqa.webrun.wl.WLImageUtil
import org.aqa.webrun.wl.WLMainHtml
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLResult
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.wl.WLUpdateRestlet
import org.restlet.Request
import org.restlet.Response

import java.io.File
import java.sql.Timestamp
import scala.xml.Elem

class StakittRun(procedure: Procedure) extends WebRunProcedure with RunTrait[WLRunReq] {

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(al => Util.isRtimage(al)).sortBy(WLImageUtil.timeOfMs)

  /**
    * Send an EventNet event indicating that a WL has been done.
    * @param extendedData metadata for URL and machine ID
    * @param runReq has patient ID
    * @param status pass/fail
    * @param NumberOfImages number of RTIMAGE files
    */
  private def sendEvent(extendedData: ExtendedData, runReq: WLRunReq, status: ProcedureStatus.Value, NumberOfImages: Int): Unit = {
    val realPatientId: String =
      try {
        AnonymizeUtil.deAnonymizeAttribute(extendedData.institution.institutionPK.get, runReq.epidList.head.get(TagByName.PatientID)).get.getSingleStringValueOrEmptyString
      } catch {
        case _: Throwable => "NA"
      }

    try {
      val event = new EventWLQASRSDone( //
        PatientId = realPatientId,
        CareEventStart = extendedData.output.dataDate.get,
        Status = status,
        NumberOfImages = NumberOfImages,
        ReportURL = Config.RootUrl + WebServer.urlOfResultsFile(extendedData.output.dir) + "/" + Output.displayFilePrefix + ".html",
        TreatmentMachine = extendedData.machine.getRealId
      )

      AQAEventNetClient.sendEventWLQASRSDone(event)
      logger.info(s"Sent EventNet event\n$event")
    } catch {
      case t: Throwable => logger.error(s"Unexpected error sending event: ${fmtEx(t)}")
    }
  }

  override def run(extendedData: ExtendedData, runReq: WLRunReq, response: Response): ProcedureStatus.Value = {

    val machineWL = MachineWL.getMachineWLOrDefault(extendedData.machine.machinePK.get)

    // list of processed images
    val resultList = {
      def doImage(rtimage: AttributeList): WLResult = {
        val wlMessage = WLMessage(runReq, rtimage)
        Analysis(extendedData, rtimage, runReq, machineWL, Some(wlMessage)).asInstanceOf[WLResult]
      }
      // Perform processing in parallel for speed
      runReq.epidList.par.map(doImage).toList
      // runReq.epidList.map(doImage).toList // use this to NOT run in parallel to make debugging easier.
    }

    // make a list of entries that have credible data, whether it is within limits or not.  Not included are
    // those that failed sanity checks, such as edges having sufficient contrast.
    val resultHasData = resultList.filter(r => WLImageStatus.hasResult(r.getImageStatus))

    val dbList = resultHasData.map(_.convertToDB)

    val wlList = dbList.filter(_.isRight).map(_.right.get)

    val insertedList = dbList.map(r => r.right.get.insert)

    logger.info(s"Inserted ${insertedList.size} WinstonLutz rows into database out of ${runReq.epidList.size} RTIMAGE files.")

    // If there are images to do a monthly analysis, then do it and add links to the web page
    val monthly: Elem = {
      try {
        val elem = WLRunIsoCheck.run(extendedData, runReq, wlList)
        elem // This will be a trivial HTML snippet if this data set does not have monthly data.
      } catch {
        case t: Throwable =>
          logger.error(s"Error analyzing monthly data: ${fmtEx(t)}")
          <span> </span>
      }
    }
    val mainHtmlText = WLMainHtml.generateGroupHtml(extendedData, resultList, runReq, monthly)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeFile(file, mainHtmlText)
    logger.info("Wrote main HTML file " + file.getAbsolutePath)

    // true if all images passed.
    val allPassed = resultList.nonEmpty && resultList.map(r => r.getImageStatus.toString).distinct.forall(text => text.equals(WLImageStatus.Passed.toString))

    WLUpdateRestlet.updateWL()
    val status =
      if (allPassed)
        ProcedureStatus.pass
      else
        ProcedureStatus.fail

    sendEvent(extendedData, runReq, status, resultList.size)

    status
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {

    val epidList = getRtimageList(alList)

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    // val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    // first try getting the RTPLAN from the uploaded values, then from the database.
    val rtplan = {
      epidList.filter(Util.isRtimage).flatMap(Phase2Util.referencedPlanUIDOpt).headOption match {
        case Some(rtplanUid) => Phase2Util.fetchRtplan(rtplanUid, alList)
        case _               => None
      }
    }

    val result: Either[WebUtil.StyleMapT, WLRunReq] = 0 match {
      case _ if epidList.isEmpty => formError("No EPID files uploaded")
      // case _ if epidSeriesList.size > 1       => formError("EPID images are from " + numSeries + " different series.")
      // case _ if orthogonalAngleList.size != 2 => formError("Need to have images with both vertical and horizontal gantry angles.  Given beam had " + gantryAngleList.mkString("  "))
      case _ =>
        val runReq = WLRunReq(epidList.sortBy(WLImageUtil.timeOfMs), rtplan)
        Right(runReq)
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): WLRunReq = {
    val rtplan = alList.filter(Util.isRtimage).flatMap(Phase2Util.referencedPlanUIDOpt).headOption match {
      case Some(rtplanUid) => Phase2Util.fetchRtplan(rtplanUid, alList)
      case _               => None
    }
    val result = WLRunReq(getRtimageList(alList).sortBy(WLImageUtil.timeOfMs), rtplan)
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
    getRtimageList(alList).map(WLImageUtil.timeOfMs).map(ms => new Timestamp(ms)).headOption
  }

  override def getProcedure: Procedure = procedure

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getMachineDeviceSerialNumberListFromRtimageUtil(alList, xmlList)

  override def getRadiationMachineNameList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = getRadiationMachineNameListFromRtimageUtil(alList, xmlList)

  override def handle(request: Request, response: Response): Unit = {
    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }
}
