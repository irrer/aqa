package org.aqa.webrun.stakitt

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
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
import org.aqa.run.ProcedureStatus.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.emptyValueMap
import org.aqa.web.WebUtil.getValueMap
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.stakitt.stakittHTML.StakittHTML
import org.aqa.webrun.wl.WLImageUtil
import org.restlet.Request
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

class StakittRun(procedure: Procedure) extends WebRunProcedure with RunTrait[SktRunReq] {

  private def getRtimageList(alList: Seq[AttributeList]) = alList.filter(al => Util.isRtimage(al)).sortBy(WLImageUtil.timeOfMs)

  override def run(extendedData: ExtendedData, runReq: SktRunReq, response: Response): ProcedureStatus.Value = {

    logger.info("Starting stakitt analysis")

    // do all the heavy lifting here, each image in parallel
    val analysisList = runReq.rtimageList.par.map(rtimage => Analysis.analyze(extendedData, rtimage, runReq.rtplan)).toList

    // put data in database
    val resultList = analysisList.filter(_.isRight).flatMap(_.right.get.stakittList).map(_.stakitt)
    // resultList.foreach(_.insert) // TODO enable Stakitt in DB

    // generate HTML
    val html = new StakittHTML(extendedData, analysisList, runReq.rtplan)
    html.makeHTML()

    // If there was a failure, then report it. Otherwise, return 'done'.
    val status: ProcedureStatus = {
      val failure = analysisList.filter(_.isLeft).map(_.left.get.status) :+ ProcedureStatus.done
      failure.head
    }

    status
  }

  /**
    * Return true if the beam name of the given RTIMAGE contains 'stakitt'.
    * Note: Near spellings of stakitt are allowed. Also allowed is upper or lower case.
    * @param rtimage Check this image.
    * @return True if beam name contains stakitt.
    */
  private def isStakitt(rtimage: AttributeList, rtplan: Option[AttributeList]): Boolean = {
    def nameIsStakitt(): Boolean = {
      val name = DicomUtil.getBeamNameOfRtimage(rtplan.get, rtimage)
      //noinspection SpellCheckingInspection
      name.isDefined && name.get.toLowerCase.matches(".*stak+[aeiouy]t+.*")
    }

    rtplan.isDefined && nameIsStakitt()
  }

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, RunReqClass] = {

    val rtimageList = getRtimageList(alList)
    logger.info("Number of RTIMAGE files uploaded: " + rtimageList.size)

    val planUID = rtimageList.flatMap(Phase2Util.referencedPlanUIDOpt).distinct.headOption

    val rtplan: Option[AttributeList] = planUID.flatMap(pUID => Phase2Util.fetchRtplan(pUID, alList))

    val stakittList = rtimageList.filter(img => isStakitt(img, rtplan))

    val result: Either[WebUtil.StyleMapT, SktRunReq] = 0 match {
      case _ if rtplan.isEmpty      => formError("RTPLAN could not be found. Try uploading it with the RTIMAGE file(s).")
      case _ if stakittList.isEmpty => formError("No Stakitt RTIMAGE files uploaded")
      case _ =>
        val runReq = SktRunReq(Util.sortByDateTime(rtimageList), rtplan.get)
        Right(runReq)
    }
    result
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], oldOutput: Option[Output]): SktRunReq = {
    val rtplan = alList.filter(Util.isRtimage).flatMap(Phase2Util.referencedPlanUIDOpt).headOption match {
      case Some(rtplanUid) => Phase2Util.fetchRtplan(rtplanUid, alList)
      case _               => None
    }

    val stakittList = Util.sortByDateTime(getRtimageList(alList).filter(img => isStakitt(img, rtplan)))
    val result = SktRunReq(stakittList, rtplan.get)
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
