package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.ImageRegistration
import org.aqa.Util
import org.aqa.db.BBbyEPID
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.OutputList
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.sql.Timestamp

/**
 * Provide the user interface and verify that the data provided is sufficient to do the analysis.
 */

/**
 * Run BBbyCBCT code.
 */
class BBbyCBCTRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[BBbyCBCTRunReq] {

  override def getPatientID(valueMap: org.aqa.web.WebUtil.ValueMapT, alList: Seq[com.pixelmed.dicom.AttributeList]): Option[String] = {
    alList.filter(al => Util.isCt(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def makeRunReqForRedo(alList: Seq[AttributeList], output: Option[Output]): org.aqa.run.RunReqClass = {
    //validate(emptyValueMap, alList.filter(al => Util.isCt(al))).right.get
    val cbctList = alList.filter(al => Util.isCt(al)).sortBy(al => al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2))
    val reg = alList.find(al => Util.isReg(al))
    val ctFrameUID = cbctList.head.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
    val rtplan: AttributeList = {
      val ds = DicomSeries.getByFrameUIDAndSOPClass(Set(ctFrameUID), SOPClass.RTPlanStorage)
      if (ds.nonEmpty) ds.head.attributeListList.head
      else {
        val planFrameUID = reg.get.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
        DicomSeries.getByFrameUIDAndSOPClass(Set(planFrameUID), SOPClass.RTPlanStorage).head.attributeListList.head
      }
    }
    BBbyCBCTRunReq(rtplan, reg, cbctList)
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
    val cbctList = alList.filter(al => Util.isCt(al))

    def getTimestamp(dateTag: AttributeTag, timeTag: AttributeTag): Option[Timestamp] = {
      val msList = cbctList.flatMap(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).map(dt => dt.getTime)
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

  override def getProcedure: Procedure = procedure

  /**
   * Look through EPID results for one that would have used this CBCT output had it been available
   * when the EPID was processed.  If there are any such EPID results, then redo them.
   */
  private def checkForEpidRedo(cbctOutput: Output, response: Response): Unit = {
    val machine = Machine.get(cbctOutput.machinePK.get).get
    val endOfDay = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(cbctOutput.dataDate.get).replaceAll("T.*", "T00:00:00")).getTime + (24 * 60 * 60 * 1000))

    // list of all output on this machine that has a data date after the cbctOutput.
    val allOutput = Output.getOutputByDateRange(machine.institutionPK, cbctOutput.dataDate.get, endOfDay).filter(o => o.machinePK == cbctOutput.machinePK)

    // the time of the next CBCT after this one.  If there is not one after this, then midnight of today.
    val cbctCeilingTime_ms = {
      // midnight of the dataDate
      val ofInterest = allOutput.
        filter(o => (o.machinePK == cbctOutput.machinePK) &&
          (o.procedurePK == cbctOutput.procedurePK) &&
          (o.outputPK.get != cbctOutput.outputPK.get)).
        sortBy(_.dataDate.get.getTime)

      val date = if (ofInterest.isEmpty) endOfDay else ofInterest.head.dataDate.get
      date.getTime
    }

    // List of EPIDs that have occurred after this CBCT but before cbctCeilingTime time
    val allEpidDaily = BBbyEPID.getForOneDay(cbctOutput.dataDate.get, machine.institutionPK).
      filter(dds => dds.machine.machinePK.get == machine.machinePK.get).
      filter(dds => (dds.output.dataDate.get.getTime < cbctCeilingTime_ms) && (dds.output.dataDate.get.getTime > cbctOutput.dataDate.get.getTime))

    // List of EPIDs to redo.  Note that because multiple BBbyEPID rows may refer to the
    // same output, only the single output (hence the 'distinct') needs to be done.
    val outputPKlist = allEpidDaily.map(dds => dds.output.outputPK.get).distinct

    logger.info("Number of EPIDs to redo because of CBCT data at " + cbctOutput.dataDate.get + " from machine " + machine.id + " : " + outputPKlist.size)

    def redo(outputPK: Long): Unit = {
      logger.info("BBbyCBCTRun starting redo of EPID output " + outputPK)
      (new OutputList).redoOutput(outputPK, response, await = true, isAuto = true)
      logger.info("BBbyCBCTRun finished redo of EPID output " + outputPK)
    }

    outputPKlist.foreach(o => redo(o))
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String] = {
    val ctList = alList.filter(al => Util.isCt(al) || Util.isReg(al))
    val dsnList = ctList.flatMap(al => Util.attributeListToDeviceSerialNumber(al)).distinct
    dsnList
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, BBbyCBCTRunReq] = {
    val cbctList = alList.filter(al => Util.isCt(al)).sortBy(al => Util.slicePosition(al))
    val regList = alList.filter(al => Util.isReg(al))
    val rtplanList = alList.filter(al => Util.isRtplan(al))

    val cbctSeriesList = cbctList.map(cbct => Util.serInstOfAl(cbct)).distinct

    def cbctFrameOfRefList = cbctList.map(cbct => Util.getFrameOfRef(cbct)).distinct

    // Make a list of REG files that support the CBCTs frame of reference and reference the CBCT by its SeriesInstanceUID.
    val qualRegList = {
      val cbctFrameOfRef = cbctFrameOfRefList.head
      val sameFrameOfRef = regList.filter(al => ImageRegistration(al).otherFrameOfRefUID.equals(cbctFrameOfRef))
      val cbctSeriesInstUID = Util.serInstOfAl(cbctList.head)

      def regReferencesCbct(reg: AttributeList): Boolean = {
        val list = DicomUtil.seqToAttr(reg, TagFromName.ReferencedSeriesSequence).map(al => al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString)
        val hasIt = list.contains(cbctSeriesInstUID)
        hasIt
      }

      val qualifiedList = sameFrameOfRef.filter(reg => regReferencesCbct(reg))
      Trace.trace("qualifiedList size: " + qualifiedList.size)
      qualifiedList
    }

    logger.info("Number of files uploaded:  RTPLAN: " + rtplanList.size + "    REG: " + regList.size + "    CBCT: " + cbctList.size)

    /** return list of plans (either uploaded or from DB) whose frame of reference match the CBCT exactly (no REG file involved) */
    def rtplanMatchingCbct: Seq[AttributeList] = {
      val cbctFramOfRef = cbctFrameOfRefList.head
      val dbPlan = DicomSeries.getByFrameUIDAndSOPClass(Set(cbctFramOfRef), SOPClass.RTPlanStorage).flatMap(db => db.attributeListList)

      val matching = (dbPlan ++ rtplanList).filter(plan => Util.getFrameOfRef(plan).equals(cbctFramOfRef))
      logger.info("Number of plans whose FrameOfReferenceUID matches CBCT exactly so they do not require a REG file: " + matching.size)
      matching
    }

    /**
     * Get the list of possible plan and reg pairs, preferring the uploaded plan(s) but also searching the plans in the database.
     */
    def getPlanAndReg: Seq[(AttributeList, AttributeList)] = {
      val uploadedPairList = for (plan <- rtplanList; reg <- qualRegList; if Util.getFrameOfRef(plan).equals(Util.getFrameOfRef(reg))) yield (plan, reg)
      if (uploadedPairList.nonEmpty)
        uploadedPairList
      else {
        val qualRegFrameOfRefList = qualRegList.map(df => Util.getFrameOfRef(df)).toSet
        val dbPlanList = DicomSeries.getByFrameUIDAndSOPClass(qualRegFrameOfRefList, SOPClass.RTPlanStorage).flatMap(db => db.attributeListList)

        val dbPairList = for (plan <- dbPlanList; reg <- qualRegList; if Util.getFrameOfRef(plan).equals(Util.getFrameOfRef(reg))) yield (plan, reg)
        dbPairList
      }
    }

    /** Get the plan to use, if there is one. */
    def rtplan: Option[AttributeList] = {
      0 match {
        case _ if getPlanAndReg.nonEmpty => Some(getPlanAndReg.head._1)
        case _ if rtplanMatchingCbct.nonEmpty => Some(rtplanMatchingCbct.head)
        case _ => None
      }
    }

    val result = 0 match {
      case _ if cbctList.isEmpty => formError("No CBCT files uploaded")
      case _ if cbctSeriesList.size > 1 => formError("CBCT slices are from " + cbctSeriesList.size + " different series.")
      case _ if cbctFrameOfRefList.isEmpty => formError("CBCT series are unusable: They do not specify a frame of reference.")
      case _ if cbctFrameOfRefList.size > 1 => formError("CBCT series uses more than one frame of reference.")
      case _ if rtplan.isEmpty => formError("Can not find a CBCT + REG + RTPLAN with compatible frame of reference.  Note that REG must reference CBCT.")
      case _ =>
        val plan = {
          val planAl = rtplan.get
          val planSop = Util.sopOfAl(planAl)
          val uploaded = rtplanList.find(al => Util.sopOfAl(al).equals(planSop))
          if (uploaded.isDefined)
            uploaded.get
          else
            planAl
        }

        val reg =
          if (rtplanMatchingCbct.nonEmpty)
            None
          else {
            val regSop = Util.sopOfAl(getPlanAndReg.head._2)
            regList.find(al => Util.sopOfAl(al).equals(regSop))
          }

        val runReq = BBbyCBCTRunReq(plan, reg, cbctList)
        Right(runReq)
    }
    result
  }

  override def run(extendedData: ExtendedData, runReq: BBbyCBCTRunReq, response: Response): ProcedureStatus.Value = {
    val status = BBbyCBCTExecute.runProcedure(extendedData, runReq, response)
    checkForEpidRedo(extendedData.output, response)
    status
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handle(valueMap, request, response, this.asInstanceOf[RunTrait[RunReqClass]])
  }

}
