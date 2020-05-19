package org.aqa.webrun.bbByCBCT

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
import org.aqa.web.WebUtil
import org.aqa.web.Session
import org.aqa.db.CachedUser
import org.aqa.web.OutputList
import org.aqa.webrun.ExtendedData
import org.aqa.ImageRegistration
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.db.BBbyCBCT
import org.restlet.data.Method
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import org.aqa.db.BBbyEPID
import org.aqa.webrun.bbByEpid.BBbyEPIDRun
import org.aqa.run.RunTrait
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import com.pixelmed.dicom.AttributeTag

/**
 * Provide the user interface and verify that the data provided is sufficient to do the analysis.
 */

//object BBbyCBCTRun extends Logging {
//  val parametersFileName = "parameters.xml"
//  val Phase2RunPKTag = "Phase2RunPK"
//
//  /**
//   * Look through EPID results for one that would have used this CBCT output had it been available
//   * when the EPID was processed.  If there are any such EPID results, then redo them.
//   */
//  private def checkForEpidRedo(cbctOutput: Output, response: Response): Unit = {
//    val machine = Machine.get(cbctOutput.machinePK.get).get
//    val endOfDay = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(cbctOutput.dataDate.get).replaceAll("T.*", "T00:00:00")).getTime + (24 * 60 * 60 * 1000))
//
//    // list of all output on this machine that has a data date after the cbctOutput.
//    val allOutput = Output.getOutputByDateRange(machine.institutionPK, cbctOutput.dataDate.get, endOfDay).filter(o => (o.machinePK == cbctOutput.machinePK))
//
//    // the time of the next CBCT after this one.  If there is not one after this, then midnight of today.
//    val cbctCeilingTime_ms = {
//      // midnight of the dataDate
//      val ofInterest = allOutput.
//        filter(o => (o.machinePK == cbctOutput.machinePK) &&
//          (o.procedurePK == cbctOutput.procedurePK) &&
//          (o.outputPK.get != cbctOutput.outputPK.get)).
//        sortBy(_.dataDate.get.getTime)
//
//      val date = if (ofInterest.isEmpty) endOfDay else ofInterest.head.dataDate.get
//      date.getTime
//    }
//
//    // List of EPIDs that have occurred after this CBCT but before cbctCeilingTime time
//    val allEpidDaily = BBbyEPID.getForOneDay(cbctOutput.dataDate.get, machine.institutionPK).
//      filter(dds => dds.machine.machinePK.get == machine.machinePK.get).
//      filter(dds => ((dds.output.dataDate.get.getTime < cbctCeilingTime_ms) && (dds.output.dataDate.get.getTime > cbctOutput.dataDate.get.getTime)))
//
//    // List of EPIDs to redo.  Note that because multiple BBbyEPID rows may refer to the
//    // same output, only the single output (hence the 'distinct') needs to be done.
//    val outputPKlist = allEpidDaily.map(dds => dds.output.outputPK.get).distinct
//
//    logger.info("Number of EPIDs to redo because of CBCT data at " + cbctOutput.dataDate.get + " from machine " + machine.id + " : " + outputPKlist.size)
//
//    def redo(outputPK: Long) = {
//      logger.info("BBbyCBCTRun starting redo of EPID output " + outputPK)
//      // BBbyEPIDRun.redo(outputPK, response.getRequest, response, true, true)  // TODO fix after transition to run2
//      logger.info("BBbyCBCTRun finished redo of EPID output " + outputPK)
//    }
//
//    outputPKlist.map(o => redo(o))
//  }
//
//  /**
//   * Determine if user is authorized to perform redo.  To be authorized, the user must be from the
//   * same institution as the original user.
//   *
//   * Being whitelisted is not sufficient, because things just get weird in terms of viewing and
//   * ownership of the data.
//   */
//  private def userAuthorizedToModify(request: Request, response: Response, input: Input): Boolean = {
//    val user = CachedUser.get(request).get
//
//    val mach = Machine.get(input.machinePK.get).get
//    val dataInstitution = mach.institutionPK
//    val requestorsInstitution = user.institutionPK
//    val same = dataInstitution == requestorsInstitution
//    logger.info("user requesting redo.  Authorized: " + same)
//    same
//  }
//
//  private def processRedoRequest(request: Request, response: Response, inputOrig: Input, outputOrig: Output, await: Boolean, isAuto: Boolean) = {
//
//    Output.ensureInputAndOutputFilesExist(outputOrig)
//    val sessionId = Session.makeUniqueId
//    val sessionDir = Session.idToFile(sessionId)
//    sessionDir.mkdirs
//    def copyToSessionDir(file: File) = {
//      val newFile = new File(sessionDir, file.getName)
//      newFile.createNewFile
//      val data = Util.readBinaryFile(file).right.get
//      Util.writeBinaryFile(newFile, data)
//    }
//    val inputFileList = Util.listDirFiles(inputOrig.dir).filter(f => f.isFile)
//    inputFileList.map(copyToSessionDir)
//    logger.info("Copied input files from " + inputOrig.dir.getAbsolutePath + " --> " + sessionDir.getAbsolutePath)
//
//    val dicomFileList = Util.listDirFiles(sessionDir).map(f => new DicomFile(f)).filter(df => df.attributeList.nonEmpty)
//    val cbctList = dicomFileList.filter(df => df.isCt)
//    val regList = dicomFileList.filter(df => df.isReg)
//    val rtplanList = dicomFileList.filter(df => df.isRtplan)
//
//    val acquisitionDate = inputOrig.dataDate match {
//      case None => None
//      case Some(timestamp) => Some(timestamp.getTime)
//    }
//
//    /** return the plan (either uploaded or from DB) whose frame of reference match the CBCT exactly (no REG file involved) */
//    val rtplanFrmOfRefRMatchesCBCT: Option[AttributeList] = {
//      def cbctFrameOfRefList = cbctList.map(cbct => Util.getFrameOfRef(cbct)).distinct
//
//      val cbctFramOfRef = cbctFrameOfRefList.head
//      val dbPlan = DicomSeries.getByFrameUIDAndSOPClass(Set(cbctFramOfRef), SOPClass.RTPlanStorage).map(db => db.attributeListList).flatten
//
//      val uploadedRtplanList = rtplanList.map(df => df.attributeList).flatten
//
//      val seq = (dbPlan ++ uploadedRtplanList).filter(plan => Util.getFrameOfRef(plan).equals(cbctFramOfRef))
//      logger.info("redo: Number of plans whose FrameOfReferenceUID matches CBCT exactly so they do not require a REG file: " + seq.size)
//      seq.headOption
//    }
//
//    val runReq =
//      if (rtplanFrmOfRefRMatchesCBCT.isDefined) {
//        // no REG required
//        val machinePK = Seq(outputOrig.machinePK, inputOrig.machinePK).flatten.head
//        new BBbyCBCTRunReq(Right(rtplanFrmOfRefRMatchesCBCT.get), None, cbctList, Machine.get(machinePK).get)
//      } else {
//        // REG is required
//        val cbctFrmRef = Util.getFrameOfRef(cbctList.head.attributeList.get)
//        val irList = regList.map(r => new ImageRegistration(r.attributeList.get))
//        val regFrmOfRefSeq = regList.map(r => new ImageRegistration(r.attributeList.get).frameOfRefUID)
//        val rtplanListFromDb = DicomSeries.getByFrameUIDAndSOPClass(regFrmOfRefSeq.toSet, SOPClass.RTPlanStorage)
//
//        val pairList = for (
//          ir <- irList;
//          p <- rtplanListFromDb;
//          if ((ir.otherFrameOfRefUID.equals(cbctFrmRef) && ir.frameOfRefUID.equals(p.frameOfReferenceUID.get)))
//        ) yield (ir.attrList, p.attributeListList.head)
//
//        val regAl = pairList.head._1
//        val planAl = pairList.head._2
//        val regDf = regList.find(df => Util.sopOfAl(df.attributeList.get).equals(Util.sopOfAl(regAl))).get
//
//        new BBbyCBCTRunReq(Right(planAl), Some(regDf), cbctList, Machine.get(outputOrig.machinePK.get).get)
//      }
//
//    val procedure = Procedure.get(outputOrig.procedurePK).get
//
//    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request), inputOrig.patientId, acquisitionDate)
//    val input = inputOutput._1
//    val output = inputOutput._2
//
//    val future = Future {
//      Trace.trace("In Future, fetching data for CBCT processing.")
//      logger.info("In Future, fetching data for CBCT processing.")
//      val extendedData = ExtendedData.get(output)
//      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, Seq(runReq.rtplan))
//      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, runReq.cbct)
//      if (runReq.regDicomFile.isDefined && runReq.regDicomFile.get.attributeList.isDefined) {
//        DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, Seq(runReq.regDicomFile.get.attributeList.get))
//      }
//
//      val runReqFinal = runReq.reDir(input.dir)
//
//      val rtplan = runReqFinal.rtplan
//      val machine = runReqFinal.machine
//
//      val finalStatus = BBbyCBCTExecute.runProcedure(extendedData, runReqFinal)
//      val finDate = new Timestamp(System.currentTimeMillis)
//      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
//
//      Phase2Util.setMachineSerialNumber(machine, runReq.cbct.head)
//      outputFinal.insertOrUpdate
//      outputFinal.updateData(outputFinal.makeZipOfFiles)
//      Run.removeRedundantOutput(outputFinal.outputPK)
//      try {
//        // remove the original input and all associated outputs to clean up any possible redundant data
//        Input.delete(inputOrig.inputPK.get)
//      } catch {
//        case t: Throwable => logger.error("Could not delete input " + inputOrig.inputPK.get + " : " + t)
//      }
//      BBbyCBCTRun.checkForEpidRedo(outputFinal, response)
//      logger.info("In Future, finished CBCT processing.")
//      Trace.trace("In Future, finished CBCT processing.")
//    }
//
//    awaitIfRequested(future, await, inputOutput._2.procedurePK)
//    ViewOutput.redirectToViewRunProgress(response, isAuto, output.outputPK.get)
//  }
//
//  /**
//   * Tell the user that the redo is forbidden and why.  Also give them a redirect back to the list of results.
//   */
//  private def forbidRedo(response: Response, msg: String, outputPK: Option[Long]) {
//    /*
//    val content = {
//      <div class="row">
//        <div class="col-md-4 col-md-offset-2">
//          { msg }
//          <p></p>
//          <a href={ OutputList.path } class="btn btn-default" role="button">Back</a>
//        </div>
//      </div>
//    }
//    */
//
//    logger.info(msg + "  ouputPK: " + outputPK)
//    val text = wrapBody(content, "Redo not permitted")
//    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
//  }
//
//  /**
//   * Given an output, redo the analysis.
//   */
//  def redo(outputPK: Long, request: Request, response: Response, await: Boolean, isAuto: Boolean) = {
//    try {
//      Output.get(outputPK) match {
//        case None => {
//          logger.info("Redo of output " + outputPK + " not possible because output does not exist")
//          val msg = "Redo not possible because output does not exist"
//          forbidRedo(response, msg, None)
//        }
//        case Some(outputOrig) => {
//          Input.get(outputOrig.inputPK) match {
//            case None => {
//              val msg = "Redo not possible because input does not exist"
//              forbidRedo(response, msg, outputOrig.outputPK)
//            }
//            case Some(inputOrig) if (!userAuthorizedToModify(request, response, inputOrig)) => {
//              val msg = "Redo not permitted because user is from a different institution."
//              forbidRedo(response, msg, outputOrig.outputPK)
//            }
//            case Some(inputOrig) => BBbyCBCTRun.processRedoRequest(request, response, inputOrig, outputOrig, await, isAuto)
//          }
//        }
//      }
//    } catch {
//      case t: Throwable => logger.warn("Unable to redo output " + outputPK + " : " + fmtEx(t))
//    }
//  }
//
//}

/**
 * Run BBbyCBCT code.
 */
class BBbyCBCTRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[BBbyCBCTRunReq] {

  override def getPatientID(valueMap: org.aqa.web.WebUtil.ValueMapT, alList: Seq[com.pixelmed.dicom.AttributeList]): Option[String] = {
    alList.filter(al => Util.isCt(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def makeRunReq(alList: Seq[com.pixelmed.dicom.AttributeList]): org.aqa.run.RunReqClass = {
    validate(emptyValueMap, alList).right.get
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
    val cbctList = alList.filter(al => Util.isCt(al))

    def getTimestamp(dateTag: AttributeTag, timeTag: AttributeTag): Option[Timestamp] = {
      val msList = cbctList.map(al => DicomUtil.getTimeAndDate(al, dateTag, timeTag)).flatten.map(dt => dt.getTime)
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

  override def getProcedure = procedure

  /**
   * Look through EPID results for one that would have used this CBCT output had it been available
   * when the EPID was processed.  If there are any such EPID results, then redo them.
   */
  private def checkForEpidRedo(cbctOutput: Output): Unit = {
    val machine = Machine.get(cbctOutput.machinePK.get).get
    val endOfDay = new Timestamp(Util.standardDateFormat.parse(Util.standardDateFormat.format(cbctOutput.dataDate.get).replaceAll("T.*", "T00:00:00")).getTime + (24 * 60 * 60 * 1000))

    // list of all output on this machine that has a data date after the cbctOutput.
    val allOutput = Output.getOutputByDateRange(machine.institutionPK, cbctOutput.dataDate.get, endOfDay).filter(o => (o.machinePK == cbctOutput.machinePK))

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
      filter(dds => ((dds.output.dataDate.get.getTime < cbctCeilingTime_ms) && (dds.output.dataDate.get.getTime > cbctOutput.dataDate.get.getTime)))

    // List of EPIDs to redo.  Note that because multiple BBbyEPID rows may refer to the
    // same output, only the single output (hence the 'distinct') needs to be done.
    val outputPKlist = allEpidDaily.map(dds => dds.output.outputPK.get).distinct

    logger.info("Number of EPIDs to redo because of CBCT data at " + cbctOutput.dataDate.get + " from machine " + machine.id + " : " + outputPKlist.size)

    def redo(outputPK: Long) = {
      logger.info("BBbyCBCTRun starting redo of EPID output " + outputPK)
      // BBbyEPIDRun.redo(outputPK, response.getRequest, response, true, true)  // TODO fix after transition to run2
      logger.info("BBbyCBCTRun finished redo of EPID output " + outputPK)
    }

    outputPKlist.map(o => redo(o))
  }

  override def getMachineDeviceSerialNumber(runReq: BBbyCBCTRunReq): String = {
    RunProcedure.getDeviceSerialNumber(runReq.cbctList).head
  }

  override def getMachine(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Machine] = {
    val cbctList = alList.filter(al => Util.isCt(al))

    val dsnList = cbctList.map(al => al.get(TagFromName.DeviceSerialNumber)).filterNot(_ == null).map(a => a.getSingleStringValueOrNull).filterNot(_ == null).distinct
    val machList = dsnList.map(dsn => Machine.findMachinesBySerialNumber(dsn)).flatten

    machList.headOption
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

    // Make a list of REG files that support the CBCT's frame of reference.  We don't care about the other REG files.
    val qualRegList = {
      val cbctFrameOfRef = cbctFrameOfRefList.head
      regList.filter(al => new ImageRegistration(al).otherFrameOfRefUID.equals(cbctFrameOfRef))
    }

    logger.info("Number of files uploaded:  RTPLAN: " + rtplanList.size + "    REG: " + regList.size + "    CBCT: " + cbctList.size)

    def numSeries(dfList: Seq[DicomFile]): Int = dfList.map(df => df.attributeList.get.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    /** return list of plans (either uploaded or from DB) whose frame of reference match the CBCT exactly (no REG file involved) */
    def rtplanMatchingCbct: Seq[AttributeList] = {
      val cbctFramOfRef = cbctFrameOfRefList.head
      val dbPlan = DicomSeries.getByFrameUIDAndSOPClass(Set(cbctFramOfRef), SOPClass.RTPlanStorage).map(db => db.attributeListList).flatten

      val matching = (dbPlan ++ rtplanList).filter(plan => Util.getFrameOfRef(plan).equals(cbctFramOfRef))
      logger.info("Number of plans whose FrameOfReferenceUID matches CBCT exactly so they do not require a REG file: " + matching.size)
      matching
    }

    /**
     * Get the list of possible plan and reg pairs, preferring the uploaded plan(s) but also searching the plans in the database.
     */
    def getPlanAndReg: Seq[(AttributeList, AttributeList)] = {
      val uploadedPairList = for (plan <- rtplanList; reg <- qualRegList; if (Util.getFrameOfRef(plan).equals(Util.getFrameOfRef(reg)))) yield (plan, reg)
      if (uploadedPairList.nonEmpty)
        uploadedPairList
      else {
        val qualRegFrameOfRefList = qualRegList.map(df => Util.getFrameOfRef(df)).toSet
        val dbPlanList = DicomSeries.getByFrameUIDAndSOPClass(qualRegFrameOfRefList, SOPClass.RTPlanStorage).map(db => db.attributeListList).flatten

        val dbPairList = for (plan <- dbPlanList; reg <- qualRegList; if (Util.getFrameOfRef(plan).equals(Util.getFrameOfRef(reg)))) yield (plan, reg)
        dbPairList
      }
    }

    /** Return true if the CBCT frame of reference matches a plan, or, if there is a CBCT-REG-RTPLAN combination that works. */
    //def isMatchingFrameOfRef = getPlanAndReg.nonEmpty || rtplanMatchingCbct.nonEmpty

    /** Get the plan to use, if there is one. */
    def rtplan: Option[AttributeList] = {
      0 match {
        case _ if (getPlanAndReg.nonEmpty) => Some(getPlanAndReg.head._1)
        case _ if (rtplanMatchingCbct.nonEmpty) => Some(rtplanMatchingCbct.head)
        case _ => None
      }
    }

    def machineCheck = RunProcedure.validateMachineSelection(valueMap, cbctList)

    val result = 0 match {
      case _ if cbctList.isEmpty => formError("No CBCT files uploaded")
      case _ if cbctSeriesList.size > 1 => formError("CBCT slices are from " + cbctSeriesList.size + " different series.")
      case _ if cbctFrameOfRefList.isEmpty => formError("CBCT series are unusable: They do not specify a frame of reference.")
      case _ if cbctFrameOfRefList.size > 1 => formError("CBCT series uses more than one frame of reference.")
      case _ if rtplan.isEmpty => formError("Can not find a CBCT + REG + RTPLAN with compatible frame of reference.")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ => {
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

        val runReq = new BBbyCBCTRunReq(plan, reg, cbctList)
        //val runReq = new BBbyCBCTRunReq()
        Right(runReq)

      }
    }
    result
  }

  //  /**
  //   * Given an image list, find the one with the earliest date/time.
  //   */
  //  private def dateTimePatId(rtimageList: Seq[AttributeList]): (Option[Long], Option[String]) = {
  //    val list = rtimageList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al)).filter(dt => dt._1.nonEmpty && dt._2.isDefined)
  //    val date: Option[Long] = {
  //      val dateList = list.map(dp => dp._1.headOption).flatten
  //      if (dateList.isEmpty) None else Some(dateList.map(d => d.getTime).min)
  //    }
  //    val patient: Option[String] = list.map(dp => dp._2).flatten.headOption
  //    (date, patient)
  //  }

  override def run(extendedData: ExtendedData, runReq: BBbyCBCTRunReq): ProcedureStatus.Value = {
    val status = BBbyCBCTExecute.runProcedure(extendedData, runReq)
    checkForEpidRedo(extendedData.output)
    status
  }

  //  /**
  //   * All the proper data is available to process.
  //   */
  //  private def run(valueMap: ValueMapT, runReq: BBbyCBCTRunReq, response: Response) = {
  //    logger.info("CBCT Data is valid.  Preparing to analyze data.")
  //    val dtpList = runReq.cbctList.map(c => Util.extractDateTimeAndPatientIdFromDicomAl(c))
  //    val PatientID = dtpList.map(dtp => dtp._2).flatten.headOption
  //    val dateTime = dtpList.map(dtp => dtp._1.headOption).flatten.map(d => d.getTime).sorted.headOption
  //
  //    val sessDir = sessionDir(valueMap).get
  //    val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(response.getRequest), PatientID, dateTime)
  //    val input = inputOutput._1
  //    val output = inputOutput._2
  //
  //    def perform = {
  //      val extendedData = ExtendedData.get(output)
  //      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, Seq(runReq.rtplan))
  //
  //      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, runReq.cbct)
  //      if (runReq.regDicomFile.isDefined && runReq.regDicomFile.get.attributeList.isDefined) {
  //        DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, Seq(runReq.regDicomFile.get.attributeList.get))
  //      }
  //
  //      val runReqFinal = runReq.reDir(input.dir)
  //
  //      val finalStatus = BBbyCBCTExecute.runProcedure(extendedData, runReqFinal)
  //      val finDate = new Timestamp(System.currentTimeMillis)
  //      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
  //
  //      Phase2Util.setMachineSerialNumber(extendedData.machine, runReq.cbct.head)
  //      outputFinal.insertOrUpdate
  //      outputFinal.updateData(outputFinal.makeZipOfFiles)
  //      Run.removeRedundantOutput(outputFinal.outputPK)
  //      BBbyCBCTRun.checkForEpidRedo(outputFinal, response)
  //      Util.garbageCollect
  //      logger.info("CBCT processing has completed")
  //    }
  //
  //    // if awaiting, then wait for completion, otherwise do it in the background
  //    if (isAwait(valueMap)) {
  //      perform
  //    } else {
  //      Future { perform }
  //    }
  //    logger.info("Redirecting web client to view run progress of CBCT processing.")
  //    ViewOutput.redirectToViewRunProgress(response, valueMap, output.outputPK.get)
  //  }

  //  /**
  //   * Respond to the 'Run' button.
  //   */
  //  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response) = {
  //    logger.info("Validating data")
  //    validate(valueMap) match {
  //      case Left(errMap) => {
  //        logger.info("BBbyCBCTRun Bad request: " + errMap.keys.map(k => k + " : " + valueMap.get(k)).mkString("\n    "))
  //        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
  //      }
  //      case Right(runReq) => {
  //        if (isAwait(valueMap)) awaitTag.synchronized(run(valueMap, runReq, response))
  //        else run(valueMap, runReq, response)
  //      }
  //    }
  //  }

  //  /**
  //   * Cancel the procedure.  Remove files and redirect to procedure list.
  //   */
  //  private def cancel(valueMap: ValueMapT, response: Response) = {
  //    sessionDir(valueMap) match {
  //      case Some(dir) => Utility.deleteFileTree(dir)
  //      case _ => ;
  //    }
  //    WebRunIndex.redirect(response)
  //  }
  //
  //  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
  //    val value = valueMap.get(button.label)
  //    val is = value.isDefined && value.get.toString.equals(button.label)
  //    is
  //  }
  //
  //  override def handle(request: Request, response: Response): Unit = {
  //    super.handle(request, response)
  //
  //    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
  //    val redo = valueMap.get(OutputList.redoTag)
  //    val del = valueMap.get(OutputList.deleteTag)
  //
  //    try {
  //      0 match {
  //        case _ if buttonIs(valueMap, cancelButton) => cancel(valueMap, response)
  //        case _ if buttonIs(valueMap, runButton) => runIfDataValid(valueMap, request, response)
  //        case _ => emptyForm(valueMap, response)
  //      }
  //    } catch {
  //      case t: Throwable => {
  //        internalFailure(response, "Unexpected failure: " + fmtEx(t))
  //      }
  //    }
  //  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handle(valueMap, request, response, this.asInstanceOf[RunTrait[RunReqClass]])
  }

}
