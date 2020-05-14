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
 * Provide the user interface and verify that the data provided is sufficient to do the analysis.
 */

object BBbyEPIDRun extends Logging {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"

  /**
   * Determine if user is authorized to perform redo.  To be authorized, the user must be from the
   * same institution as the original user.
   *
   * Being whitelisted is not sufficient, because things just get weird in terms of viewing and
   * ownership of the data.
   */
  private def userAuthorizedToModify(request: Request, response: Response, input: Input): Boolean = {
    val user = CachedUser.get(request).get

    val mach = Machine.get(input.machinePK.get).get
    val dataInstitution = mach.institutionPK
    val requestorsInstitution = user.institutionPK
    val same = dataInstitution == requestorsInstitution
    logger.info("user requesting redo.  Authorized: " + same)
    same
  }

  //  private def processRedoRequest(request: Request, response: Response, inputOrig: Input, outputOrig: Output, await: Boolean, isAuto: Boolean) = {
  //    logger.info("Processing redo request for outputPK " + outputOrig.outputPK.get)
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
  //    val epidList = dicomFileList.filter(df => df.isRtimage)
  //
  //    val acquisitionDate = inputOrig.dataDate match {
  //      case None => None
  //      case Some(timestamp) => Some(timestamp.getTime)
  //    }
  //
  //    val runReq = new BBbyEPIDRunReq(epidList, Machine.get(outputOrig.machinePK.get).get)
  //
  //    val procedure = Procedure.get(outputOrig.procedurePK).get
  //
  //    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request),
  //      Util.getOutputPatientId(runReq.epidList.head), Util.getOutputDateTime(runReq.epidList))
  //
  //    val input = inputOutput._1
  //    val output = inputOutput._2
  //
  //    val future = Future {
  //      val extendedData = ExtendedData.get(output)
  //      val runReqFinal = runReq.reDir(input.dir)
  //
  //      val machine = runReqFinal.machine
  //
  //      val finalStatus = BBbyEPIDAnalyse.runProcedure(extendedData, runReqFinal)
  //      val finDate = new Timestamp(System.currentTimeMillis)
  //      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
  //
  //      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, runReqFinal.epidList)
  //
  //      Phase2Util.setMachineSerialNumber(machine, runReq.epidList.head)
  //      outputFinal.insertOrUpdate
  //      outputFinal.updateData(outputFinal.makeZipOfFiles)
  //      Run.removeRedundantOutput(outputFinal.outputPK)
  //      // remove the original input and all associated outputs to clean up any possible redundant data
  //      Input.delete(inputOrig.inputPK.get)
  //    }
  //
  //    awaitIfRequested(future, await, inputOutput._2.procedurePK)
  //    ViewOutput.redirectToViewRunProgress(response, isAuto, output.outputPK.get)
  //    logger.info("Finished processing redo request for outputPK " + outputOrig.outputPK.get)
  //  }

  //  /**
  //   * Tell the user that the redo is forbidden and why.  Also give them a redirect back to the list of results.
  //   */
  //   private def forbidRedo(response: Response, msg: String, outputPK: Option[Long]) {
  //     val content = {
  //       <div class="row">
  //         <div class="col-md-4 col-md-offset-2">
  //           { msg }
  //           <p></p>
  //           <a href={ OutputList.path } class="btn btn-default" role="button">Back</a>
  //         </div>
  //       </div>
  //     }
  //
  //     logger.info(msg + "  ouputPK: " + outputPK)
  //     val text = wrapBody(content, "Redo not permitted")
  //     setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  //   }

  /**
   * Given an output, redo the analysis.
   */
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
  //            case Some(inputOrig) => BBbyEPIDRun.processRedoRequest(request, response, inputOrig, outputOrig, await, isAuto)
  //          }
  //        }
  //      }
  //    } catch {
  //      case t: Throwable => logger.warn("Unable to redo output " + outputPK + " : " + fmtEx(t))
  //    }
  //  }

}

/**
 * Run BBbyEPID code.
 */
class BBbyEPIDRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[BBbyEPIDRunReq] {

  private def getEpidList(alList: Seq[AttributeList]) = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
  //  private def getSeries(dicomFile: DicomFile): String = getSeries(dicomFile.attributeList.get)

  override def getProcedure = procedure

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

  def validateMachineSelection(valueMap: ValueMapT, epidList: Seq[AttributeList]): Either[StyleMapT, Machine] = {
    val serNoByImage = {
      epidList.
        map(al => DicomUtil.findAllSingle(al, TagFromName.DeviceSerialNumber)).flatten.
        map(serNo => serNo.getSingleStringValueOrEmptyString).distinct
    }

    val machList = serNoByImage.distinct.map(serNo => Machine.findMachinesBySerialNumber(serNo)).flatten

    val distinctMachListSerNo = machList.map(mach => mach.machinePK).flatten.distinct

    // machine user chose from list
    val chosenMachine = for (pkTxt <- valueMap.get(RunProcedure.machineSelectorLabel); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m

    val result: Either[StyleMapT, Machine] = 0 match {
      case _ if (distinctMachListSerNo.size == 1) => Right(machList.head)
      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
      case _ if (distinctMachListSerNo.size > 1) => formError("Files come from more than one machine; please Cancel and try again.")
      case _ => formError("Unknown machine.  Please choose from the 'Machine' list below or click Cancel and then use the Administration interface to add it.")
    }
    result
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  override def validate(valueMap: ValueMapT, form: WebForm, request: Request, response: Response, alList: Seq[AttributeList]): Either[StyleMapT, BBbyEPIDRunReq] = {
    val epidList = alList.filter(al => Util.modalityOfAl(al).trim.equalsIgnoreCase("RTIMAGE"))
    val angleList = epidList.map(epid => Util.gantryAngle(epid))
    def angleTextList = angleList.map(a => Util.fmtDbl(a)).mkString("  ")
    // true if all angles are valid
    val anglesTypeList = angleList.map(angle => AngleType.classifyAngle(angle)).flatten

    def epidSeriesList = epidList.map(epid => getSeries(epid)).distinct
    def frameOfRefList = epidList.map(epid => epid.get(TagFromName.FrameOfReferenceUID)).filterNot(attr => attr == null).map(attr => attr.getSingleStringValueOrEmptyString).distinct

    def machineCheck = validateMachineSelection(valueMap, epidList)

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

  //  /**
  //   * Given an image list, find the one with the earliest date/time.
  //   */
  //  private def dateTimePatId(rtimageList: Seq[AttributeList]): (Option[Long], Option[String]) = {
  //    //  Util.dateTimeAndPatientIdFromDicom(rtimageList.head)   fffffff   TODO
  //    val list = rtimageList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al)).filter(dt => dt._1.nonEmpty && dt._2.isDefined)
  //    val date: Option[Long] = {
  //      val dateList = list.map(dp => dp._1.headOption).flatten
  //      if (dateList.isEmpty) None else Some(dateList.map(d => d.getTime).min)
  //    }
  //    val patient: Option[String] = list.map(dp => dp._2).flatten.headOption
  //    (date, patient)
  //  }

  override def run(extendedData: ExtendedData, runReq: BBbyEPIDRunReq): ProcedureStatus.Value = {
    val status = BBbyEPIDAnalyse.runProcedure(extendedData, runReq)
    status
  }

  //    logger.info("EPID Data is valid.  Preparing to analyze data.")
  //    val patientId = {
  //      val at = runReq.epidList.head.get(TagFromName.PatientID)
  //      if ((at != null) && (at.getSingleStringValueOrNull != null))
  //        Some(at.getSingleStringValueOrNull)
  //      else
  //        None
  //    }
  //
  //    val sessDir = sessionDir(valueMap).get
  //    val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(response.getRequest),
  //      Util.getOutputPatientId(runReq.epidList.head), Util.getOutputDateTime(runReq.epidList))
  //    val input = inputOutput._1
  //    val output = inputOutput._2
  //
  //    def perform = {
  //      val extendedData = ExtendedData.get(output)
  //      val runReqFinal = runReq.reDir(input.dir)
  //      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, runReqFinal.epidList)
  //
  //      val finDate = new Timestamp(System.currentTimeMillis)
  //      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
  //
  //      Phase2Util.setMachineSerialNumber(extendedData.machine, runReq.epidList.head)
  //      outputFinal.insertOrUpdate
  //      outputFinal.updateData(outputFinal.makeZipOfFiles)
  //      Run.removeRedundantOutput(outputFinal.outputPK)
  //      logger.info("EPID processing has completed")
  //      Util.garbageCollect
  //    }
  //
  //    // if awaiting, then wait for completion, otherwise do it in the background
  //    if (isAwait(valueMap)) {
  //      perform
  //    } else {
  //      Future { perform }
  //    }
  //    logger.info("Redirecting web client to view run progress of EPID processing.")
  //    ViewOutput.redirectToViewRunProgress(response, valueMap, output.outputPK.get)
  //  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handle(valueMap, request, response, this.asInstanceOf[RunTrait[RunReqClass]])
  }

}
