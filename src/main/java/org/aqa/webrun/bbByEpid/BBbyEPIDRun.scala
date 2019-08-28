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

  /**
   * Add this series to the database if it is not already in.  Use the SOPInstanceUID to determine if it is already in the database.
   */
  private def insertIfNew(alList: Seq[AttributeList], extendedData: ExtendedData) = {
    val sopUID = Util.sopOfAl(alList.head)

    val current = DicomSeries.getBySopInstanceUID(sopUID)
    if (current.isEmpty) {
      val ds = DicomSeries.makeDicomSeries(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, alList)
      logger.info("inserted DicomSeries in to database: " + ds)
      ds.insert
    }
  }

  private def processRedoRequest(request: Request, response: Response, inputOrig: Input, outputOrig: Output) = {

    Output.ensureInputAndOutputFilesExist(outputOrig)
    val sessionId = Session.makeUniqueId
    val sessionDir = Session.idToFile(sessionId)
    sessionDir.mkdirs
    def copyToSessionDir(file: File) = {
      val newFile = new File(sessionDir, file.getName)
      newFile.createNewFile
      val data = Util.readBinaryFile(file).right.get
      Util.writeBinaryFile(newFile, data)
    }
    val inputFileList = Util.listDirFiles(inputOrig.dir).filter(f => f.isFile)
    inputFileList.map(copyToSessionDir)
    logger.info("Copied input files from " + inputOrig.dir.getAbsolutePath + " --> " + sessionDir.getAbsolutePath)

    val dicomFileList = Util.listDirFiles(sessionDir).map(f => new DicomFile(f)).filter(df => df.attributeList.nonEmpty)
    val epidList = dicomFileList.filter(df => df.isModality(SOPClass.CTImageStorage))
    val regList = dicomFileList.filter(df => df.isModality(SOPClass.SpatialRegistrationStorage))

    val acquisitionDate = inputOrig.dataDate match {
      case None => None
      case Some(timestamp) => Some(timestamp.getTime)
    }

    val bbByEPID = BBbyEPID.getByOutput(outputOrig.outputPK.get).head

    val rtplan = {
      val series = DicomSeries.getBySopInstanceUID(bbByEPID.rtplanSOPInstanceUID).head
      series.attributeListList.head
    }

    val reg = {
      def getFrameOfRef(al: AttributeList): String = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString

      val epidFrmRef = getFrameOfRef(epidList.head.attributeList.get)
      val planFrmRef = getFrameOfRef(rtplan)
      def regWorks(r: AttributeList): Boolean = {
        val ir = new ImageRegistration(r)
        ir.frameOfRefUID.equals(planFrmRef) && ir.otherFrameOfRefUID.equals(epidFrmRef)
      }
      regList.find(r => regWorks(r.attributeList.get))
    }

    val runReq = new BBbyEPIDRunReq(Right(rtplan), reg, epidList, Machine.get(outputOrig.machinePK.get).get)

    val procedure = Procedure.get(outputOrig.procedurePK).get

    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request), inputOrig.patientId, acquisitionDate)
    val input = inputOutput._1
    val output = inputOutput._2

    Future {
      val extendedData = ExtendedData.get(output)
      insertIfNew(Seq(runReq.rtplan), extendedData)
      val runReqFinal = runReq.reDir(input.dir)

      val rtplan = runReqFinal.rtplan
      val machine = runReqFinal.machine

      val finalStatus = BBbyEPIDExecute.runProcedure(extendedData, runReqFinal)
      val finDate = new Timestamp(System.currentTimeMillis)
      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

      Phase2Util.setMachineSerialNumber(machine, runReq.epid.head)
      outputFinal.insertOrUpdate
      outputFinal.updateData(outputFinal.makeZipOfFiles)
      Run.removeRedundantOutput(outputFinal.outputPK)
      // remove the original input and all associated outputs to clean up any possible redundant data
      Input.delete(inputOrig.inputPK.get)
    }

    val suffix = "?" + ViewOutput.outputPKTag + "=" + output.outputPK.get
    response.redirectSeeOther(ViewOutput.path + suffix)
  }

  /**
   * Tell the user that the redo is forbidden and why.  Also give them a redirect back to the list of results.
   */
  private def forbidRedo(response: Response, msg: String, outputPK: Option[Long]) {
    val content = {
      <div class="row">
        <div class="col-md-4 col-md-offset-2">
          { msg }
          <p></p>
          <a href={ OutputList.path } class="btn btn-default" role="button">Back</a>
        </div>
      </div>
    }

    logger.info(msg + "  ouputPK: " + outputPK)
    val text = wrapBody(content, "Redo not permitted")
    setResponse(text, response, Status.CLIENT_ERROR_FORBIDDEN)
  }

  /**
   * Given an output, redo the analysis.
   */
  def redo(outputPK: Long, request: Request, response: Response) = {
    try {
      Output.get(outputPK) match {
        case None => {
          logger.info("Redo of output " + outputPK + " not possible because output does not exist")
          val msg = "Redo not possible because output does not exist"
          forbidRedo(response, msg, None)
        }
        case Some(outputOrig) => {
          Input.get(outputOrig.inputPK) match {
            case None => {
              val msg = "Redo not possible because input does not exist"
              forbidRedo(response, msg, outputOrig.outputPK)
            }
            case Some(inputOrig) if (!userAuthorizedToModify(request, response, inputOrig)) => {
              val msg = "Redo not permitted because user is from a different institution."
              forbidRedo(response, msg, outputOrig.outputPK)
            }
            case Some(inputOrig) => BBbyEPIDRun.processRedoRequest(request, response, inputOrig, outputOrig)
          }
        }
      }
    } catch {
      case t: Throwable => logger.warn("Unable to redo output " + outputPK + " : " + fmtEx(t))
    }
  }

}

/**
 * Run BBbyEPID code.
 */
class BBbyEPIDRun(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  /** Defines precision - Format to use when showing numbers. */
  private val outputFormat = "%7.5e"

  private def getFrameOfRef(al: AttributeList): String = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  private def getFrameOfRef(dicomFile: DicomFile): String = getFrameOfRef(dicomFile.attributeList.get)

  private def getSeries(al: AttributeList): String = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
  private def getSeries(dicomFile: DicomFile): String = getSeries(dicomFile.attributeList.get)

  //private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)
  private val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("BBbyEPID"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, Machine] = {
    val machineRelatedList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage) || df.isModality(SOPClass.CTImageStorage) || df.isModality(SOPClass.SpatialRegistrationStorage))
    // machines that DICOM files reference (based on device serial numbers)
    val referencedMachines = machineRelatedList.map(df => Machine.attributeListToMachine(df.attributeList.get)).flatten.distinct
    val chosenMachine = for (pkTxt <- valueMap.get(machineSelector.label); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m

    val result: Either[StyleMapT, Machine] = 0 match {
      case _ if (referencedMachines.size == 1) => Right(referencedMachines.head)
      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
      case _ if (referencedMachines.size > 1) => formErr("Files come from more than one machine; please go back and try again.  Machines: " + referencedMachines.map(m => m.id).mkString("  "))
      case _ => formErr("Unknown machine.  Please choose from the 'Machine' list below or click Cancel and then use the Administration interface to add it.")
    }
    result
  }

  /**
   * Save an attribute list in the tmp directory.
   */
  private def saveDbRtplan(al: AttributeList): DicomFile = {
    val fileName = Util.sopOfAl(al) + ".dcm"
    val tmpFile = new File(Config.tmpDirFile, fileName)
    DicomUtil.writeAttributeList(al, tmpFile)
    new DicomFile(tmpFile)
  }

  /**
   * Search the uploaded plans and reg files to see if there is a set with compatible frames of reference
   */
  private def searchUploadedPlans(rtplanList: Seq[DicomFile], regList: Seq[DicomFile]): Option[(DicomFile, Option[DicomFile])] = {
    def toIR(df: DicomFile) = new ImageRegistration(df.attributeList.get)
    val compatUploaded = for (rtplan <- rtplanList; reg <- regList; if (toIR(reg).sameFrameOfRef(rtplan.attributeList.get))) yield { (rtplan, Some(reg)) }
    if (compatUploaded.isEmpty) None else compatUploaded.headOption
  }

  /**
   * Search uploaded plans that have the same frame of reference as the EPID, in which case a REG file is not needed.
   */
  private def searchUploadedPlansWithoutReg(rtplanList: Seq[DicomFile], epidFrameOfRef: String): Option[(DicomFile, Option[DicomFile])] = {
    val compatPlan = rtplanList.filter(rtplan => getFrameOfRef(rtplan).equals(epidFrameOfRef))
    if (compatPlan.isEmpty) None else Some(compatPlan.head, None)
  }

  /**
   * Search database plans that work with one of the uploaded REG files.
   */
  private def searchDbPlanWithReg(regList: Seq[DicomFile]): Option[(DicomFile, Option[DicomFile])] = {
    val regFrUidSet = regList.map(reg => getFrameOfRef(reg)).toSet
    val rtPlanListFromDb = DicomSeries.getByFrameUIDAndSOPClass(regFrUidSet, SOPClass.RTPlanStorage).map(dicomSeries => dicomSeries.attributeListList.head)
    val compatUploaded = for (rtplan <- rtPlanListFromDb; reg <- regList; if (new ImageRegistration(reg).sameFrameOfRef(rtplan))) yield { (rtplan, Some(reg)) }

    val compat = if (compatUploaded.isEmpty)
      None
    else {
      val rtplanDicomFile = saveDbRtplan(compatUploaded.head._1)
      Some(rtplanDicomFile, compatUploaded.head._2)
    }
    compat
  }

  /**
   * Search database plans that have the same frame of reference as the EPID, so no REG file required.
   */
  private def searchDbPlanWithoutReg(epidFrameOfRef: String): Option[(DicomFile, Option[DicomFile])] = {
    val rtPlanListFromDb = DicomSeries.getByFrameUIDAndSOPClass(Set(epidFrameOfRef), SOPClass.RTPlanStorage).map(dicomSeries => dicomSeries.attributeListList.head)

    val compat = if (rtPlanListFromDb.isEmpty)
      None
    else {
      val rtplanDicomFile = saveDbRtplan(rtPlanListFromDb.head)
      Some(rtplanDicomFile, None)
    }
    compat
  }

  /**
   * Get the SOP of the plan referenced by the given epid.
   */
  private def getPlanRef(epid: DicomFile): String = {
    val seq = DicomUtil.seqToAttr(epid.attributeList.get, TagFromName.ReferencedRTPlanSequence)
    seq.head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
  }

  private def getGantryAngle(epid: DicomFile): Double = {
    epid.attributeList.get.get(TagFromName.GantryAngle).getDoubleValues.head
  }

  /**
   * Only allow angles that are within 5 degrees of right angles.
   */
  private def isValidAngle(angle: Double) = {
    val rounded = Util.angleRoundedTo90(angle)
    val canonicalAngle = ((angle.round.toInt + 360) % 360)
    (rounded - canonicalAngle).abs < 5
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validate(valueMap: ValueMapT): Either[StyleMapT, BBbyEPIDRunReq] = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val epidList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    val rtplanList = dicomFileList.filter(df => df.isModality(SOPClass.RTPlanStorage))
    val angleList = epidList.map(epid => getGantryAngle(epid))
    // true if all angles are valid
    val anglesValid = angleList.map(angle => isValidAngle(angle)).reduce(_ && _)

    def epidSeriesList = epidList.map(epid => getSeries(epid)).distinct
    def referencedUploadedPlanList = epidList.map(epid => getPlanRef(epid)).distinct

    def machineCheck = validateMachineSelection(valueMap, epidList)

    logger.info("Number of files uploaded:  RTPLAN: " + rtplanList.size + "    EPID: " + epidList.size)

    def numSeries(dfList: Seq[DicomFile]): Int = dfList.map(df => df.attributeList.get.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    /** return the SOP of the plan (either uploaded or from DB) that is referenced by the EPID, or none if it can not be found. */
    def rtplanRefencedByEpid: Option[String] = {
      rtplanList.find(plan => Util.sopOfAl(plan.attributeList.get).equals(referencedUploadedPlanList.head)) match {
        case Some(rtplanDf) => Some(Util.sopOfAl(rtplanDf.attributeList.get))
        case _ => {
          // check in database
          DicomSeries.getBySopInstanceUID(referencedUploadedPlanList.head) match {
            case (ds) => Some(ds.head.sopInstanceUIDList)
            case _ => None
          }
        }
      }
    }

    val result = 0 match {
      case _ if epidList.isEmpty => formErr("No EPID files uploaded")
      case _ if !anglesValid => ("EPID angles must be either vertical or horizontal (0, 90, 180, 270).  Found: " + angleList.mkString("  "))
      case _ if epidSeriesList.size > 1 => formErr("EPID slices are from " + numSeries(epidList) + " different series.")
      case _ if referencedUploadedPlanList.size > 1 => formErr("EPID series references more than one RTPLAN.")
      case _ if rtplanRefencedByEpid.isEmpty => formErr("Can not find the RTPLAN referenced by the EPID files.")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ => {

        val plan: Either[DicomFile, AttributeList] = {
          val planSop = referencedUploadedPlanList.head
          rtplanList.find(plan => Util.sopOfAl(plan.attributeList.get).equals(planSop)) match {
            case Some(planDicomFile) => Left(planDicomFile)
            case _ => {
              Right(DicomSeries.getBySopInstanceUID(planSop).head.attributeListList.head)
            }
          }

        }

        val runReq = new BBbyEPIDRunReq(plan, epidList, machineCheck.right.get)
        Right(runReq)
      }
    }
    result
  }

  /**
   * Given an image list, find the one with the earliest date/time.
   */
  private def dateTimePatId(rtimageList: Seq[AttributeList]): (Option[Long], Option[String]) = {
    val list = rtimageList.map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al)).filter(dt => dt._1.nonEmpty && dt._2.isDefined)
    val date: Option[Long] = {
      val dateList = list.map(dp => dp._1.headOption).flatten
      if (dateList.isEmpty) None else Some(dateList.map(d => d.getTime).min)
    }
    val patient: Option[String] = list.map(dp => dp._2).flatten.headOption
    (date, patient)
  }

  /**
   * Respond to the 'Run' button.
   */
  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response) = {

    logger.info("Validating data")
    validate(valueMap) match {
      case Left(errMap) => {
        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        logger.info("Data is valid.  Preparing to analyze data.")
        // only consider the EPID files for the date-time stamp.  The plan could have been from months ago.
        val dtp = dateTimePatId(runReq.epidList.map(df => df.attributeList.get))

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(request), dtp._2, dtp._1)
        val input = inputOutput._1
        val output = inputOutput._2

        Future {
          val extendedData = ExtendedData.get(output)
          BBbyEPIDRun.insertIfNew(Seq(runReq.rtplan), extendedData)
          val runReqFinal = runReq.reDir(input.dir)

          //          val plan = runReqFinal.rtplan
          //          val machine = machineCheck.right.get
          //          Phase2Util.saveRtplanAsDicomSeries(runReq.rtplan)

          //          val rtimageMap = Phase2.constructRtimageMap(plan, rtimageList)

          val finalStatus = BBbyEPIDExecute.runProcedure(extendedData, runReqFinal)
          val finDate = new Timestamp(System.currentTimeMillis)
          val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

          Phase2Util.setMachineSerialNumber(extendedData.machine, runReq.epid.head)
          outputFinal.insertOrUpdate
          outputFinal.updateData(outputFinal.makeZipOfFiles)
          Run.removeRedundantOutput(outputFinal.outputPK)
        }

        val suffix = "?" + ViewOutput.outputPKTag + "=" + output.outputPK.get
        response.redirectSeeOther(ViewOutput.path + suffix)
      }
    }
  }

  /**
   * Cancel the procedure.  Remove files and redirect to procedure list.
   */
  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => Utility.deleteFileTree(dir)
      case _ => ;
    }
    WebRunIndex.redirect(response)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    val redo = valueMap.get(OutputList.redoTag)
    val del = valueMap.get(OutputList.deleteTag)

    try {
      0 match {
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
        case _ if buttonIs(valueMap, cancelButton) => cancel(valueMap, response)
        case _ if buttonIs(valueMap, runButton) => runIfDataValid(valueMap, request, response)
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
