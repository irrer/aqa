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

/**
 * Provide the user interface and verify that the data provided is sufficient to do the analysis.
 */

object BBbyCBCTRun extends Logging {
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

  //  private def processRedoRequest(request: Request, response: Response, inputOrig: Input, outputOrig: Output) = {
  //
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
  //
  //    val dicomFileList = Util.listDirFiles(sessionDir).map(f => new DicomFile(f)).filter(df => df.attributeList.nonEmpty)
  //    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage) || df.isModality(SOPClass.CTImageStorage))
  //
  //    logger.info("Copied input files from " + inputOrig.dir.getAbsolutePath + " --> " + sessionDir.getAbsolutePath)
  //
  //    val acquisitionDate = inputOrig.dataDate match {
  //      case None => None
  //      case Some(timestamp) => Some(timestamp.getTime)
  //    }
  //
  //    val runReq = {
  //      val rtplanFile = new File(Config.sharedDir, Phase2Util.referencedPlanUID(rtimageList.head) + Util.dicomFileNameSuffix)
  //      val rtplan = new DicomFile(rtplanFile)
  //      val machine = {
  //        if (outputOrig.machinePK.isDefined && Machine.get(outputOrig.machinePK.get).isDefined)
  //          Machine.get(outputOrig.machinePK.get).get
  //        else {
  //          val serNo = rtimageList.map(df => df.attributeList).flatten.head.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrEmptyString
  //          Machine.findMachinesBySerialNumber(serNo).head
  //        }
  //      }
  //      val rtimageMap = constructRtimageMap(rtplan, rtimageList)
  //      val flood = rtimageMap(Config.FloodFieldBeamName)
  //
  //      //new BBbyCBCTRunReq(rtplan, None, machine, rtimageMap, flood) // TODO handle rtplanCBCT
  //      ???
  //    }
  //
  //    val procedure = Procedure.get(outputOrig.procedurePK).get
  //
  //    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request), inputOrig.patientId, acquisitionDate)
  //    val input = inputOutput._1
  //    val output = inputOutput._2
  //
  //    Future {
  //      val extendedData = ExtendedData.get(output)
  //      val runReqFinal = runReq.reDir(input.dir)
  //
  //      val rtplan = runReqFinal.rtplan
  //      val machine = runReqFinal.machine
  //      Phase2Util.saveRtplan(rtplan)
  //
  //      val rtimageMap = constructRtimageMap(rtplan, rtimageList)
  //
  //      val finalStatus = Phase2.runPhase2(extendedData, rtimageMap, runReqFinal)
  //      val finDate = new Timestamp(System.currentTimeMillis)
  //      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
  //
  //      Phase2Util.setMachineSerialNumber(machine, runReqFinal.flood.attributeList.get)
  //      outputFinal.insertOrUpdate
  //      outputFinal.updateData(outputFinal.makeZipOfFiles)
  //      Run.removeRedundantOutput(outputFinal.outputPK)
  //      // remove the original input and all associated outputs to clean up any possible redundant data
  //      Input.delete(inputOrig.inputPK.get)
  //    }
  //
  //    val suffix = "?" + ViewOutput.outputPKTag + "=" + output.outputPK.get
  //    response.redirectSeeOther(ViewOutput.path + suffix)
  //  }

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
            //  case Some(inputOrig) => processRedoRequest(request, response, inputOrig, outputOrig) TODO
          }
        }
      }
    } catch {
      case t: Throwable => logger.warn("Unable to redo output " + outputPK + " : " + fmtEx(t))
    }
  }

}

/**
 * Run BBbyCBCT code.
 */
class BBbyCBCTRun(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  /** Defines precision - Format to use when showing numbers. */
  private val outputFormat = "%7.5e"

  private def getFrameOfRef(al: AttributeList): String = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  private def getFrameOfRef(dicomFile: DicomFile): String = getFrameOfRef(dicomFile.attributeList.get)

  //private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)
  private val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("BBbyCBCT"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, Machine] = {
    val imageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage) || df.isModality(SOPClass.CTImageStorage))
    // machines that DICOM files reference (based on device serial numbers)
    val referencedMachines = imageList.map(df => attributeListToMachine(df.attributeList.get)).flatten.distinct
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
   * Search uploaded plans that have the same frame of reference as the CBCT, in which case a REG file is not needed.
   */
  private def searchUploadedPlansWithoutReg(rtplanList: Seq[DicomFile], cbctFrameOfRef: String): Option[(DicomFile, Option[DicomFile])] = {
    val compatPlan = rtplanList.filter(rtplan => getFrameOfRef(rtplan).equals(cbctFrameOfRef))
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
   * Search database plans that have the same frame of reference as the CBCT, so no REG file required.
   */
  private def searchDbPlanWithoutReg(cbctFrameOfRef: String): Option[(DicomFile, Option[DicomFile])] = {
    val rtPlanListFromDb = DicomSeries.getByFrameUIDAndSOPClass(Set(cbctFrameOfRef), SOPClass.RTPlanStorage).map(dicomSeries => dicomSeries.attributeListList.head)

    val compat = if (rtPlanListFromDb.isEmpty)
      None
    else {
      val rtplanDicomFile = saveDbRtplan(rtPlanListFromDb.head)
      Some(rtplanDicomFile, None)
    }
    compat
  }

  /**
   * Find a compatible set of RTPLAN and REG files.  If the plan has the same frame of reference as the
   * CBCT, then no REG file is required.  If a compatible plan was uploaded, then use that, otherwise
   * try to get one from the database.
   *
   * @return (RTPLAN, REG)
   */
  private def findRtplanAndReg(rtplanList: Seq[DicomFile], regList: Seq[DicomFile], cbctList: Seq[DicomFile]): Option[(DicomFile, Option[DicomFile])] = {
    val cbctFrameOfRef = getFrameOfRef(cbctList.head)
    val uploadedPlans = searchUploadedPlans(rtplanList, regList)
    val uploadedPlansWithoutReg = searchUploadedPlansWithoutReg(rtplanList, cbctFrameOfRef)

    lazy val dbPlanWithReg = searchDbPlanWithReg(regList)
    lazy val dbPlanWithoutReg = searchDbPlanWithoutReg(cbctFrameOfRef)

    val pair = 0 match {
      case _ if uploadedPlans.isDefined => uploadedPlans
      case _ if uploadedPlansWithoutReg.isDefined => uploadedPlansWithoutReg
      case _ if dbPlanWithReg.isDefined => dbPlanWithReg
      case _ if dbPlanWithoutReg.isDefined => dbPlanWithoutReg
      case _ => None
    }

    pair
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validate(valueMap: ValueMapT, rtplanList: Seq[DicomFile], regList: Seq[DicomFile], cbctList: Seq[DicomFile]): Either[StyleMapT, BBbyCBCTRunReq] = {
    logger.info("Number of files uploaded:  RTPLAN: " + rtplanList.size + "    REG: " + regList.size + "    CBCT: " + cbctList.size)

    def numSeries(dfList: Seq[DicomFile]): Int = dfList.map(df => df.attributeList.get.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size
    val machineCheck = validateMachineSelection(valueMap, cbctList)

    val result = 0 match {
      case _ if cbctList.isEmpty => formErr("No CBCT files uploaded")
      //case _ if regList.isEmpty => formErr("No REG files uploaded")
      case _ if (numSeries(cbctList) > 1) => formErr("CBCT files should all be from the same series, but are from " + numSeries(cbctList) + " different series")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ => {
        val rtplanAndReg = findRtplanAndReg(rtplanList, regList, cbctList)

        if (rtplanAndReg.isEmpty)
          formErr("Can not find RTPLAN for given CBCT and REG files")
        else {
          val runReq = new BBbyCBCTRunReq(rtplanAndReg.get._1, rtplanAndReg.get._2, cbctList, machineCheck.right.get)
          Right(runReq)
        }
      }
    }
    result
  }

  /**
   * Given an image list, find the one with the earliest date/time.
   */
  private def dateTimePatId(rtimageList: Seq[DicomFile]): Util.DateTimeAndPatientId = {
    val list = rtimageList.map(df => Util.dateTimeAndPatientIdFromDicom(df.file)).filter(dt => dt.dateTime.isDefined)
    val dtap =
      if (list.isEmpty) new Util.DateTimeAndPatientId(None, None)
      else list.minBy(dt => dt.dateTime.get)
    logger.info("DateTime and PatientId: " + dtap)
    dtap
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

  /**
   * Respond to the 'Run' button.
   */
  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response) = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val cbctList = dicomFileList.filter(df => df.isModality(SOPClass.CTImageStorage)).sortBy(df => Util.slicePosition(df.attributeList.get))
    // CBCT frame of reference.  If there is more than one CBCT series, then the data will be rejected anyway
    val cbctFrameOfRef = getFrameOfRef(cbctList.head.attributeList.get)
    // Make a list of REG files that support the CBCT's frame of reference.  We don't care about the other REG files.
    val regList = dicomFileList.filter(df => df.isModality(SOPClass.SpatialRegistrationStorage) && new ImageRegistration(df.attributeList.get).otherFrameOfRefUID.equals(cbctFrameOfRef))
    val rtplanList = dicomFileList.filter(df => df.isModality(SOPClass.RTPlanStorage))
    val machineCheck = validateMachineSelection(valueMap, cbctList)

    validate(valueMap, rtplanList, regList, cbctList) match {
      case Left(errMap) => {
        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        logger.info("Validating data")
        // only consider the CBCT files for the date-time stamp.  The plan could have been from months ago.
        val dtp = dateTimePatId(cbctList)

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, machineCheck.right.get, sessDir, getUser(request), dtp.PatientID, dtp.dateTime)
        val input = inputOutput._1
        val output = inputOutput._2

        Future {
          val extendedData = ExtendedData.get(output)
          insertIfNew(Seq(runReq.rtplan), extendedData)
          val runReqFinal = runReq.reDir(input.dir)

          //          val plan = runReqFinal.rtplan
          //          val machine = machineCheck.right.get
          //          Phase2Util.saveRtplanAsDicomSeries(runReq.rtplan)

          //          val rtimageMap = Phase2.constructRtimageMap(plan, rtimageList)

          val finalStatus = BBbyCBCTExecute.runProcedure(extendedData, runReqFinal)
          val finDate = new Timestamp(System.currentTimeMillis)
          val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

          Phase2Util.setMachineSerialNumber(extendedData.machine, runReq.cbct.head)
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
