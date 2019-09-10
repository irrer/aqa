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
   * For classifying angles.
   */
  object AngleType extends Enumeration {
    val horizontal = Value
    val vertical = Value
  }

  /**
   * Only allow angles that are within 5 degrees of right angles.
   */
  def classifyAngle(angle: Double): Option[AngleType.Value] = {
    val rounded = Util.angleRoundedTo90(angle)
    val canonicalAngle = ((angle.round.toInt + 3600) % 360)
    val angTyp = (((rounded - canonicalAngle).abs < 5), canonicalAngle) match {
      case (true, 0) => Some(AngleType.vertical)
      case (true, 180) => Some(AngleType.vertical)
      case (true, 90) => Some(AngleType.horizontal)
      case (true, 270) => Some(AngleType.horizontal)
      case _ => None
    }
    angTyp
  }

  /**
   * Return true if the attribute list's gantry angle is of the specified type.
   */
  def isAngleType(al: AttributeList, angleType: AngleType.Value): Boolean = {
    classifyAngle(Util.gantryAngle(al)) match {
      case Some(at) => at.toString.equals(angleType.toString())
      case _ => false
    }
  }

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
    val epidList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))

    val acquisitionDate = inputOrig.dataDate match {
      case None => None
      case Some(timestamp) => Some(timestamp.getTime)
    }

    val bbByEPID = BBbyEPID.getByOutput(outputOrig.outputPK.get).head

    val runReq = new BBbyEPIDRunReq(epidList, Machine.get(outputOrig.machinePK.get).get)

    val procedure = Procedure.get(outputOrig.procedurePK).get

    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request), inputOrig.patientId, acquisitionDate)
    val input = inputOutput._1
    val output = inputOutput._2

    Future {
      val extendedData = ExtendedData.get(output)
      val runReqFinal = runReq.reDir(input.dir)

      val machine = runReqFinal.machine

      val finalStatus = BBbyEPIDAnalyse.runProcedure(extendedData, runReqFinal)
      val finDate = new Timestamp(System.currentTimeMillis)
      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

      Phase2Util.setMachineSerialNumber(machine, runReq.epidList.head)
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
    val machineRelatedList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
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
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validate(valueMap: ValueMapT): Either[StyleMapT, BBbyEPIDRunReq] = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val epidListDf = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    val epidList = epidListDf.map(df => df.attributeList).flatten
    val angleList = epidList.map(epid => Util.gantryAngle(epid))
    def angleTextList = angleList.map(a => Util.fmtDbl(a)).mkString("  ")
    // true if all angles are valid
    val anglesTypeList = angleList.map(angle => BBbyEPIDRun.classifyAngle(angle)).flatten

    def epidSeriesList = epidList.map(epid => getSeries(epid)).distinct

    def machineCheck = validateMachineSelection(valueMap, epidListDf)

    logger.info("Number of RTIMAGE files uploaded: " + epidList.size)

    val numSeries = epidList.map(epid => epid.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString).distinct.sorted.size

    val result: Either[WebUtil.StyleMapT, BBbyEPIDRunReq] = 0 match {
      case _ if epidList.isEmpty => formErr("No EPID files uploaded")
      // TODO: Should an incomplete set of angles be accepted to support the odd test?
      //      case _ if !anglesTypeList.contains(BBbyEPIDRun.AngleType.horizontal) => formErr("No EPID image with horizontal gantry angle (0 or 180) present.  Angles uploaded: " + angleTextList)
      //      case _ if !anglesTypeList.contains(BBbyEPIDRun.AngleType.vertical) => formErr("No EPID image with vertical gantry angle (90 or 270) present.  Angles uploaded: " + angleTextList)
      case _ if epidSeriesList.size > 1 => formErr("EPID slices are from " + numSeries + " different series.")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ => {
        val runReq = new BBbyEPIDRunReq(epidListDf, machineCheck.right.get)
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
        val dtp = dateTimePatId(runReq.epidList)

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(request), dtp._2, dtp._1)
        val input = inputOutput._1
        val output = inputOutput._2

        Future {
          val extendedData = ExtendedData.get(output)
          val runReqFinal = runReq.reDir(input.dir)
          BBbyEPIDRun.insertIfNew(runReqFinal.epidList, extendedData)
          val finalStatus = BBbyEPIDAnalyse.runProcedure(extendedData, runReqFinal)
          val finDate = new Timestamp(System.currentTimeMillis)
          val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

          Phase2Util.setMachineSerialNumber(extendedData.machine, runReq.epidList.head)
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
