package org.aqa.webrun.phase2

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
import org.aqa.webrun.phase2.symmetryAndFlatness.SymmetryAndFlatnessAnalysis
import org.aqa.webrun.phase2.wedge.WedgeAnalysis
import org.aqa.webrun.phase2.collimatorPosition.CollimatorPositionAnalysis
import org.aqa.webrun.phase2.collimatorCentering.CollimatorCenteringAnalysis
import org.aqa.webrun.phase2.centerDose.CenterDoseAnalysis
import org.aqa.webrun.phase2.metadataCheck.MetadataCheckAnalysis
import org.aqa.webrun.phase2.metadataCheck.MetadataCheckValidation
import org.aqa.webrun.phase2.leafPosition.LeafPositionAnalysis
import org.aqa.web.Session
import org.aqa.db.CachedUser
import org.aqa.web.OutputList
import org.aqa.webrun.ExtendedData
import org.aqa.db.DicomSeries
import org.aqa.webrun.phase2.vmat.VMATAnalysis

object Phase2 extends Logging {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"

  /**
   * Create a list of the RTIMAGEs mapped by beam name.
   */
  private def constructRtimageMap(plan: DicomFile, rtimageList: Seq[DicomFile]) = {
    rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(plan, rtimage), rtimage)).filter(ni => ni._1.isDefined).map(ni => (ni._1.get, ni._2)).toMap
  }

  private def makeHtml(extendedData: ExtendedData, procedureStatus: ProcedureStatus.Value, elemList: Seq[Elem], runReq: RunReq) = {

    val sizedGroups = edu.umro.ScalaUtil.Util.sizedGroups(elemList, 4)

    def showGroup(group: Seq[Elem]) = {
      <div class="row" style="margin-top:40px; margin-left:10px; margin-right:10px;">
        { group.map(e => <div class="col-md-3">{ e }</div>) }
      </div>
    }

    def table = {
      <div class="col-md-10 col-md-offset-1">
        { sizedGroups.map(g => showGroup(g)) }
      </div>
    }

    val text = Phase2Util.wrapSubProcedure(extendedData, table, "Phase 2", procedureStatus, None, runReq)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
  }

  /**
   * Run the sub-procedures.
   */
  private def runPhase2(extendedData: ExtendedData, rtimageMap: Map[String, DicomFile], runReq: RunReq): ProcedureStatus.Value = {
    logger.info("Starting Phase2 analysis")

    val alList = (runReq.rtimageMap.map(df => df._2.attributeList)).flatten.toSeq

    // Save DICOM series to the database if they are not already there.  It is possible that the RTIMAGE files are
    // from different series, so make sure to split them by series and store them separately.  This is important for
    // the automatic client that will ask this server if a given series has been processed or not.  This actually
    // happened once.
    val seriesList = alList.groupBy(al => Util.serInstOfAl(al)).map(series => series._2)
    seriesList.map(series => DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, series))
    if (runReq.rtplan.attributeList.isDefined)
      DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, Seq(runReq.rtplan.attributeList.get))

    val summaryList: Either[Seq[Elem], Seq[Elem]] = MetadataCheckAnalysis.runProcedure(extendedData, runReq) match {
      case Left(fail) => Left(Seq(fail))
      case Right(metadataCheck) => {
        BadPixelAnalysis.runProcedure(extendedData, runReq) match {
          case Left(fail) => Left(Seq(metadataCheck.summary, fail))
          case Right(badPixel) => {
            CollimatorCenteringAnalysis.runProcedure(extendedData, runReq) match {
              case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, fail))
              case Right(collimatorCentering) => {
                CenterDoseAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result) match {
                  case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, collimatorCentering.summary, fail))
                  case Right(centerDose) => {
                    val prevSummaryList = Seq(metadataCheck, badPixel, collimatorCentering, centerDose).map(r => r.summary)
                    val vmatFunction: Seq[() => Either[Elem, SubProcedureResult]] = { // TODO this should be incorporated into the <code>seq</code> list when VMAT is approved.
                      if (Config.VMATPenumbraBorderThickness_mm == -1) {
                        Seq[() => Either[Elem, SubProcedureResult]]()
                      } else {
                        Seq(() => VMATAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result))
                      }
                    }
                    val seq: Seq[() => Either[Elem, SubProcedureResult]] = Seq(
                      () => CollimatorPositionAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result),
                      () => WedgeAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result, centerDose.resultList),
                      () => SymmetryAndFlatnessAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result),
                      () => LeafPositionAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result)) ++ vmatFunction

                    val list = seq.par.map(f => f()).toSeq
                    val summaryList = prevSummaryList ++ list.map(r => if (r.isLeft) r.left.get else r.right.get.summary)
                    val pass = (list.find(r => r.isLeft).isEmpty) && list.filter(s => !Phase2Util.statusOk(s.right.get.status)).isEmpty
                    if (pass) Right(summaryList) else Left(summaryList)
                  }
                }
              }
            }
          }
        }
      }
    }
    logger.info("Finished all Phase2 analysis")

    val status = summaryList match {
      case Left(_) => ProcedureStatus.fail
      case Right(_) => ProcedureStatus.pass
    }

    val sumList = summaryList match {
      case Left(list) => list
      case Right(list) => list
    }

    logger.info("Generating Phase2 HTML")
    makeHtml(extendedData, status, sumList, runReq)
    logger.info("Done generating Phase2 HTML")

    status
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

  private def processRedoRequest(request: Request, response: Response, inputOrig: Input, outputOrig: Output, await: Boolean, isAuto: Boolean) = {

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

    val dicomFileList = Util.listDirFiles(sessionDir).map(f => new DicomFile(f)).filter(df => df.attributeList.nonEmpty)
    val rtimageList = dicomFileList.filter(df => df.isRtimage)

    logger.info("Copied input files from " + inputOrig.dir.getAbsolutePath + " --> " + sessionDir.getAbsolutePath)

    val acquisitionDate = inputOrig.dataDate match {
      case None => None
      case Some(timestamp) => Some(timestamp.getTime)
    }

    val runReq = {
      val rtplanFile = new File(Config.sharedDir, Phase2Util.referencedPlanUID(rtimageList.head) + Util.dicomFileNameSuffix)
      val rtplan = new DicomFile(rtplanFile)
      val machine = {
        if (outputOrig.machinePK.isDefined && Machine.get(outputOrig.machinePK.get).isDefined)
          Machine.get(outputOrig.machinePK.get).get
        else {
          val serNo = rtimageList.map(df => df.attributeList).flatten.head.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrEmptyString
          Machine.findMachinesBySerialNumber(serNo).head
        }
      }
      val rtimageMap = constructRtimageMap(rtplan, rtimageList)
      val flood = rtimageMap(Config.FloodFieldBeamName)

      new RunReq(rtplan, None, machine, rtimageMap, flood) // TODO handle rtplanCBCT
    }

    val procedure = Procedure.get(outputOrig.procedurePK).get

    val inputOutput = Run.preRun(procedure, runReq.machine, sessionDir, getUser(request), inputOrig.patientId, acquisitionDate)
    val input = inputOutput._1
    val output = inputOutput._2

    val future = Future {
      val extendedData = ExtendedData.get(output)
      val runReqFinal = runReq.reDir(input.dir)

      val rtplan = runReqFinal.rtplan
      val machine = runReqFinal.machine
      Phase2Util.saveRtplan(rtplan)

      val rtimageMap = constructRtimageMap(rtplan, rtimageList)

      val finalStatus = Phase2.runPhase2(extendedData, rtimageMap, runReqFinal)
      val finDate = new Timestamp(System.currentTimeMillis)
      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

      Phase2Util.setMachineSerialNumber(machine, runReqFinal.flood.attributeList.get)
      outputFinal.insertOrUpdate
      outputFinal.updateData(outputFinal.makeZipOfFiles)
      Run.removeRedundantOutput(outputFinal.outputPK)
      // remove the original input and all associated outputs to clean up any possible redundant data
      Input.delete(inputOrig.inputPK.get)
    }

    awaitIfRequested(future, await, inputOutput._2.procedurePK)
    ViewOutput.redirectToViewRunProgress(response, isAuto, output.outputPK.get)
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
   * Given a Phase 2 output, redo the analysis.
   */
  def redo(outputPK: Long, request: Request, response: Response, await: Boolean, isAuto: Boolean) = {
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
            case Some(inputOrig) => processRedoRequest(request, response, inputOrig, outputOrig, await, isAuto)
          }
        }
      }
    } catch {
      case t: Throwable => logger.warn("Unable to redo output " + outputPK + " : " + fmtEx(t))
    }
  }

}

/**
 * Run Phase2 code.
 */
class Phase2(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  /** Defines precision - Format to use when showing numbers. */
  private val outputFormat = "%7.5e"

  //private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)
  private val machineSelector = new WebInputSelectMachine("Machine", 6, 0)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("Phase2"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, Machine] = {
    val rtimageList = dicomFileList.filter(df => df.isRtimage)
    // machines that DICOM files reference (based on device serial numbers)
    val referencedMachines = rtimageList.map(df => Machine.attributeListToMachine(df.attributeList.get)).flatten.distinct
    val chosenMachine = for (pkTxt <- valueMap.get(machineSelector.label); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m

    val result: Either[StyleMapT, Machine] = 0 match {
      case _ if (referencedMachines.size == 1) => Right(referencedMachines.head)
      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
      case _ if (referencedMachines.size > 1) => formErr("Files come from more than one machine; please go back and try again.  Machines: " + referencedMachines.map(m => m.id).mkString("  "))
      case _ => formErr("Unknown machine.  Please choose from the 'Machine' list below or click Cancel and then use the Administration interface to add it.")
    }
    result
  }

  private case class BasicData(rtplan: DicomFile, machine: Machine, rtimageListByBeam: Seq[(Option[String], DicomFile)]) {

    private def dfToString(dicomFile: DicomFile) = {
      val sop = { if (dicomFile.attributeList.isDefined) Util.sopOfAl(dicomFile.attributeList.get) else "NoAttributelist" }
      val modality = { if (dicomFile.attributeList.isDefined) Util.modalityOfAl(dicomFile.attributeList.get) else "" }
      sop + "    " + modality + "    " + dicomFile.file.getName
    }

    private def beamOf(beamName: Option[String]) = if (beamName.isDefined) beamName.get else "NoBeamName"

    override def toString =
      "machine: " + dfToString(rtplan) + "\n" +
        "machine id: " + machine.id + "    serial number: " + machine.serialNumber + "    machinePK: " + machine.machinePK + "\n" +
        "rtimageListByBeam:\n    " + rtimageListByBeam.map(r => beamOf(r._1) + "  " + dfToString(r._2)).mkString("\n    ")
  }

  /**
   * Check that there is a single plan, single machine, and some images.
   */
  private def basicValidation(valueMap: ValueMapT, rtplanList: Seq[DicomFile], rtimageList: Seq[DicomFile]): Either[StyleMapT, BasicData] = {
    logger.info("Number of RTPLAN files uploaded: " + rtplanList.size)
    logger.info("Number of RTIMAGE files duploaded: " + rtimageList.size)
    val machineSerialNumberList = rtimageList.map(rtimage => Util.getAttrValue(rtimage.attributeList.get, TagFromName.DeviceSerialNumber)).flatten

    def rtimageDate(rtimage: DicomFile): Long = {
      Util.extractDateTimeAndPatientIdFromDicomAl(rtimage.attributeList.get)._1.map(d => d.getTime).distinct.sorted.last
    }

    val dateTimeList = rtimageList.map(rtimage => rtimageDate(rtimage)).sorted
    val maxDuration = Math.round(Config.MaxProcedureDuration * 60 * 1000).toLong

    val machineCheck = validateMachineSelection(valueMap, rtimageList)

    // associate each image with a plan
    val planGroups = rtplanList.map(plan => (plan, rtimageList.filter(img => Phase2Util.imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    /**
     * Make a human readable list of machines
     */
    def machineList: String = {
      val dstnct = machineSerialNumberList.distinct
      val idList = dstnct.map(serNo => Machine.findMachinesBySerialNumber(serNo)).flatten.distinct.map(mach => mach.id).mkString("  ")
      dstnct.mkString("Serial Numbers: " + dstnct.mkString("  ")) + "    Machines: " + idList
    }

    0 match {
      case _ if (rtplanList.isEmpty) => formErr("No RTPLANS found.  Try uploading the RTPLAN with the images to get it into the system.")
      case _ if (rtimageList.isEmpty) => formErr("No RTIMAGEs given")
      case _ if (planGroups.isEmpty) => formErr("No RTPLAN found for RTIMAGEs.  Try uploading the RTPLAN with the RTIMAGE files.")
      case _ if (planGroups.size > 1) => formErr("The RTIMAGEs reference multiple plans.  Only one plan per run is permitted.")
      case _ if (planGroups.head._2.size < rtimageList.size) => {
        formErr("There are " + rtimageList.size + " images but only " + planGroups.head._2.size + " reference this plan")
      }

      case _ if (machineSerialNumberList.isEmpty) => {
        formErr("None of the " + rtimageList.size +
          " RTIMAGEs have a device serial number (0018,1000) tag.\\n" +
          "This can happen on a new machine or one that has been recently serviced.\\n" +
          "The device serial number is required by this software to identify the instance of the machine.")
      }

      case _ if (machineSerialNumberList.size != rtimageList.size) => {
        formErr("Only " + machineSerialNumberList.size + " of the " + rtimageList.size +
          " RTIMAGEs have a device serial number (0018,1000) tag.\\n" +
          "This can happen on a new machine or one that has been recently serviced.\\n" +
          "The device serial number is required by this software to identify the instance of the machine.")
      }
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ if (machineSerialNumberList.distinct.size != 1) => formErr("There are RTIMAGEs from more than one machine: " + machineList + "  Only one machine's data can be analyzed.")

      case _ if ((dateTimeList.last - dateTimeList.head) > maxDuration) =>
        formErr("Over " + Config.MaxProcedureDuration + " minutes from first to last image.  These RTIMAGE files were not from the same session")
      case _ => {
        val rtplan = planGroups.head._1
        val rtimageByBeam = rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(rtplan, rtimage), rtimage))
        Right(new BasicData(rtplan, machineCheck.right.get, rtimageByBeam))
      }
    }
  }

  /**
   *  determine if there are undefined beams
   */
  private def beamNotDefinedProblem(basicData: BasicData): Option[String] = {
    // check for beams that are not in the plan
    val undefFileNameList = basicData.rtimageListByBeam.filter(b => b._1.isEmpty).map(b => b._2.file.getName)
    if (undefFileNameList.nonEmpty) {
      Some("The image set is probably referencing the wrong plan." + WebUtil.titleNewline + "There were " + undefFileNameList.size + " files that references a beam  that was not in the plan:" + WebUtil.titleNewline + undefFileNameList.mkString(WebUtil.titleNewline))
    } else None
  }

  /**
   *  determine if there is more than one image that reference/define the same beam
   */
  private def beamMultiRefProblem(basicData: BasicData): Option[String] = {
    val multiRefList = basicData.rtimageListByBeam.filter(b => b._1.isDefined).groupBy(b => b._1.get.toUpperCase).map(g => g._2).filter(g => g.size > 1)
    if (multiRefList.isEmpty) None
    else {
      val sep = "\\n    "
      val text = "Same beam is referenced by multiple files:" + sep + multiRefList.map(mr => mr.map(r => r._1.get.trim + "-->" + r._2.file.getName.trim).mkString(sep)).mkString(sep)
      Some(text)
    }
  }

  /**
   * Check beam definitions, existence of flood field, and organize inputs into <code>RunReq</code> to facilitate further processing.
   */
  def basicBeamValidation(valueMap: ValueMapT, basicData: BasicData): Either[StyleMapT, RunReq] = {
    val rtimageMap = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && (!rl._1.get.equals(Config.FloodFieldBeamName))).map(rl => (rl._1.get, rl._2)).toMap
    val flood = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && (rl._1.get.equals(Config.FloodFieldBeamName)))

    (beamNotDefinedProblem(basicData), beamMultiRefProblem(basicData)) match {
      case (Some(errorMessage), _) => formErr(errorMessage)
      case (_, Some(errorMessage)) => formErr(errorMessage)
      case _ if (flood.isEmpty) => formErr("Flood field beam is missing")
      case _ => Right(new RunReq(basicData.rtplan, None, basicData.machine, rtimageMap, flood.head._2)) // success // TODO handle rtplanCBCT
    }
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validate(valueMap: ValueMapT, rtplanList: Seq[DicomFile], rtimageList: Seq[DicomFile]): Either[StyleMapT, RunReq] = {
    basicValidation(valueMap, rtplanList: Seq[DicomFile], rtimageList: Seq[DicomFile]) match {
      case Left(fail) => Left(fail)
      case Right(basicData) => {
        logger.info("Received file list: " + basicData)
        val basicBeamValid = basicBeamValidation(valueMap, basicData)
        if (basicBeamValid.isLeft) basicBeamValid
        else {
          val runReq = basicBeamValid.right.get
          val err = MetadataCheckValidation.validate(runReq)
          if (err.isDefined) Left(Error.make(form.uploadFileInput.get, err.get)) else {
            val bpErr = BadPixelAnalysis.validate(runReq)
            if (bpErr.isDefined) Left(Error.make(form.uploadFileInput.get, err.get)) else Right(runReq)
          }
        }
      }
    }
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
   * If more than one RTIMAGE reference the same beam, then remove all but the one with the the latest date/time.
   */
  private def cullRedundantBeamReferences(rtimageList: Seq[DicomFile]): Seq[DicomFile] = {
    val groupList = rtimageList.groupBy(df => df.attributeList.get.get(TagFromName.ReferencedBeamNumber).getIntegerValues.head).values

    def latestDateTime(al: AttributeList): Long = {
      Util.extractDateTimeAndPatientIdFromDicomAl(al)._1.map(dt => dt.getTime).max
    }

    def minGroup(g: Seq[DicomFile]): DicomFile = {
      if (g.size == 1) g.head
      else {
        val sorted = g.map(df => (latestDateTime(df.attributeList.get), df)).sortBy(_._1)
        val latestDicomFile = sorted.last._2
        val text = sorted.dropRight(1).map(df => Util.sopOfAl(df._2.attributeList.get)).mkString("\n    ")
        logger.info("Ignoring RTIMAGE files that redundantly reference the same beam.  Keeping the chronologically newest one:\n    " + text +
          "\nKeeping " + Util.sopOfAl(latestDicomFile.attributeList.get))
        latestDicomFile
      }
    }

    val culled = groupList.map(g => minGroup(g))
    logger.info("Number of RTIMAGE files culled due to redundant beam references: " + (rtimageList.size - culled.size))
    culled.toSeq
  }

  private def run(valueMap: ValueMapT, runReq: RunReq, response: Response, rtimageList: Seq[DicomFile]) = {

    // only consider the RTIMAGE files for the date-time stamp.  The plan could have been from months ago.
    val dtp = dateTimePatId(rtimageList)

    val sessDir = sessionDir(valueMap).get
    val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(response.getRequest), dtp.PatientID, dtp.dateTime)
    val input = inputOutput._1
    val output = inputOutput._2

    def perform = {
      val extendedData = ExtendedData.get(output)
      val runReqFinal = runReq.reDir(input.dir)

      val plan = runReqFinal.rtplan
      val machine = runReqFinal.machine
      // save serial number now in case analysis crashes with an exception
      Phase2Util.setMachineSerialNumber(machine, runReqFinal.flood.attributeList.get)
      Phase2Util.saveRtplan(plan)

      val rtimageMap = Phase2.constructRtimageMap(plan, rtimageList)

      val finalStatus = Phase2.runPhase2(extendedData, rtimageMap, runReqFinal)
      val finDate = new Timestamp(System.currentTimeMillis)
      val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

      outputFinal.insertOrUpdate
      outputFinal.updateData(outputFinal.makeZipOfFiles)
      Run.removeRedundantOutput(outputFinal.outputPK)
      Util.garbageCollect
      logger.info("Phase 2 analysis has completed.")
    }

    // if awaiting, then wait for completion, otherwise do it in the background
    if (isAwait(valueMap)) {
      perform
    } else {
      Future { perform }
    }
    logger.info("Redirecting web client to view run progress of EPID processing.")
    ViewOutput.redirectToViewRunProgress(response, valueMap, output.outputPK.get)

  }

  /**
   * Respond to the 'Run' button.
   */
  private def runIfDataValid(valueMap: ValueMapT, request: Request, response: Response) = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val rtplanList = Phase2Util.getPlanList(dicomFileList)
    val rtimageList = cullRedundantBeamReferences(dicomFileList.filter(df => df.isRtimage))

    validate(valueMap, rtplanList, rtimageList) match {
      case Left(errMap) => {
        logger.info("Phase2 Bad request: " + errMap.keys.map(k => k + " : " + valueMap.get(k)).mkString("\n    "))
        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        if (isAwait(valueMap)) awaitTag.synchronized {
          run(valueMap, runReq, response, rtimageList)
        }
        else run(valueMap, runReq, response, rtimageList)
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
