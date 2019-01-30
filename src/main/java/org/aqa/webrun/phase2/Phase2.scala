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

object Phase2 {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"
}

/**
 * Run Phase2 code.
 */
class Phase2(procedure: Procedure) extends WebRunProcedure(procedure) with Logging {

  /** Defines precision - Format to use when showing numbers. */
  private val outputFormat = "%7.5e"

  private val machineSelector = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("Phase2"), List(List(machineSelector), List(runButton, cancelButton)), 10)

  private def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    Trace.trace // TODO rm
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
    Trace.trace // TODO rm
  }

  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, Machine] = {
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    // machines that DICOM files reference (based on device serial numbers)
    val referencedMachines = rtimageList.map(df => attributeListToMachine(df.attributeList.get)).flatten.distinct
    val chosenMachine = for (pkTxt <- valueMap.get(machineSelector.label); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m

    val result: Either[StyleMapT, Machine] = 0 match {
      case _ if (referencedMachines.size == 1) => Right(referencedMachines.head)
      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
      case _ if (referencedMachines.size > 1) => formErr("Files come from more than one machine; please go back and try again.  Machines: " + referencedMachines.map(m => m.id).mkString("  "))
      case _ => formErr("Unknown machine.  Please choose from the 'Machine' list below or use the Administration interface to add it.")
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
  private def basicValidation(valueMap: ValueMapT): Either[StyleMapT, BasicData] = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val rtplanList = Phase2Util.getPlanList(dicomFileList)
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))
    val machineSerialNumberListOpt = rtimageList.map(rtimage => Util.getAttrValue(rtimage.attributeList.get, TagFromName.DeviceSerialNumber))
    val machineSerialNumberList = machineSerialNumberListOpt.flatten
    val nullSerialNumber = machineSerialNumberList.size != machineSerialNumberListOpt.size

    def rtimageDate(rtimage: DicomFile): Long = {
      Util.extractDateTimeAndPatientIdFromDicom(rtimage.attributeList.get)._1.map(d => d.getTime).distinct.sorted.last
    }

    val dateTimeList = rtimageList.map(rtimage => rtimageDate(rtimage)).sorted
    val maxDuration = Math.round(Config.MaxProcedureDuration * 60 * 1000).toLong

    val machineCheck = validateMachineSelection(valueMap, rtimageList)

    // associate each image with a plan
    val planGroups = rtplanList.map(plan => (plan, rtimageList.filter(img => Phase2Util.imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    if (true) { // TODO rm
      planGroups.keys.map(plan => println("\n\nPlan: " + plan.file.getName + "    size: " + planGroups(plan).size +
        "\n    " + planGroups(plan).map(d => d.file.getName).mkString("\n    ")))
      println
    }
    /**
     * Make a human readable list of machines
     */
    def machineList: String = {
      val dstnct = machineSerialNumberList.distinct
      val idList = dstnct.map(serNo => Machine.findMachinesBySerialNumber(serNo)).flatten.distinct.map(mach => mach.id).mkString("  ")
      dstnct.mkString("Serial Numbers: " + dstnct.mkString("  ")) + "    Machines: " + idList
    }

    0 match {
      case _ if (rtplanList.isEmpty) => formErr("No RTPLANS found")
      case _ if (rtimageList.isEmpty) => formErr("No RTIMAGEs given")
      case _ if (planGroups.isEmpty) => formErr("No RTPLAN found for RTIMAGEs")
      case _ if (planGroups.size > 1) => formErr("The RTIMAGEs reference multiple plans.  Only one plan per run is permitted.")
      case _ if (planGroups.head._2.size < rtimageList.size) => formErr("There are " + rtimageList.size + " images but only " + planGroups.head._2.size + " reference this plan")
      case _ if (machineCheck.isLeft) => Left(machineCheck.left.get)
      case _ if (machineSerialNumberList.distinct.size != 1) => formErr("There are RTIMAGEs from more than one machine: " + machineList)
      case _ if (nullSerialNumber) => formErr("At least one RTIMAGE has no serial number.")
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
    else Some("Same beam is referenced by multiple files:" + WebUtil.titleNewline + multiRefList.map(mr => mr.map(r => r._1.get.trim + "-->" + r._2.file.getName.trim).mkString(WebUtil.titleNewline)).mkString(WebUtil.titleNewline))
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
      case _ => Right(new RunReq(basicData.rtplan, basicData.machine, rtimageMap, flood.head._2)) // success
    }
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validate(valueMap: ValueMapT, dicomFileList: Seq[DicomFile]): Either[StyleMapT, RunReq] = {
    basicValidation(valueMap) match {
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

  private def dateTimePatId(rtimageList: Seq[DicomFile]) = {
    val list = rtimageList.map(df => Util.dateTimeAndPatientIdFromDicom(df.file))
    if (list.isEmpty) new Util.DateTimeAndPatientId(None, None)
    else list.minBy(dt => dt.dateTime)
  }

  private def makeHtml(extendedData: ExtendedData, procedureStatus: ProcedureStatus.Value, elemList: Seq[Elem], runReq: RunReq) = {

    def table = {
      <div class="col-md-10 col-md-offset-1">
        <table class="table table-responsive">
          <tr>
            { elemList.map(e => <td>{ e }</td>) }
          </tr>
        </table>
      </div>
    }

    val text = Phase2Util.wrapSubProcedure(extendedData, table, "Phase 2", procedureStatus, None, runReq)
    val file = new File(extendedData.output.dir, Output.displayFilePrefix + ".html")
    Util.writeBinaryFile(file, text.getBytes)
  }

  /**
   * Create a list of the RTIMAGEs mapped by beam name.
   */
  private def constructRtimageMap(plan: DicomFile, rtimageList: Seq[DicomFile]) = {
    rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(plan, rtimage), rtimage)).filter(ni => ni._1.isDefined).map(ni => (ni._1.get, ni._2)).toMap
  }

  /**
   * Run the sub-procedures.
   */
  private def runPhase2(extendedData: ExtendedData, rtimageMap: Map[String, DicomFile], runReq: RunReq): ProcedureStatus.Value = {
    logger.info("Starting Phase2 analysis")

    val summaryList: Either[Seq[Elem], Seq[Elem]] = MetadataCheckAnalysis.runProcedure(extendedData, runReq) match {
      case Left(fail) => Left(Seq(fail))
      case Right(metadataCheck) => {
        BadPixelAnalysis.runProcedure(extendedData, runReq) match {
          case Left(fail) => Left(Seq(metadataCheck.summary, fail))
          case Right(badPixel) => {
            CenterDoseAnalysis.runProcedure(extendedData, runReq) match {
              case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, fail))
              case Right(centerDose) => {
                CollimatorCenteringAnalysis.runProcedure(extendedData, runReq) match {
                  case Left(fail) => Left(Seq(metadataCheck.summary, badPixel.summary, centerDose.summary, fail))
                  case Right(collimatorCentering) => {

                    val prevSummaryList = Seq(metadataCheck, badPixel, centerDose, collimatorCentering).map(r => r.summary)
                    // TODO run in parallel
                    val list = Seq(
                      CollimatorPositionAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result),
                      WedgeAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result, centerDose.resultList),
                      SymmetryAndFlatnessAnalysis.runProcedure(extendedData, runReq),
                      LeafPositionAnalysis.runProcedure(extendedData, runReq, collimatorCentering.result))

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
   * Respond to the 'Run' button.
   */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    val dicomFileList = dicomFilesInSession(valueMap)
    val rtimageList = dicomFileList.filter(df => df.isModality(SOPClass.RTImageStorage))

    validate(valueMap, dicomFileList) match {
      case Left(errMap) => {
        form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
      }
      case Right(runReq) => {
        // only consider the RTIMAGE files for the date-time stamp.  The plan could have been from months ago.
        val dtp = dateTimePatId(rtimageList)

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, runReq.machine, sessDir, getUser(request), dtp.PatientID, dtp.dateTime) // TODO This takes a long time.  Optimize?
        val input = inputOutput._1
        val output = inputOutput._2

        val later = Future {
          val extendedData = ExtendedData.get(output)
          val runReqFinal = runReq.reDir(input.dir)

          val plan = runReqFinal.rtplan
          val machine = runReqFinal.machine
          Phase2Util.saveRtplan(plan)

          val rtimageMap = constructRtimageMap(plan, rtimageList)

          val finalStatus = runPhase2(extendedData, rtimageMap, runReqFinal)
          val finDate = new Timestamp(System.currentTimeMillis)
          val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))

          Phase2Util.setMachineSerialNumber(machine, runReqFinal.flood.attributeList.get)
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
    Trace.trace(valueMap) // TODO rm

    try {
      Trace.trace // TODO rm
      0 match {
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
        case _ if buttonIs(valueMap, cancelButton) => cancel(valueMap, response)
        case _ if buttonIs(valueMap, runButton) => run(valueMap, request, response)
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
    Trace.trace // TODO rm
  }

}
