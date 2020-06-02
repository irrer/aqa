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
import org.aqa.run.RunTrait
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass

object Phase2 extends Logging {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"

  /**
   * Create a list of the RTIMAGEs mapped by beam name.
   */
  private def constructRtimageMap(plan: AttributeList, rtimageList: Seq[AttributeList]) = {
    rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(plan, rtimage), rtimage)).filter(ni => ni._1.isDefined).map(ni => (ni._1.get, ni._2)).toMap
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

}

/**
 * Run Phase2 code.
 */
class Phase2(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[RunReq] {

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

  //  private def validateMachineSelection(valueMap: ValueMapT, dicomFileList: Seq[AttributeList]): Either[StyleMapT, Machine] = {
  //    val rtimageList = dicomFileList.filter(al => Util.isRtimage(al))
  //    // machines that DICOM files reference (based on device serial numbers)
  //    val referencedMachines = rtimageList.map(al => Machine.attributeListToMachine(al)).flatten.distinct
  //    val chosenMachine = for (pkTxt <- valueMap.get(machineSelector.label); pk <- Util.stringToLong(pkTxt); m <- Machine.get(pk)) yield m
  //
  //    val result: Either[StyleMapT, Machine] = 0 match {
  //      case _ if (referencedMachines.size == 1) => Right(referencedMachines.head)
  //      case _ if (chosenMachine.isDefined) => Right(chosenMachine.get)
  //      case _ if (referencedMachines.size > 1) => formErr("Files come from more than one machine; please go back and try again.  Machines: " + referencedMachines.map(m => m.id).mkString("  "))
  //      case _ => formErr("Unknown machine.  Please choose from the 'Machine' list below or click Cancel and then use the Administration interface to add it.")
  //    }
  //    result
  //  }

  private case class BasicData(rtplan: AttributeList, rtimageListByBeam: Seq[(Option[String], AttributeList)]) {

    private def dfToString(al: AttributeList) = {
      val sop = Util.sopOfAl(al)
      val modality = Util.modalityOfAl(al)
      sop + "    " + modality
    }

    private def beamOf(beamName: Option[String]) = if (beamName.isDefined) beamName.get else "NoBeamName"

    override def toString =
      "machine: " + dfToString(rtplan) + "\n" +
        "rtimageListByBeam:\n    " + rtimageListByBeam.map(r => beamOf(r._1) + "  " + dfToString(r._2)).mkString("\n    ")
  }

  /**
   * Check that there is a single plan, single machine, and some images.
   */
  private def basicValidation(valueMap: ValueMapT, rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Either[StyleMapT, BasicData] = {
    logger.info("Number of RTPLAN files uploaded: " + rtplanList.size)
    logger.info("Number of RTIMAGE files duploaded: " + rtimageList.size)
    val machineSerialNumberList = rtimageList.map(rtimage => Util.getAttrValue(rtimage, TagFromName.DeviceSerialNumber)).flatten

    def rtimageDate(rtimage: AttributeList): Long = {
      Util.extractDateTimeAndPatientIdFromDicomAl(rtimage)._1.map(d => d.getTime).distinct.sorted.last
    }

    val dateTimeList = rtimageList.map(rtimage => rtimageDate(rtimage)).sorted
    val maxDuration = Math.round(Config.MaxProcedureDuration * 60 * 1000).toLong

    // associate each image with a plan
    //val planGroups = rtplanList.map(plan => (plan, rtimageList.filter(img => Phase2Util.imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    val planUIDReferences = rtimageList.map(img => Phase2Util.referencedPlanUID(img)).distinct

    // Look in the uploaded rtplan list and DicomSeries in the database for the plan(s) referenced.  If that fails, then try the shared directory.
    val referencedRtplanList: Seq[AttributeList] = {
      val matchingUploaded = rtplanList.filter(plan => planUIDReferences.contains(Util.sopOfAl(plan)))
      val dbList = planUIDReferences.map(planUID => DicomSeries.getBySopInstanceUID(planUID)).flatten.map(ds => ds.attributeListList.head)
      val list = matchingUploaded ++ dbList
      if (list.nonEmpty)
        list
      else {
        // TODO deprecate this when the shared directory is removed
        val sharedList = DicomFile.readDicomInDir(Config.sharedDir).filter(df => df.isRtplan).map(df => df.attributeList).flatten
        val matchingList = sharedList.filter(plan => planUIDReferences.contains(Util.sopOfAl(plan)))
        matchingList
      }
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
      case _ if (planUIDReferences.size > 1) => formErr("The RTIMAGES reference more than one RTPLAN.")
      case _ if (referencedRtplanList.isEmpty) => formErr("Can not find the referenced RTPLAN.  Retry and upload the RTPLAN with the images. ")
      case _ if (rtimageList.isEmpty) => formErr("No RTIMAGEs given")

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

      case _ if ((dateTimeList.last - dateTimeList.head) > maxDuration) =>
        formErr("Over " + Config.MaxProcedureDuration + " minutes from first to last image.  These RTIMAGE files were not from the same session")
      case _ => {
        val rtplan = referencedRtplanList.head
        val rtimageByBeam = rtimageList.map(rtimage => (Phase2Util.getBeamNameOfRtimage(rtplan, rtimage), rtimage))
        Right(new BasicData(rtplan, rtimageByBeam)) // TODO remove machine from class?
      }
    }
  }

  /**
   *  determine if there are undefined beams
   */
  private def beamNotDefinedProblem(basicData: BasicData): Option[String] = {
    // check for beams that are not in the plan
    val undefFileNameList = basicData.rtimageListByBeam.filter(b => b._1.isEmpty).map(b => Util.sopOfAl(b._2))
    if (undefFileNameList.nonEmpty) {
      Some("The image set is probably referencing the wrong plan." + WebUtil.titleNewline +
        "There were " + undefFileNameList.size + " files that references a beam  that was not in the plan:" + WebUtil.titleNewline +
        undefFileNameList.mkString(WebUtil.titleNewline))
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
      val text = "Same beam is referenced by multiple files:" + sep + multiRefList.map(mr => mr.map(r => r._1.get.trim + "-->" + Util.sopOfAl(r._2)).mkString(sep)).mkString(sep)
      Some(text)
    }
  }

  /**
   * Check beam definitions, existence of flood field, and organize inputs into <code>RunReq</code> to facilitate further processing.
   */
  def basicBeamValidation(basicData: BasicData): Either[StyleMapT, RunReq] = {
    val rtimageMap = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && (!rl._1.get.equals(Config.FloodFieldBeamName))).map(rl => (rl._1.get, rl._2)).toMap
    val flood = basicData.rtimageListByBeam.filter(rl => rl._1.isDefined && (rl._1.get.equals(Config.FloodFieldBeamName)))

    (beamNotDefinedProblem(basicData), beamMultiRefProblem(basicData)) match {
      case (Some(errorMessage), _) => formErr(errorMessage)
      case (_, Some(errorMessage)) => formErr(errorMessage)
      case _ if (flood.isEmpty) => formErr("Flood field beam is missing")
      case _ => Right(new RunReq(basicData.rtplan, rtimageMap, flood.head._2)) // success
    }
  }

  /**
   * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
   */
  private def validatePhase2(valueMap: ValueMapT, rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]): Either[StyleMapT, RunReq] = {
    basicValidation(valueMap, rtplanList: Seq[AttributeList], rtimageList: Seq[AttributeList]) match {
      case Left(fail) => Left(fail)
      case Right(basicData) => {
        logger.info("Received file list: " + basicData)
        val basicBeamValid = basicBeamValidation(basicData)
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
  private def runPhase2(extendedData: ExtendedData, runReq: RunReq): ProcedureStatus.Value = {
    logger.info("Starting Phase2 analysis")

    val alList = runReq.rtimageMap.values.toSeq

    // Save DICOM series to the database if they are not already there.  It is possible that the RTIMAGE files are
    // from different series, so make sure to split them by series and store them separately.  This is important for
    // the automatic client that will ask this server if a given series has been processed or not.  This actually
    // happened once.
    val seriesList = alList.groupBy(al => Util.serInstOfAl(al)).map(series => series._2)
    seriesList.map(series => DicomSeries.insertIfNew(extendedData.user.userPK.get, extendedData.input.inputPK, extendedData.machine.machinePK, series))

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
                      if (Config.VMATDeviationThreshold_pct == -1) {
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

  override def makeRunReqForRedo(alList: Seq[AttributeList]): RunReqClass = {
    //val result = validate(emptyValueMap, alList.filter(al => Util.isRtimage(al)))
    val rtimageList = alList.filter(al => Util.isRtimage(al))

    def getRtplan = {
      val rtplanUID = Phase2Util.referencedPlanUID(rtimageList.head)
      DicomSeries.getBySeriesInstanceUID(rtplanUID).headOption match {
        // get this from the database
        case Some(ds) => ds.attributeListList.head
        case _ => {
          // if it was not in the database, then check in the share directory.  This code should be deprecated soon.  TODO
          val file = new File(Config.sharedDir, rtplanUID + ".dcm")
          val df = new DicomFile(file)
          df.attributeList.get
        }
      }
    }

    val rtplan = getRtplan
    val rtimageMap = rtimageList.map(al => (Phase2Util.getBeamNameOfRtimage(rtplan, al).get, al)).toMap
    val floodBeamName = rtimageMap.keys.find(_.toLowerCase.contains("flood")).get
    val runReq = new RunReq(rtplan, rtimageMap, rtimageMap(floodBeamName))
    runReq
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList]): Seq[String] = {
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    val dsnList = rtimageList.map(al => Util.attributeListToDeviceSerialNumber(al)).flatten.distinct
    dsnList
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[String] = {
    alList.filter(al => Util.isRtimage(al)).map(al => Util.patientIdOfAl(al)).headOption
  }

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList]): Option[Timestamp] = {
    val min = alList.filter(al => Util.isRtimage(al)).map(al => Util.extractDateTimeAndPatientIdFromDicomAl(al)).map(dp => dp._1).flatten.minBy(_.getTime)
    Some(new Timestamp(min.getTime))
  }

  override def getProcedure: Procedure = procedure

  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList]): Either[StyleMapT, RunReqClass] = {
    val rtplanList = alList.filter(al => Util.isRtplan(al))
    val rtimageList = alList.filter(al => Util.isRtimage(al))
    validatePhase2(valueMap, rtplanList, rtimageList)
  }

  override def run(extendedData: ExtendedData, runReq: RunReq, response: Response): ProcedureStatus.Value = {
    val status = runPhase2(extendedData, runReq)
    //.runProcedure(extendedData, runReq)
    status
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = getValueMap(request)
    RunProcedure.handle(valueMap, request, response, this.asInstanceOf[RunTrait[RunReqClass]])
  }

}
