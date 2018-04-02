package org.aqa.webrun

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
import org.aqa.db.ImageIdentification
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
import org.aqa.web.DicomAccess
import org.aqa.web.WebServer
import com.pixelmed.dicom.TimeAttribute
import edu.umro.ScalaUtil.DicomUtil

object Phase2 {
  val parametersFileName = "parameters.xml"
  val Phase2RunPKTag = "Phase2RunPK"
  val spreadsheetHtmlFileName = "spreadsheet.html"
  val spreadsheetFileName = "spreadsheet.xlsx"
}

/**
 * Run Phase2 code.
 */
class Phase2(procedure: Procedure) extends WebRunProcedure(procedure) with PostProcess with Logging {

  /** Defines precision - Format to use when showing numbers. */
  val outputFormat = "%7.5e"

  private val machine = new WebInputSelectOption("Machine", 6, 0, machineList, showMachineSelector)

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("Phase2"), List(List(machine), List(runButton, cancelButton)), 6)

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private case class RunRequirements(machine: Machine, sessionDir: File, plan: DicomFile, imageIdFileList: Seq[ImageIdentificationFile]) {
    val pass = (imageIdFileList.size > 2) && imageIdFileList.map(iif => iif.imageIdentification.pass).reduce(_ && _)
  }

  def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

  private def validate(valueMap: ValueMapT, outputPK: Option[Long]): Either[StyleMapT, RunRequirements] = {
    val rtimageList = dicomFilesInSession(valueMap).filter(df => df.isModality(SOPClass.RTImageStorage))
    val rtplanList = dicomFilesInSession(valueMap).filter(df => df.isModality(SOPClass.RTPlanStorage))
    val dicomList = rtplanList ++ rtimageList

    // machines that DICOM files reference (based on device serial numbers)
    val machList = rtimageList.map(df => attributeListToMachine(df.attributeList.get)).flatten.distinct

    // The machine to use
    val mach = {
      machList.headOption match {
        case Some(mach) => Some(mach)
        case _ => for (pkTxt <- valueMap.get(machine.label); pk <- Util.stringToLong(pkTxt); mach <- Machine.get(pk)) yield mach
      }
    }

    val imgIdent = analyzeImageIdentification(dicomList, mach)

    sessionDir(valueMap) match {
      case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded")
      case _ if (dicomList.isEmpty) => formErr("No DICOM files have been uploaded.")
      case _ if (machList.size > 1) => formErr("Files from more than one machine were found.  Click Cancel to start over.")
      case _ if (mach.isEmpty) => formErr("These files do not have a serial number of a known machine.  Choose a machine from the list.  If it is not on the list, then use Administration --> Machines --> Create new Machine.")
      case _ if (imgIdent.isLeft) => Left(imgIdent.left.get)
      case Some(dir) => Right(new RunRequirements(mach.get, dir, imgIdent.right.get._1, imgIdent.right.get._2))
    }
  }

  /**
   * Get all RTPLAN files, including any that were just now downloaded and any that are in the
   * machine's configuration directory.  If a plan is found more than once then return only one
   * occurrence of it.  If the plan occurs both in the machine's configuration directory and was
   * downloaded, then prefer the one in the machine's configuration directory.
   */
  private def getPlanList(dicomList: Seq[DicomFile], machine: Option[Machine]): Seq[DicomFile] = {
    val configuredPlans: Seq[DicomFile] = {
      try {
        DicomFile.readDicomInDir(machine.get.configDir.get).filter(df => df.isModality(SOPClass.RTPlanStorage))
      } catch {
        case t: Throwable => {
          logger.warn("Unexpected problem while getting pre-configured RTPLANs: " + t)
          Seq[DicomFile]()
        }
      }
    }

    val downloadedPlans: Seq[DicomFile] = try {
      val configSopList = configuredPlans.map(c => Util.sopOfAl(c.attributeList.get))
      dicomList.filter(df => df.isModality(SOPClass.RTPlanStorage)).filter(d => !configSopList.contains(Util.sopOfAl(d.attributeList.get)))
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected problem while getting RTPLAN: " + t)
        Seq[DicomFile]()
      }
    }

    DicomFile.distinctSOPInstanceUID(configuredPlans ++ downloadedPlans)
  }

  /**
   * Determine if the given image references the given plan.
   */
  private def imageReferencesPlan(plan: DicomFile, image: DicomFile): Boolean = {
    try {
      val planUID = plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
      def doesRef(al: AttributeList) = al.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrNull.equals(planUID)
      Util.seq2Attr(image.attributeList.get, TagFromName.ReferencedRTPlanSequence).map(al => doesRef(al)).nonEmpty
    } catch {
      case t: Throwable => false
    }
  }

  val MIN_IMAGES = 3

  case class ImageIdentificationFile(dicomFile: DicomFile, imageIdentification: ImageIdentification) {
    def move(oldDir: File, newDir: File): ImageIdentificationFile = {
      if (dicomFile.file.getParentFile == oldDir) {
        new ImageIdentificationFile(new DicomFile(new File(newDir, dicomFile.file.getName)), imageIdentification)
      } else this
    }
  }

  /**
   * Perform an image identification analysis of the
   */
  private def analyzeImageIdentification(dicomList: Seq[DicomFile], machine: Option[Machine]): Either[StyleMapT, (DicomFile, Seq[ImageIdentificationFile])] = {
    val planList = getPlanList(dicomList, machine)
    val imageList = dicomList.filter(df => df.isModality(SOPClass.RTImageStorage))

    // associate each image with a plan
    val planGroups = planList.map(plan => (plan, imageList.filter(img => imageReferencesPlan(plan, img)))).filter(pi => pi._2.nonEmpty).toMap

    0 match {
      case _ if (planList.isEmpty) => formErr("No RTPLANS found")
      case _ if (imageList.isEmpty) => formErr("No RTIMAGEs given")
      case _ if (planGroups.isEmpty) => formErr("No RTPLAN found for RTIMAGEs")
      case _ if (planGroups.size > 1) => formErr("The RTIMAGEs reference multiple plans.  Only one plan per run is permitted.")
      case _ => {
        val plan = planGroups.head._1
        val imageList = planGroups.head._2
        val results = imageList.
          map(image => (image, ImageIdentificationAnalysis.makeImageIdentification(plan.attributeList.get, image.attributeList.get))).
          filter(ii => ii._2.nonEmpty).
          map(ii => new ImageIdentificationFile(ii._1, ii._2.get))
        // only let one result in for each beam
        val distinct = results.map(iif => (iif.imageIdentification.beamName, iif)).toMap.values.toSeq
        0 match {
          case _ if (results.size != distinct.size) => {
            val multiImageBeams = distinct.diff(results).map(iff => iff.imageIdentification.beamName).distinct
            formErr("Multiple RTIMAGEs were taken from beam " + multiImageBeams)
          }
          case _ if (results.size < MIN_IMAGES) => formErr("Your must provide at least " + MIN_IMAGES + " but only " + results.size + " were found")
          case _ => Right(plan, results)
        }
      }
    }
  }

  private def saveResultsToDatabase(output: Output, imageIdFileList: Seq[ImageIdentificationFile]) = {
    val list = imageIdFileList.map(imgId => imgId.imageIdentification.copy(outputPK = output.outputPK.get))
    ImageIdentification.insert(list)
  }

  private def makeCsvFile(procedureDesc: String, institutionName: String, outputDir: File, machineId: String, acquisitionDate: String, analysisDate: String, userId: String, runReq: RunRequirements) = {

    type II = ImageIdentification

    val dblFmt = "%14.11f"
    val textFmt = "%s"
    val columns: Seq[(String, (II) => Any)] = Seq(
      ("beamName", (ii: II) => ii.beamName),
      ("gantryAnglePlan_deg", (ii: II) => ii.gantryAnglePlan_deg),
      ("gantryAnglePlanMinusImage_deg", (ii: II) => ii.gantryAnglePlanMinusImage_deg),
      ("collimatorAnglePlan_deg", (ii: II) => ii.collimatorAnglePlan_deg),
      ("collimatorAnglePlanMinusImage_deg", (ii: II) => ii.collimatorAnglePlanMinusImage_deg),
      ("x1JawPlan_mm", (ii: II) => ii.x1JawPlan_mm),
      ("x1JawPlanMinusImage_mm", (ii: II) => ii.x1JawPlanMinusImage_mm),
      ("x2JawPlan_mm", (ii: II) => ii.x2JawPlan_mm),
      ("x2JawPlanMinusImage_mm", (ii: II) => ii.x2JawPlanMinusImage_mm),
      ("y1JawPlan_mm", (ii: II) => ii.y1JawPlan_mm),
      ("y1JawPlanMinusImage_mm", (ii: II) => ii.y1JawPlanMinusImage_mm),
      ("y2JawPlan_mm", (ii: II) => ii.y2JawPlan_mm),
      ("y2JawPlanMinusImage_mm", (ii: II) => ii.y2JawPlanMinusImage_mm),
      ("energyPlan_kev", (ii: II) => ii.energyPlan_kev),
      ("energyPlanMinusImage_kev", (ii: II) => ii.energyPlanMinusImage_kev),
      ("flatteningFilter", (ii: II) => ii.flatteningFilter),
      ("pass", (ii: II) => ii.pass))

    def imageIdentificationToCsv(ii: ImageIdentification): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _ => Util.textToCsv(any.toString)
        }
      }
      columns.map(c => fmt(c._2(ii))).mkString(",")
    }

    val metaData = {
      val info = Seq(
        ("Procedure", procedureDesc),
        ("Machine", machineId),
        ("Institution", institutionName),
        ("Acquisition Date", acquisitionDate),
        ("Analysis Date", analysisDate),
        ("User", userId))

      Seq(
        info.map(s => Util.textToCsv(s._1)).mkString(","),
        info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data = runReq.imageIdFileList.map(iif => imageIdentificationToCsv(iif.imageIdentification))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(outputDir, ImageIdentification.csvFileName)
    Util.writeFile(file, text)
  }

  private def makeDisplay(output: Output, runReq: RunRequirements) = {

    def wrap(col: Int, elem: Elem): Elem = {
      <div class={ "col-md-" + col }>{ elem }</div>
    }

    def wrap2(col: Int, name: String, value: String): Elem = {
      <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ value }</div>
    }

    val machine = if (output.machinePK.isDefined) Machine.get(output.machinePK.get) else None
    val institution = if (machine.isDefined) Institution.get(machine.get.institutionPK) else None
    val input = Input.get(output.inputPK)
    val procedure = Procedure.get(output.procedurePK)
    val user = if (output.userPK.isDefined) User.get(output.userPK.get) else None

    val institutionName = if (institution.isDefined) institution.get.name else "unknown"

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _ => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _ => "unknown"
      }
    }

    val machineId = if (machine.isDefined) machine.get.id else "unknown"
    val userId = if (user.isDefined) user.get.id else "unknown"

    val elapsed: String = {
      val fin = output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = {
      procedure match {
        case Some(proc) =>
          proc.name + " : " + proc.version
        case _ => ""
      }
    }

    makeCsvFile(
      procedureDesc,
      institutionName,
      output.dir,
      machineId,
      (if (output.dataDate.isDefined) Util.standardDateFormat.format(output.dataDate.get) else "none"),
      (Util.standardDateFormat.format(if (output.analysisDate.isDefined) output.analysisDate.get else (new Date))),
      userId,
      runReq)

    val csvFileReference = {
      <a title="Download Image Identification as CSV File" href={ ImageIdentification.csvFileName }>CSV</a>
    }

    val viewRtPlan = {
      val title = "RT Plan"
      val link = DicomAccess.write(runReq.plan, WebServer.urlOfMachineConfigurationFile(runReq.plan.file), title, output.dir, DicomFile.ContrastModel.maxContrast)
      val elem = { <a title="View RT Plan DICOM file" href={ link }>{ title }</a> }
      elem
    }

    class Row(val title: String, name: String, val get: (ImageIdentificationFile) => String) {
      def toHeader = <th title={ title }>{ name }</th>
      def toRow(imgId: ImageIdentificationFile) = <td title={ title }>{ get(imgId) }</td>
    }

    def degree(diff: Double): String = diff.formatted("%6e")

    def jaw(diff: Double): String = diff.formatted("%6e")

    class RowBeamName(override val title: String, name: String, override val get: (ImageIdentificationFile) => String) extends Row(title, name, get) {
      override def toRow(imgIdFile: ImageIdentificationFile) = {

        val link = DicomAccess.write(imgIdFile.dicomFile, WebServer.urlOfResultsFile(imgIdFile.dicomFile.file), get(imgIdFile) + " : " + imgIdFile.dicomFile.file.getName, output.dir, DicomFile.ContrastModel.maxContrast)

        val elem = { <td title={ title + ".  Follow link to view DICOM" }><a href={ link }>{ get(imgIdFile) }</a></td> }
        elem
      }
    }

    val rowList = Seq(
      new RowBeamName("Name of beam in plan", "Beam Name", (imgIdFile: ImageIdentificationFile) => imgIdFile.imageIdentification.beamName),
      new Row("Gantry Angle plan minus image in degrees", "Gantry Angle", (imgIdFile: ImageIdentificationFile) => degree(imgIdFile.imageIdentification.gantryAnglePlanMinusImage_deg)),
      new Row("Collimator Angle plan minus image in degrees", "Collimator Angle", (imgIdFile: ImageIdentificationFile) => degree(imgIdFile.imageIdentification.collimatorAnglePlanMinusImage_deg)),
      new Row("X1 Jaw plan minus image in mm", "X1 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.x1JawPlanMinusImage_mm)),
      new Row("X2 Jaw plan minus image in mm", "X2 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.x2JawPlanMinusImage_mm)),
      new Row("Y1 Jaw plan minus image in mm", "Y1 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.y1JawPlanMinusImage_mm)),
      new Row("Y2 Jaw plan minus image in mm", "Y2 Jaw", (imgIdFile: ImageIdentificationFile) => jaw(imgIdFile.imageIdentification.y2JawPlanMinusImage_mm)),
      new Row("Energy plan minus image in kev", "Energy", (imgIdFile: ImageIdentificationFile) => imgIdFile.imageIdentification.energyPlanMinusImage_kev.toString),
      new Row("Yes if Flattening Filter was present", "FF", (imgIdFile: ImageIdentificationFile) => if (imgIdFile.imageIdentification.flatteningFilter) "Yes" else "No"),
      new Row("Pass if angles and jaw differences within tolerences", "Status", (imgIdFile: ImageIdentificationFile) => if (imgIdFile.imageIdentification.pass) "Pass" else "Fail"))

    def imageIdentificationTableHeader: Elem = {
      <thead><tr>{ rowList.map(row => row.toHeader) }</tr></thead>
    }

    def imageIdentificationToTableRow(imgIdFile: ImageIdentificationFile): Elem = {
      if (imgIdFile.imageIdentification.pass) {
        <tr>{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      } else {
        <tr class="danger">{ rowList.map(row => row.toRow(imgIdFile)) }</tr>
      }
    }

    val passFailImage = {
      if (runReq.pass) {
        <div title="Passed!"><img src="/static/images/pass.png" width="128"/></div>
      } else {
        <div title="Failed"><img src="/static/images/fail.png" width="128"/></div>
      }
    }

    val tbody = runReq.imageIdFileList.sortWith((a, b) => DicomUtil.compareDicom(a.dicomFile.attributeList.get, b.dicomFile.attributeList.get) < 0).map(iif => imageIdentificationToTableRow(iif))

    val div = {
      <div class="row col-md-10 col-md-offset-1">
        <div class="row">
          <div class="col-md-1">{ passFailImage }</div>
          <div class="col-md-3" title="Image Identification"><h2>Image Identification</h2></div>
          <div class="col-md-1" title="Machine"><h2>{ machineId }</h2></div>
        </div>
        <div class="row" style="margin:20px;">
          { wrap2(1, "Institution", institutionName) }
          { wrap2(2, "Data Acquisition", dateToString(output.dataDate)) }
          { wrap2(2, "Analysis", analysisDate) }
          { wrap2(1, "User", userId) }
          { wrap2(1, "Elapsed", elapsed) }
          { wrap2(3, "Procedure", procedureDesc) }
        </div>
        <div class="row" style="margin:20px;">
          <div class="col-md-1">{ csvFileReference }</div>
          <div class="col-md-1">{ viewRtPlan }</div>
        </div>
        <div class="row" style="margin:20px;">
          <table class="table table-striped">
            { imageIdentificationTableHeader }
            <tbody>{ tbody }</tbody>
          </table>
        </div>
      </div>
    }

    val text = wrapBody(div, "Image Identification", None, true, None)
    text
  }

  /**
   * If the serial number for the machine is not already set, then set it by using the DeviceSerialNumber in the RTIMAGE.
   */
  private def setMachineSerialNumber(machine: Machine, rtimage: AttributeList) = {
    if (machine.serialNumber.isEmpty) {
      try {
        val DeviceSerialNumber = rtimage.get(TagFromName.DeviceSerialNumber).getSingleStringValueOrNull
        if (DeviceSerialNumber != null) Machine.setSerialNumber(machine.machinePK.get, DeviceSerialNumber)
      } catch {
        case t: Throwable => logger.warn("Unable to update machine serial number " + machine + " : " + t)
      }
    }
  }

  /**
   * If a plan was used that was not already saved, then save it.
   */
  private def saveRtplan(machine: Machine, plan: DicomFile) = {
    try {
      Machine.get(machine.machinePK.get) match {
        case Some(mach) => {
          val cfgDir = mach.configDir.get
          if (!cfgDir.equals(plan.file.getParentFile)) {
            val data = Util.readBinaryFile(plan.file)
            val planFile = new File(cfgDir, plan.attributeList.get.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull + Util.dicomFileNameSuffix)
            Util.writeBinaryFile(planFile, data.right.get)
            logger.info("Wrote new plan file " + planFile)
          }
        }
        case _ => logger.error("Machine not in database.  Unable to save rtplan for machine " + machine)
      }
    } catch {
      case t: Throwable => logger.error("Unable to save rtplan for machine " + machine + " : " + t)
    }
  }

  /**
   * Run the procedure.
   */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    validate(valueMap, None) match {
      case Right(runReqValidated) => {

        // only consider the RTIMAGE files for the date-time stamp.  The plan could have been from months ago.
        val rtimageList = dicomFilesInSession(valueMap).filter(df => df.isModality(SOPClass.RTImageStorage))
        val dtp: Util.DateTimeAndPatientId = {
          val list = rtimageList.map(df => Util.dateTimeAndPatientIdFromDicom(df.file))
          if (list.isEmpty) new Util.DateTimeAndPatientId(None, None)
          else list.minBy(dt => dt.dateTime)
        }

        val sessDir = sessionDir(valueMap).get
        val inputOutput = Run.preRun(procedure, runReqValidated.machine, sessDir, getUser(request), dtp.PatientID, dtp.dateTime)
        val input = inputOutput._1
        val output = inputOutput._2

        val planDicomFile = {
          val plan = runReqValidated.plan
          val machDir = runReqValidated.machine.configDir.get
          if (plan.file.getParentFile == machDir) plan
          else (new DicomFile(new File(input.dir, plan.file.getName)))
        }
        val runReq = runReqValidated.copy(imageIdFileList = runReqValidated.imageIdFileList.map(iif => iif.move(sessDir, input.dir))).copy(plan = planDicomFile)
        saveResultsToDatabase(output, runReq.imageIdFileList)
        val finalStatus = if (runReq.imageIdFileList.find(iif => iif.imageIdentification.pass == false).isEmpty) ProcedureStatus.pass else ProcedureStatus.fail
        val finDate = new Timestamp(System.currentTimeMillis)
        val outputFinal = output.copy(status = finalStatus.toString).copy(finishDate = Some(finDate))
        val display = makeDisplay(outputFinal, runReq)
        Util.writeBinaryFile(new File(output.dir, Output.displayFilePrefix + ".html"), display.getBytes)
        //setResponse(display, response, Status.SUCCESS_OK)

        setMachineSerialNumber(runReq.machine, runReq.imageIdFileList.head.dicomFile.attributeList.get)
        saveRtplan(runReq.machine, runReq.plan)
        outputFinal.insertOrUpdate
        outputFinal.updateData(outputFinal.makeZipOfFiles)
        Run.removeRedundantOutput(outputFinal.outputPK)

        val suffix = "?" + ViewOutput.outputPKTag + "=" + outputFinal.outputPK.get
        response.redirectSeeOther(ViewOutput.path + suffix)
      }
      case Left(errMap) => form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
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
        case _ if buttonIs(valueMap, runButton) => run(valueMap, request, response)
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

  private def insertIntoDatabase(dir: File, outputPK: Long) = {
    val elem = XML.loadFile(new File(dir, ProcedureOutputUtil.outputFileName))
    ProcedureOutputUtil.insertIntoDatabase(elem, Some(outputPK))
  }

  private def makeSpreadsheet(dir: File, locXml: LOCXml, response: Response): Unit = {
    try {
      (new LOCSpreadsheet(dir, locXml, response)).write
    } catch {
      case t: Throwable => logger.warn("Failed to make spreadsheet: " + fmtEx(t))
    }
  }

}
