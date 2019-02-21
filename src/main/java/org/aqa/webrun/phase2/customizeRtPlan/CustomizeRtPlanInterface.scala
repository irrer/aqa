package org.aqa.webrun.phase2.customizeRtPlan

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.CachedUser
import org.restlet.data.Status
import scala.xml.Elem
import org.aqa.Config
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import org.aqa.db.MultileafCollimator
import org.aqa.db.MachineBeamEnergy
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import org.aqa.AnonymizeUtil
import com.pixelmed.dicom.ValueRepresentation
import com.pixelmed.dicom.AttributeTag
import edu.umro.util.UMROGUID
import java.io.File
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SequenceItem
import org.aqa.VarianPrivateTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.LongStringAttribute
import com.pixelmed.dicom.CodeStringAttribute
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebServer

object CustomizeRtPlanInterface {
  def reference(machinePK: Long) = { (new CustomizeRtPlanInterface).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlanInterface extends Restlet with SubUrlRoot with Logging {

  private val pageTitleSelect = "Select Plan Parameters"

  private val machineIdTag = "MachineId"

  private val machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def machineFromValueMap(valueMap: ValueMapT): Machine = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    machine
  }

  private def linkToMachineUpdate(valueMap: ValueMapT): Elem = {
    val machine = machineFromValueMap(valueMap)
    MachineUpdate.linkToMachineUpdate(machine.machinePK.get, machine.id)
  }

  private def machineIdHtml(valueMap: ValueMapT): Elem = {
    <h3 title="Plan customization for machine">Custom RT Plan Comptatible with Machine { linkToMachineUpdate(valueMap) }</h3>
  }

  val machineId = new WebPlainText(machineIdTag, false, 6, 0, machineIdHtml)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", false)

  private def machineName = new WebInputText("Machine Name", true, 2, 0, "To match planning system", false)

  private def patientID = new WebInputText("Patient ID", true, 2, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row0: WebRow = List(machineId)
  private val row2: WebRow = List(toleranceTable, machineName)
  private val row1: WebRow = List(patientID, patientName)

  private def getMachineEnergyList(machinePK: Long): Seq[MachineBeamEnergy] = {
    def compareMBE(a: MachineBeamEnergy, b: MachineBeamEnergy): Boolean = {

      val cmpr = (a.photonEnergy_MeV, b.photonEnergy_MeV, a.fffEnergy_MeV, b.fffEnergy_MeV) match {
        case (Some(aPho), Some(bPho), _, _) if aPho != bPho => aPho < bPho
        case (Some(aPho), _, _, _) => true
        case (_, Some(bPho), _, _) => false
        case (_, _, Some(afff), Some(bfff)) if afff != bfff => afff < bfff
        case (_, _, Some(afff), _) => true
        case (_, _, _, Some(bfff)) => false
        case _ => true
      }
      cmpr
    }

    val list = MachineBeamEnergy.getByMachine(machinePK).sortWith(compareMBE)
    list
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", false, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)
  private val backButton = makeButton("Back", false, ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(createButton, cancelButton, machinePK)

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine) = {
    val form = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList))

    // if field is empty
    def empty(label: String) = valueMap.get(label).isEmpty || (valueMap(label).trim.size == 0)

    val defaultPatient = "$AQA_" + machine.machinePK.get

    def getRealMachineId = AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)

    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val machineNameMap = if (empty(machineName.label)) Map((machineName.label, getRealMachineId)) else emptyValueMap

    val valMap = valueMap ++ patientIdMap ++ patientNameMap ++ machineNameMap
    form.setFormResponse(valMap, styleNone, pageTitleSelect, response, Status.SUCCESS_OK)
  }

  /**
   * Make sure fields are valid.
   */
  private def validate(valueMap: ValueMapT): StyleMapT = {
    // if field is empty
    def empty(label: String) = valueMap.get(label).isEmpty || (valueMap(label).trim.size == 0)

    val tolErr = if (empty(toleranceTable.label)) Error.make(toleranceTable, "A tolerance table name must be given.") else styleNone
    val machErr = if (empty(machineName.label)) Error.make(machineName, "A machine name must be given.") else styleNone
    val patIdErr = if (empty(patientID.label)) Error.make(patientID, "A patient ID must be given.") else styleNone
    val patNameErr = if (empty(patientName.label)) Error.make(patientName, "A patient name must be given.") else styleNone

    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)
    val collimatorErr = if (CustomizeRtPlan.getCollimatorCompatiblePlanForMachine(machine.get).isEmpty) Error.make(createButton, "There is no pre-defined plan to support this machine's collimator.") else styleNone

    (tolErr ++ machErr ++ patIdErr ++ patNameErr ++ collimatorErr)
  }
  
  private def showDownload(rtplan: AttributeList, valueMap: ValueMapT, machine: Machine, response: Response) = {

    val sopuid = Util.sopOfAl(rtplan)
    val file = new File(Config.tmpDirFile, sopuid + ".dcm")
    DicomUtil.writeAttributeList(rtplan, file)
    val downloadUrl = WebServer.urlOfTmpFile(file)

    val downloadLink = new WebPlainText("Download", false, 3, 0, (ValueMapT) => { <h4> <a href={ downloadUrl } title="Click to download DICOM RTPLAN file.">Download</a></h4> })

    val dicomViewHtml = { <span><h4><p/>Preview</h4><p/><pre title="DICOM meta-data">{ WebUtil.nl + DicomUtil.attributeListToString(rtplan) }</pre></span> }
    val dicomView = new WebPlainText("Download", false, 10, 0, (ValueMapT) => dicomViewHtml)

    val r1: WebRow = List(downloadLink, backButton, machinePK)
    val r2: WebRow = List(dicomView)
    val form = new WebForm(pathOf, List(row0, r1, r2))

    form.setFormResponse(valueMap, styleNone, "Download RTPLAN", response, Status.SUCCESS_OK)
  }

  private case class BeamReference(beam: AttributeList, fractionReference: AttributeList);

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  private def validateAndMakePlan(valueMap: ValueMapT, response: Response) = {
    val styleMap = validate(valueMap)
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
    val planEnergyList = CustomizeRtPlan.getPlanBeamList(machine).toList
    if (styleMap.nonEmpty) {
      val form = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList))
      form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
    } else {
      val planSpecification = new CustomizeRtPlan.PlanSpecification(
        valueMap(toleranceTable.label),
        valueMap(patientID.label),
        valueMap(patientName.label),
        valueMap(machineName.label))
      val rtplan = CustomizeRtPlan.makePlan(machine, planEnergyList, planSpecification, machineEnergyList)
      showDownload(rtplan, valueMap, machine, response)
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if user.isEmpty => updateMach
        case _ if machine.isEmpty => updateMach
        case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach
        case _ if buttonIs(valueMap, cancelButton) => updateMach
        case _ if buttonIs(valueMap, backButton) => updateMach
        case _ if buttonIs(valueMap, createButton) => validateAndMakePlan(valueMap, response)
        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }
}
