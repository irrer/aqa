package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
import org.aqa.db.CachedUser
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MultileafCollimator
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

class Phase3HTML extends Restlet with SubUrlRoot with Logging {

  private val pageTitle = "Select Tests for RTPLAN"

  private def makeButton(name: String, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 2, 0, subUrl, pathOf, buttonType)
  }

  private def cancelButton = makeButton("Cancel", ButtonType.BtnDefault)

  private def createPlanButton = makeButton("Create Plan", ButtonType.BtnPrimary)

  private def machinePK = new WebInputHidden(MachineUpdate.machinePKTag)

  private def patientID = new WebInputHidden(CustomizeRtPlanInterface.patientIdTag)

  private def patientName = new WebInputHidden(CustomizeRtPlanInterface.patientNameTag)

  private def machineName = new WebInputHidden(CustomizeRtPlanInterface.machineNameTag)

  private def planName = new WebInputHidden(CustomizeRtPlanInterface.planNameTag)

  private def toleranceTableName = new WebInputHidden(CustomizeRtPlanInterface.toleranceTableNameTag)


  /**
   * Get all beams from Phase3 rtplans that are defined in the configured plans.
   *
   * @param machine For this machine.
   * @return List of beams from multiple plans.
   */
  private def prototypeBeams(machine: Machine): Seq[Beam] = {

    def beamsFromPlan(plan: AttributeList): Seq[Beam] = {
      DicomUtil.seqToAttr(plan, TagByName.BeamSequence).map(Beam)
    }

    val collimatorModel = MultileafCollimator.get(machine.multileafCollimatorPK).get.model

    def getPlan(name: String): Option[AttributeList] = {
      try {
        val plan = Config.PlanFileList.find(pf => pf.collimatorModel.equals(collimatorModel) && pf.procedure.equals(name)).get
        Some(DicomFile(plan.file).attributeList.get)
      }
      catch {
        case _: Throwable => None
      }
    }

    val beamList = Seq("Phase3", "FocalSpot").flatMap(getPlan).flatMap(beamsFromPlan)
    beamList
  }

  private def subProcedureList(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator): Seq[SubProcedure] = {
    Seq(new SPFocalSpot(machine, beamEnergyList, multileafCollimator))
  }

  /*
  private def allBeams(machine:Machine): Seq[Beam] = {
    val beamEnergyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)
    subProcedureList(machine, beamEnergyList).map(_.setBeamList())
  }
  */

  /**
   * Make a list of all possible beams for this machine for Phase3.
   *
   * @param machine For this machine.
   * @return List of all possible Phase3 beams.
   */
  private def makeBeamList(machine: Machine): Seq[Beam] = {
    val beamEnergyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)
    val multileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val subProcList = subProcedureList(machine, beamEnergyList, multileafCollimator )
    val beamList = subProcList.flatMap(subProc => subProc.getBeamList(prototypeBeams(machine)))
    beamList
  }

  // @formatter:off
  private def selectedBeamsField(machine: Machine) = {
    val list = makeBeamList(machine).map(_.beamName)
    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      valueMap => <span> {list.mkString("  ")} </span>)
  }
  // @formatter:on


  private def rowList(machine: Machine): List[WebRow] = {

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    def rowSelectedBeams: WebRow = List(selectedBeamsField(machine))

    val rowButton: WebRow = List(cancelButton, createPlanButton)

    List(
      rowCommonParameters,
      rowSelectedBeams,
      rowButton
    )
  }

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine): Unit = {
    val form = new WebForm(pathOf, rowList(machine))

    def getRealMachineId = {
      machine.getRealTpsId match {
        case Some(text) if text.trim.nonEmpty => text.trim
        case _ => ""
      }
    }

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  private def createPlan(valueMap: ValueMapT, response: Response, machine: Machine): Unit = {
    Trace.trace()
    ???
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.equals(button.label)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap = getValueMap(request)

    try {
      val user = CachedUser.get(request)
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach(): Unit =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        case _ if user.isEmpty => updateMach()
        case _ if machine.isEmpty => updateMach()
        case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()
        case _ if buttonIs(valueMap, createPlanButton) => createPlan(valueMap, response, machine.get)

        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }

  }
}
