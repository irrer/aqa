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
import org.aqa.db.MachineType
import org.restlet.Restlet
import org.restlet.data.Status
import org.restlet.Request
import org.restlet.Response

import scala.xml.Elem

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


    def beamsFromPlan(plan: AttributeList): Seq[Beam] = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).map(al => Beam.makeBeamFromAl(machine, al))


    val collimatorModel = MultileafCollimator.get(machine.multileafCollimatorPK).get.model

    def getPlan(name: String): Option[AttributeList] = {
      try {
        val plan = Config.PlanFileList.find(pf => pf.collimatorModel.equals(collimatorModel) && pf.procedure.equals(name)).get
        Some(DicomFile(plan.file).attributeList.get)
      }
      catch {
        case _: Throwable =>
          None
      }
    }

    val beamList = Seq("Phase3", "FocalSpot").flatMap(getPlan).flatMap(beamsFromPlan)
    beamList
  }

  private def makeSubProcedureList(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator): Seq[SubProcedure] = {
    Seq(new SPFocalSpot(machine, beamEnergyList, multileafCollimator, prototypeBeams(machine)))
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
  private def makeBeamList(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator): Seq[Beam] = {

    /**
     * Determine if the machine supports this energy.
     *
     * Must match all of: FFF, photonEnergy, and doseRate.
     *
     * @param beam For this beam.
     * @return True if the machine can deliver it.
     */
    def beamEnergyIsSupported(beam: Beam): Boolean = {
      val matching = beamEnergyList.find(e => {
        (e.isFFF == beam.beamEnergy.isFFF) &&
          // e.isFFF.toString.equals(beam.beamEnergy.isFFF.toString) &&
          (e.photonEnergy_MeV.get == beam.beamEnergy.photonEnergy_MeV.get) &&
          (e.machineBeamEnergyPK.get == beam.beamEnergy.machineBeamEnergyPK.get)
      })
      matching.isDefined
    }

    val subProcList = makeSubProcedureList(machine, beamEnergyList, multileafCollimator)
    val beamList = {
      val list = subProcList.flatMap(subProc => subProc.getBeamList)
      list.filter(beamEnergyIsSupported)
    }
    beamList
  }

  private def beamToHtml(beam: Beam, valueMap: ValueMapT, subProcedureList: Seq[SubProcedure]): Elem = {

    case class UsedBySubProc(subProc: SubProcedure, used: Boolean) {
      def toHtml: Elem = {
        if (used)
          <span style="border: 5px solid lightgreen;">
            {subProc.abbreviation}
          </span>
        else
          <span style="border: 5px solid white;">
            {subProc.abbreviation}
          </span>
      }
    }

    /** Return an indicator as to whether the sub procedure uses the beam. */
    def subProcUseOfBeam(subProc: SubProcedure): UsedBySubProc = {
      // val isUsed = subProc.selectionList.find(sel => valueMap.contains(sel.selectionName) && sel.beamList.exists(b => b.beamName.equals(beam.beamName))) // TODO put back
      val isUsed = if (beam.isFFF) Some(1) else None // TODO rm
      UsedBySubProc(subProc, isUsed.isDefined)
    }

    val beamSetUse = subProcedureList.map(subProcUseOfBeam)

    val subProcedureUses: Elem = {
      val usedBy = <div style="margin: 10px;">
        {beamSetUse.map(_.toHtml)}
      </div>
      usedBy
    }

    val image: Elem = {
      <span>
        {beam.beamName}
      </span>

    }

    <div>
      <table>
        <tr>
          <td>
            {subProcedureUses}
          </td>
          <td>
            {image}
          </td>
        </tr>
      </table>
    </div>
  }

  // @formatter:off
  private def selectedBeamsField(machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator, subProcList: Seq[SubProcedure]) = {
    val beamList = makeBeamList(machine, beamEnergyList, multileafCollimator)
    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      // valueMap => <span> {list.map(beamName => {<br>{beamName}</br>})} </span>)
      // _ => <span> {list.map(beamName => {<span style="margin-left: 32px;">{beamName}</span>})} </span>)
    valueMap => <span> {beamList.map(beam => {<span> {beamToHtml(beam, valueMap, subProcList)} </span>})} </span>)
  }
  // @formatter:on

  private def makeSubProcedureSelector(subProcList: Seq[SubProcedure], machine: Machine, beamEnergyList: Seq[MachineBeamEnergy], multileafCollimator: MultileafCollimator): List[WebRow] = {
    def makeSelectorHtml(subProc: SubProcedure): WebRow = {
      val list = subProc.selectionList.map(s => new WebInputCheckbox(label = s.selectionName, showLabel = true, col = 2, offset = 0))
      // @formatter:off
      val empty: Elem = { <span></span>}
      // @formatter:on
      val name = new WebPlainText(label = s"${subProc.name}:", showLabel = true, col = 1, offset = 0, _ => empty)
      (name +: list).toList
    }

    val checkBoxIdList: List[WebRow] = subProcList.map(makeSelectorHtml).toList
    checkBoxIdList
  }

  /** A list of beam energies that the machine supports. If any have an undefined dose rate, then fill it in. */
  private def rowList(machine: Machine): List[WebRow] = {
    val beamEnergyList = {
      val machineType = MachineType.get(machine.machineTypePK).get
      CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get).map(energy => CustomizeRtPlanUtil.resolveEnergy(energy, machineType))
    }

    val multileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get

    val subProcList = makeSubProcedureList(machine, beamEnergyList, multileafCollimator)

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    def rowSelectedBeams: WebRow = List(selectedBeamsField(machine, beamEnergyList, multileafCollimator, subProcList))

    def rowSelectSubProcedures: List[WebRow] = makeSubProcedureSelector(subProcList, machine, beamEnergyList, multileafCollimator)

    val rowButton: WebRow = List(cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(rowCommonParameters, rowSelectedBeams) ++ rowSelectSubProcedures ++ List(rowButton)
    rowList
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
