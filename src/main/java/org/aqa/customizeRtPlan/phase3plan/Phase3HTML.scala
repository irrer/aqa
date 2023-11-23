package org.aqa.customizeRtPlan.phase3plan

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.Logging
import org.aqa.customizeRtPlan.CustomizeRtPlanInterface
import org.aqa.db.Machine
import org.aqa.db.MultileafCollimator
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.Config
import org.aqa.DicomFile
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


  private case class UsedBySubProc(subProc: SubProcedure, used: Boolean) {
    def toHtml: Elem = {
      if (used)
        <span style="border: 5px solid lightgreen;" title={subProc.name}>
          {subProc.abbreviation}
        </span>
      else
        <span style="border: 5px solid white;" title={subProc.name}>
          {subProc.abbreviation}
        </span>
    }
  }


  /** Return an indicator as to whether the sub procedure uses the beam. */
  private def subProcUseOfBeam(subProc: SubProcedure, beam: Beam, valueMap: ValueMapT): Elem = {
    val color =
    //if (subProc.metaData.subProcedureUsesBeam(beam, subProc, valueMap)) // TODO put back
      if (beam.isFFF) // TODO rm
        "lightgreen"
      else
        "white"

    val id = s"${subProc.name} :: ${beam.beamName}"
    <span id={id} style={s"border: 5px solid $color;border-radius:40px; margin:5px;"} title={subProc.name}>
      <span style="margin:6px;">
        {subProc.abbreviation}
      </span>
    </span>

  }


  private def beamToHtml(beam: Beam, valueMap: ValueMapT, metaData: SPMetaData): Elem = {

    val beamSetUse = metaData.subProcedureList.map(sub => subProcUseOfBeam(sub, beam, valueMap))

    val subProcedureUses: Elem = {
      val usedBy = <td style="margin: 5px;">
        {beamSetUse}
      </td>
      usedBy
    }

    val image: Elem = {
      val imageUrl = metaData.urlOfExampleImage(beam)
      <div style="margin: 5px;">
        <img src={imageUrl} height="100" style="margin: 2px;"/>
        <br/>{beam.beamName}
      </div>
    }

    <div style="border:1px solid lightgrey;margin: 2px;">
      <table>
        <tr>
          <td>
            {image}
          </td>
          <td>
            {subProcedureUses}
          </td>
        </tr>
      </table>
    </div>
  }

  // @formatter:off
  private def selectedBeamsField(metaData: SPMetaData) = {
    new WebPlainText(
      label = "Selected Beams", showLabel = false,
      col = 0, offset = 0,
      // valueMap => <span> {list.map(beamName => {<br>{beamName}</br>})} </span>)
      // _ => <span> {list.map(beamName => {<span style="margin-left: 32px;">{beamName}</span>})} </span>)
    valueMap => <div> {metaData.prototypeBeamList.map(beam => {<div class="col-md-3"> {beamToHtml(beam, valueMap, metaData)} </div>})} </div>)
  }
  // @formatter:on

  private def makeSubProcedureSelector(metaData: SPMetaData): List[WebRow] = {
    def makeSelectorHtml(subProc: SubProcedure): WebRow = {
      val list = subProc.selectionList.map(s => new WebInputCheckbox(label = s.selectionName, showLabel = true, col = 2, offset = 0))
      // @formatter:off
      val empty: Elem = { <span></span>}
      // @formatter:on
      val name = new WebPlainText(label = s"${subProc.name}:", showLabel = true, col = 12, offset = 0, _ => empty)
      (name +: list).toList
    }

    val checkBoxIdList: List[WebRow] = metaData.subProcedureList.map(makeSelectorHtml).toList
    checkBoxIdList
  }

  /** A list of beam energies that the machine supports. If any have an undefined dose rate, then fill it in. */
  private def rowList(metaData: SPMetaData): List[WebRow] = {
    /*
    val beamEnergyList = {
      val machineType = MachineType.get(machine.machineTypePK).get
      CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get).map(energy => CustomizeRtPlanUtil.resolveEnergy(energy, machineType))
    }

    val multileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get

    val epid = EPID.get(machine.epidPK).get
    val exampleImageFileList = Util.listDirFiles(exampleBeamImagesDir(epid))

    val subProcList = makeSubProcedureList(metaData)

    */

    def rowSelectSubProcedures: List[WebRow] = makeSubProcedureSelector(metaData)

    def rowSelectedBeams: WebRow = List(selectedBeamsField(metaData))

    // all hidden fields inherited from and specified in the custom plan interface.
    val rowCommonParameters: WebRow = List(machinePK, patientID, patientName, machineName, planName, toleranceTableName)

    val rowButton: WebRow = List(cancelButton, createPlanButton)

    val rowList: List[WebRow] = List(rowCommonParameters) ++ rowSelectSubProcedures ++ List(rowSelectedBeams) ++ List(rowButton)
    rowList
  }

  /**
   * Make a form to present to the user.
   *
   * @param valueMap List of HTML field values.
   * @param response HTML response.
   * @param metaData Related information.
   */
  private def formSelect(valueMap: ValueMapT, response: Response, metaData: SPMetaData): Unit = {
    val form = new WebForm(pathOf, rowList(metaData))

    def getRealMachineId = {
      metaData.machine.getRealTpsId match {
        case Some(text) if text.trim.nonEmpty => text.trim
        case _ => ""
      }
    }

    form.setFormResponse(valueMap, styleNone, pageTitle, response, Status.SUCCESS_OK)
  }

  private def createPlan(valueMap: ValueMapT, response: Response, metaData: SPMetaData): Unit = {
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
      val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong)

      def updateMach(): Unit =
        MachineUpdate.redirect(valueMap(machinePK.label).toLong, response)

      0 match {
        // case _ if CachedUser.get(request).isEmpty => updateMach()
        // case _ if machine.isEmpty => updateMach()
        // case _ if (user.get.institutionPK != machine.get.institutionPK) && (!WebUtil.userIsWhitelisted(request)) => updateMach()
        case _ if buttonIs(valueMap, cancelButton) => updateMach()
        case _ if buttonIs(valueMap, createPlanButton) =>
          val metaData = SPMetaData(machine.get)
          createPlan(valueMap, response, metaData)

        case _ => // first time viewing the form.  Set defaults
          val metaData = SPMetaData(machine.get)
          formSelect(valueMap, response, metaData)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
