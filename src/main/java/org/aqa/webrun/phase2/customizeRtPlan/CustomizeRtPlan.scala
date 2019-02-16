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

object CustomizeRtPlan {
  def reference(machinePK: Long) = { (new CustomizeRtPlan).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)

  private val standardBeamEnergy = 6.0
}

/**
 * Generate a DICOM RTPLAN file customized for the user's environment.
 */
class CustomizeRtPlan extends Restlet with SubUrlRoot with Logging {

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
    <h3 title="Plan customization for machine">Create Custom RT Plan Comptatible with Machine { linkToMachineUpdate(valueMap) }</h3>
  }

  val machineId = new WebPlainText(machineIdTag, false, 6, 0, machineIdHtml)

  private def toleranceTable = new WebInputText("Tolerance Table Name", true, 3, 0, "Should match name in planning system", false)

  private def machineName = new WebInputText("Machine Name", true, 2, 0, "To match planning system", false)

  private def patientID = new WebInputText("Patient ID", true, 2, 0, "")

  private def patientName = new WebInputText("Patient Name", true, 3, 0, "")

  private val row0: WebRow = List(machineId)
  private val row2: WebRow = List(toleranceTable, machineName)
  private val row1: WebRow = List(patientID, patientName)

  private def getCollimatorCompatiblePlanForMachine(machine: Machine): Option[Config.Phase2PlanFileConfig] = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.Phase2PlanFileList.filter(pf => pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) && pf.model.equalsIgnoreCase(collimator.model)).headOption
    planFile
  }

  /**
   * Represent a plan beam.  This class is public only to support testing.
   */
  case class PlanBeam(energy: Double, name: String, fff: Boolean) {
    def fffAsText = { if (fff) " FFF" else "" }

    override def toString = {
      name + " : " + Util.fmtDbl(energy) + fffAsText
    }
  }

  private def getPlanBeamList(machine: Machine): List[PlanBeam] = {
    val plan = getCollimatorCompatiblePlanForMachine(machine)
    if (plan.isDefined) {
      val planAttrList = plan.get.dicomFile.attributeList.get

      def beamSeqToPlanBeam(beamSeq: AttributeList): PlanBeam = {
        val name = beamSeq.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.trim
        val energy = DicomUtil.findAllSingle(beamSeq, TagFromName.NominalBeamEnergy).head.getDoubleValues.head
        val fff = {
          val FluenceModeID = DicomUtil.findAllSingle(beamSeq, TagFromName.FluenceModeID)
          if (FluenceModeID.isEmpty) false
          else FluenceModeID.map(fmi => fmi.getSingleStringValueOrEmptyString.toUpperCase.contains("FFF")).reduce(_ || _)
        }
        new PlanBeam(energy, name, fff)
      }

      val attrList = DicomUtil.findAllSingle(planAttrList, TagFromName.NominalBeamEnergy)
      val list = attrList.map(a => a.getDoubleValues.head).distinct.sorted

      val planBeamList = DicomUtil.seqToAttr(planAttrList, TagFromName.BeamSequence).map(beamSeq => beamSeqToPlanBeam(beamSeq)).sortBy(_.energy)

      logger.info("Energy list found in plan for machine " + machine.id + " :\n    " + planBeamList.mkString("\n    "))
      planBeamList.toList
    } else List[PlanBeam]()

  }

  /**
   * For testing only
   */
  def testGetPlanBeamList(machine: Machine): Seq[PlanBeam] = {
    getPlanBeamList(machine)
  }

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
    val collimatorErr = if (getCollimatorCompatiblePlanForMachine(machine.get).isEmpty) Error.make(createButton, "There is no pre-defined plan to support this machine's collimator.") else styleNone

    (tolErr ++ machErr ++ patIdErr ++ patNameErr ++ collimatorErr)
  }

  private def replaceAllUids(attributeList: AttributeList) = {
    // get list of attributes that are UIDs
    val uidSet = Config.ToBeAnonymizedList.keySet.filter(tag => ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
    val attrList = DicomUtil.findAll(attributeList, uidSet)
    val replaceMap = attrList.map(at => at.getSingleStringValueOrEmptyString).distinct.map(uid => (uid, UMROGUID.getUID)).toMap
    def replace(at: Attribute) = {
      val uid = replaceMap(at.getSingleStringValueOrEmptyString)
      at.removeValues
      at.addValue(uid)
    }
    attrList.map(at => replace(at))
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

  /**
   * Save an anonymized version of this file, so when the user uploads RTIMAGEs generated by
   * this plan, the plan will already be uploaded.
   */
  private def saveAnonymizedDicom(institutionPK: Long, rtplan: AttributeList) = {
    val anon = AnonymizeUtil.anonymizeDicom(institutionPK, rtplan)
    val file = new File(Config.sharedDir, Util.sopOfAl(anon) + ".dcm")
    DicomUtil.writeAttributeList(anon, file)
  }

  private def deleteBeamFromPlan(rtplan: AttributeList, beamName: String): Unit = {

    def deleteFractSeq(BeamNumber: Int): Unit = {
      val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagFromName.FractionGroupSequence).head
      val ReferencedBeamSequence = DicomUtil.seqToAttr(FractionGroupSequence, TagFromName.ReferencedBeamSequence)
      DicomUtil.removeSeq(FractionGroupSequence, TagFromName.ReferencedBeamSequence, (al: AttributeList) => al.get(TagFromName.ReferencedBeamNumber).getIntegerValues.head == BeamNumber)
    }

    def deletePatientSetup(PatientSetupNumber: Int): Unit = {
      DicomUtil.removeSeq(rtplan, TagFromName.PatientSetupSequence, (al: AttributeList) => al.get(TagFromName.PatientSetupNumber).getIntegerValues.head == PatientSetupNumber)
    }

    // remove the beam from the BeamSequence
    def deleteBeamSeq: Unit = {
      val removed = DicomUtil.removeSeq(rtplan, TagFromName.BeamSequence, (al: AttributeList) => al.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(beamName))

      val BeamNumber = removed.head.get(TagFromName.BeamNumber).getIntegerValues.head
      val PatientSetupNumber = removed.head.get(TagFromName.ReferencedPatientSetupNumber).getIntegerValues.head

      deleteFractSeq(BeamNumber)
      deletePatientSetup(PatientSetupNumber)
    }

    deleteBeamSeq
    logger.info("CustomizeRtPlan removed beam " + beamName)
  }

  /**
   * Ensure that the number of beams in the FractionGroupSequence is correct.
   */
  private def setNumberOfBeamsInFractionGroupSequence(rtplan: AttributeList): Unit = {
    val noOfBeams = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence).size
    def setNumberOfBeams(al: AttributeList) = {
      val NumberOfBeams = al.get(TagFromName.NumberOfBeams)
      NumberOfBeams.removeValues
      NumberOfBeams.addValue(noOfBeams)
    }
    DicomUtil.seqToAttr(rtplan, TagFromName.FractionGroupSequence).map(al => setNumberOfBeams(al))
  }

  private def changeNominalBeamEnergy(beamAl: AttributeList, energy: Double): Unit = {
    val controlPtSeq = DicomUtil.seqToAttr(beamAl, TagFromName.ControlPointSequence)

    def changeOne(cpt: AttributeList): Unit = {
      val NominalBeamEnergy = cpt.get(TagFromName.NominalBeamEnergy)

      if (NominalBeamEnergy != null) {
        NominalBeamEnergy.removeValues
        NominalBeamEnergy.addValue(energy)
      }
    }

    controlPtSeq.map(cpt => changeOne(cpt))
  }

  /**
   * Get the energy of a beam.
   */
  private def getEnergy(beamAl: AttributeList): Double = {

    def cpsEnergy(cpal: AttributeList): Option[Double] = {
      val NominalBeamEnergy = cpal.get(TagFromName.NominalBeamEnergy)
      if (NominalBeamEnergy == null) None
      else Some(NominalBeamEnergy.getDoubleValues.head)
    }

    val cps = DicomUtil.seqToAttr(beamAl, TagFromName.ControlPointSequence).map(cpal => cpsEnergy(cpal)).flatten.max
    cps
  }

  private def getFractionReference(rtplan: AttributeList, BeamNumber: Int): AttributeList = {
    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagFromName.FractionGroupSequence)
    val ReferencedBeamSequence = FractionGroupSequence.map(fractAl => DicomUtil.seqToAttr(fractAl, TagFromName.ReferencedBeamSequence)).flatten

    def beamNumberMatches(fractAl: AttributeList): Boolean = {
      val ReferencedBeamNumber = fractAl.get(TagFromName.ReferencedBeamNumber)
      (ReferencedBeamNumber != null) && (ReferencedBeamNumber.getIntegerValues.head == BeamNumber)
    }

    ReferencedBeamSequence.filter(fractAl => beamNumberMatches(fractAl)).head
  }

  private case class BeamReference(beam: AttributeList, fractionReference: AttributeList);

  /**
   * Determine if this is an FFF beam.
   */
  private def isFFFBeam(beamAl: AttributeList): Boolean = {
    val PrimaryFluenceModeSequence = DicomUtil.seqToAttr(beamAl, TagFromName.PrimaryFluenceModeSequence)
    def isFFF(pfms: AttributeList): Boolean = {
      val FluenceModeID = pfms.get(TagFromName.FluenceModeID)
      (FluenceModeID != null) && FluenceModeID.getSingleStringValueOrEmptyString.toLowerCase.contains("fff")
    }
    val fff = PrimaryFluenceModeSequence.map(pfms => isFFF(pfms)).reduce(_ || _)
    fff
  }

  /**
   * Determine if the beam is non-standard
   */
  private def isNonStandard(beamAl: AttributeList): Boolean = {
    beamAl.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.startsWith(Config.PrefixForMachineDependentBeamName) || isFFFBeam(beamAl)
  }

  /**
   * Get the beam to be copied and modified to make non-standard beams.  It is chosen by selecting the
   * beams names starting with <code>Config.PrefixForMachineDependentBeamName</code> and of those selecting
   * the one with the highest energy.
   */
  private def getPrototypeNonStandardBeam(rtplan: AttributeList): AttributeList = {
    val beamList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val proto = beamList.filter(b => isNonStandard(b)).maxBy(getEnergy _)
    proto
  }

  /**
   * Remove beams from the plan that do not have the standard energy level.  Also remove their ControlPoint counterparts.
   */
  private def removeNonStandardBeams(rtplan: AttributeList): Unit = {
    val beamNameList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence).filter(beamAl => isNonStandard(beamAl)).map(beamAl => beamAl.get(TagFromName.BeamName).getSingleStringValueOrEmptyString)
    beamNameList.map(beamName => deleteBeamFromPlan(rtplan, beamName))
  }

  /**
   * Get an unused beam number.
   */
  private def getAvailableBeamNumber(rtplan: AttributeList): Int = {
    val all = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence).map(beamAl => beamAl.get(TagFromName.BeamNumber).getIntegerValues.head)
    val available = (1 to (all.max + 1)).find(i => !(all.contains(i)))
    available.get
  }

  private def setFluence(beamAl: AttributeList, fff: Boolean) {

    /**
     * Remove both the DICOM standard and Varian fluence references.
     */
    def removeFluence = {
      val PrimaryFluenceModeSequence = beamAl.get(TagFromName.PrimaryFluenceModeSequence)
      if (PrimaryFluenceModeSequence != null) beamAl.remove(TagFromName.PrimaryFluenceModeSequence)

      val VarianPrimaryFluenceModeSequence = beamAl.get(VarianPrivateTag.PrimaryFluenceModeSequence)
      if (VarianPrimaryFluenceModeSequence != null) beamAl.remove(VarianPrivateTag.PrimaryFluenceModeSequence)
    }

    /**
     * Add a sequence to the current beam.
     */
    def addSequence(tag: AttributeTag): AttributeList = {
      val seq = new SequenceAttribute(tag)
      beamAl.put(seq)
      val al = new AttributeList
      seq.addItem(al)
      al
    }

    def addFluenceMode(al: AttributeList, value: String) = {
      val FluenceMode = AttributeFactory.newAttribute(TagFromName.FluenceMode)
      FluenceMode.addValue(value)
      al.put(FluenceMode)
    }

    def addFluenceModeID(al: AttributeList, value: String) = {
      val FluenceModeID = AttributeFactory.newAttribute(TagFromName.FluenceModeID)
      FluenceModeID.addValue(value)
      al.put(FluenceModeID)
    }

    def addVarianCreator3285(al: AttributeList) = al.put(VarianPrivateTag.newVarianCreator3285("Varian Medical Systems VISION 3285"))
    def addVarianFluenceMode(al: AttributeList, mode: String) = al.put(VarianPrivateTag.newFluenceMode(mode))
    def addVarianFluenceModeID(al: AttributeList, mode: String) = al.put(VarianPrivateTag.newFluenceModeID(mode))

    def changeToFluenceStandard = {
      removeFluence
      // standard attributes
      val al = addSequence(TagFromName.PrimaryFluenceModeSequence)
      addFluenceMode(al, "STANDARD")

      // Varian attributes
      val alv = addSequence(VarianPrivateTag.PrimaryFluenceModeSequence)
      addVarianCreator3285(alv)
      addVarianFluenceMode(alv, "STANDARD")
    }

    def changeToFluenceFFF = {
      removeFluence
      // standard attributes
      val al = addSequence(TagFromName.PrimaryFluenceModeSequence)
      addFluenceMode(al, "NON_STANDARD")
      addFluenceModeID(al, "FFF")

      // Varian attributes
      val alv = addSequence(VarianPrivateTag.PrimaryFluenceModeSequence)
      addVarianCreator3285(alv)
      addVarianFluenceMode(alv, "NON_STANDARD")
      addVarianFluenceModeID(alv, "FFF")
    }

    (isFFFBeam(beamAl), fff) match {
      case (true, true) => ; // no change necessary
      case (false, false) => ; // no change necessary
      case (true, false) => changeToFluenceStandard
      case (false, true) => changeToFluenceFFF
    }
  }

  /**
   * Add another entry with the given number to the PatientSetupSequence.
   */
  private def addPatientSetup(rtplan: AttributeList, PatientSetupNumber: Int): Unit = {
    val patientSetup = DicomUtil.clone(DicomUtil.seqToAttr(rtplan, TagFromName.PatientSetupSequence).head)
    val PatientSetupSequence = rtplan.get(TagFromName.PatientSetupSequence).asInstanceOf[SequenceAttribute]

    patientSetup.get(TagFromName.PatientSetupNumber).removeValues
    patientSetup.get(TagFromName.PatientSetupNumber).addValue(PatientSetupNumber)
    PatientSetupSequence.addItem(patientSetup)
  }

  /**
   * Add a beam that supports the given machine energy.  Do it by copying and modifying both prototypes.  The original prototypes are
   * not changed, but rtplan is changed.
   */
  private def addBeam(rtplan: AttributeList, machineEnergy: MachineBeamEnergy, prototypeBeam: AttributeList, prototypeFractionReference: AttributeList): Unit = {
    val BeamNumber = getAvailableBeamNumber(rtplan)
    val beamAl = DicomUtil.clone(prototypeBeam)
    val fraction = DicomUtil.clone(prototypeFractionReference)

    // modify the fraction
    val ReferencedBeamNumber = fraction.get(TagFromName.ReferencedBeamNumber)
    ReferencedBeamNumber.removeValues
    ReferencedBeamNumber.addValue(BeamNumber)

    // modify the beam

    val beamNum = beamAl.get(TagFromName.BeamNumber)
    beamNum.removeValues
    beamNum.addValue(BeamNumber)

    val refPatSetupNumber = beamAl.get(TagFromName.ReferencedPatientSetupNumber)
    refPatSetupNumber.removeValues
    refPatSetupNumber.addValue(BeamNumber)

    val beamNameText = {
      val energy = machineEnergy.photonEnergy_MeV.get
      val numText = if (energy.round == energy) energy.round.toLong.toString else Util.fmtDbl(energy)
      val fffText = if (machineEnergy.isFFF) "F" else ""
      Config.PrefixForMachineDependentBeamName + numText + fffText
    }

    val BeamName = beamAl.get(TagFromName.BeamName)
    BeamName.removeValues
    BeamName.addValue(beamNameText)

    setFluence(beamAl, machineEnergy.isFFF)
    changeNominalBeamEnergy(beamAl, machineEnergy.photonEnergy_MeV.get)

    val beamSeq = rtplan.get(TagFromName.BeamSequence).asInstanceOf[SequenceAttribute]
    beamSeq.addItem(beamAl)

    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagFromName.FractionGroupSequence).head
    val ReferencedBeamSequence = FractionGroupSequence.get(TagFromName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute]
    ReferencedBeamSequence.addItem(fraction)
    addPatientSetup(rtplan, BeamNumber)
  }

  private def reassignPlanEnergies(rtplan: AttributeList, valueMap: ValueMapT, planBeamList: Seq[PlanBeam], machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    // use this beam and its fraction reference to make non-standard beams
    val prototypeBeam = getPrototypeNonStandardBeam(rtplan)

    val prototypeFractionReference = getFractionReference(rtplan, prototypeBeam.get(TagFromName.BeamNumber).getIntegerValues.head)

    removeNonStandardBeams(rtplan)

    val supportedNonStandardEnergyList = machineEnergyList.filter(me => me.photonEnergy_MeV.get != CustomizeRtPlan.standardBeamEnergy)

    supportedNonStandardEnergyList.map(me => addBeam(rtplan, me, prototypeBeam, prototypeFractionReference))

    setNumberOfBeamsInFractionGroupSequence(rtplan)
  }

  /**
   * There are some Varian private tags that are in a proprietary format that is not possible to
   * duplicate.  The tags are not necessary for treatment so they are deleted so that they do not
   * invalidate the plan.
   */
  private def removeVarianPrivateTagAttributes(rtplan: AttributeList): Unit = {

    val privateTagList = Seq(
      VarianPrivateTag.VarianCreator3253,
      VarianPrivateTag.ExtendedInterfaceData,
      VarianPrivateTag.ExtendedInterfaceLength,
      VarianPrivateTag.ExtendedInterfaceFormat,
      VarianPrivateTag.VarianCreator3287,
      VarianPrivateTag.PlanIntegritySequence)

    privateTagList.map(tag => rtplan.remove(tag))
  }

  /**
   * Given all the required information, create a plan that is compatible with the given machine.
   */
  private def makePlan(valueMap: ValueMapT, response: Response, machine: Machine, planBeamList: Seq[PlanBeam], machineEnergyList: Seq[MachineBeamEnergy]) = {

    val rtplan = DicomUtil.clone(getCollimatorCompatiblePlanForMachine(machine).get.dicomFile.attributeList.get)
    replaceAllUids(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, valueMap(toleranceTable.label))

    val overrideList = Config.Phase2PlanAttributeOverrideList ++ Seq(
      new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, valueMap(toleranceTable.label)),
      new Config.PlanAttributeOverride(TagFromName.PatientID, valueMap(patientID.label).trim),
      new Config.PlanAttributeOverride(TagFromName.PatientName, valueMap(patientName.label).trim),
      new Config.PlanAttributeOverride(TagFromName.TreatmentMachineName, valueMap(machineName.label).trim))

    // modify all attributes that get a constant value
    overrideList.map(ov => {
      DicomUtil.findAllSingle(rtplan, ov.tag).map(at => { at.removeValues; at.addValue(ov.value) })
    })

    removeVarianPrivateTagAttributes(rtplan)
    reassignPlanEnergies(rtplan, valueMap, planBeamList, machineEnergyList)
    saveAnonymizedDicom(machine.institutionPK, rtplan)
    showDownload(rtplan, valueMap, machine, response)
  }

  private def createPlan(valueMap: ValueMapT, response: Response) = {
    val styleMap = validate(valueMap)
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get
    val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
    val planEnergyList = getPlanBeamList(machine).toList
    if (styleMap.nonEmpty) {
      val form = new WebForm(pathOf, List(row0, row1, row2) ++ List(assignButtonList))
      form.setFormResponse(valueMap, styleMap, pageTitleSelect, response, Status.SUCCESS_OK)
    } else {
      makePlan(valueMap, response, machine, planEnergyList, machineEnergyList)
    }
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
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
        case _ if buttonIs(valueMap, createButton) => createPlan(valueMap, response)
        case _ => formSelect(valueMap, response, machine.get) // first time viewing the form.  Set defaults
      }
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }
}
