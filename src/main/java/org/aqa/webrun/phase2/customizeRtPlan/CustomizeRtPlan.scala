package org.aqa.webrun.phase2.customizeRtPlan

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.Config
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeList
import org.aqa.AnonymizeUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import org.aqa.VarianPrivateTag
import org.aqa.db.MultileafCollimator
import java.io.File
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.ValueRepresentation
import org.aqa.Util
import com.pixelmed.dicom.Attribute
import org.aqa.db.MachineBeamEnergy
import edu.umro.util.UMROGUID
import edu.umro.ScalaUtil.Trace

//import edu.umro.util.UMROGUID
//import java.io.File
//import edu.umro.ScalaUtil.Trace
//import com.pixelmed.dicom.SequenceAttribute
//import com.pixelmed.dicom.SequenceItem
//import org.aqa.VarianPrivateTag
//import com.pixelmed.dicom.AttributeFactory
//import com.pixelmed.dicom.LongStringAttribute
//import com.pixelmed.dicom.CodeStringAttribute
//import org.aqa.web.MachineUpdate
//import org.aqa.web.WebUtil
//import org.aqa.web.WebServer

object CustomizeRtPlan extends Logging {

  private val standardBeamEnergy = 6.0

  def getCollimatorCompatiblePlanForMachine(machine: Machine): Option[Config.Phase2PlanFileConfig] = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.Phase2PlanFileList.filter(pf => pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) && pf.model.equalsIgnoreCase(collimator.model)).headOption
    planFile
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

  /**
   * Given a planned beam's attribute list, get the energy it specifies.
   */
  private def getBeamEnergy(beamAl: AttributeList): Double = {
    DicomUtil.findAllSingle(beamAl, TagFromName.NominalBeamEnergy).head.getDoubleValues.head
  }

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
   * Given a planned beam's attribute list, determine if it is supported by the machine.
   */
  private def beamIsSupported(beamAl: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]) = {
    val beamEnergy = getBeamEnergy(beamAl)
    val fff = isFFFBeam(beamAl)
    machineEnergyList.find(me => (me.photonEnergy_MeV.get == beamEnergy) && (me.isFFF == fff)).isDefined
  }

  private def machineEnergyIsInPlan(rtplan: AttributeList, machineEnergy: MachineBeamEnergy): Boolean = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val energy = machineEnergy.photonEnergy_MeV.get
    val fff = machineEnergy.isFFF
    val isIn = beamAlList.filter(beamAl => (getBeamEnergy(beamAl) == energy) && (isFFFBeam(beamAl) == fff))
    isIn.nonEmpty
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

  private def removeBeamFromPlan(rtplan: AttributeList, beamName: String): Unit = {
    logger.info("Removing beam " + beamName)
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
      val removed = DicomUtil.removeSeq(rtplan, TagFromName.BeamSequence, (al: AttributeList) => beamNameOf(al).equals(beamName))

      val BeamNumber = removed.head.get(TagFromName.BeamNumber).getIntegerValues.head
      val PatientSetupNumber = removed.head.get(TagFromName.ReferencedPatientSetupNumber).getIntegerValues.head

      deleteFractSeq(BeamNumber)
      deletePatientSetup(PatientSetupNumber)
    }

    deleteBeamSeq
    logger.info("CustomizeRtPlan removed beam " + beamName)
  }

  /**
   * Remove beams from the plan that are not supported by this machine.  Also remove their ControlPoint counterparts.
   */
  private def removeUnsupportedBeams(rtplan: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val unsupported = beamAlList.filter(beamAl => !beamIsSupported(beamAl, machineEnergyList))
    val unsupportedNameList = unsupported.map(beamAl => beamNameOf(beamAl))
    unsupportedNameList.map(beamName => removeBeamFromPlan(rtplan, beamName))
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

  /**
   * Represent a plan beam.  This class is public only to support testing.
   */
  case class PlanBeam(energy: Double, name: String, fff: Boolean) {
    def fffAsText = { if (fff) " FFF" else "" }

    override def toString = {
      name + " : " + Util.fmtDbl(energy) + fffAsText
    }
  }

  case class PlanSpecification(
    toleranceTable: String,
    patientID: String,
    patientName: String,
    machineName: String);

  def getPlanBeamList(machine: Machine): List[PlanBeam] = {
    val plan = getCollimatorCompatiblePlanForMachine(machine)
    if (plan.isDefined) {
      val planAttrList = plan.get.dicomFile.attributeList.get

      def beamSeqToPlanBeam(beamAl: AttributeList): PlanBeam = {
        val name = beamNameOf(beamAl)
        val energy = DicomUtil.findAllSingle(beamAl, TagFromName.NominalBeamEnergy).head.getDoubleValues.head
        val fff = {
          val FluenceModeID = DicomUtil.findAllSingle(beamAl, TagFromName.FluenceModeID)
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

  /**
   * Get the name of the given beam specified by its attribute list.
   */
  private def beamNameOf(beamAl: AttributeList): String = {
    beamAl.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.trim
  }

  /**
   * Get the beam to be copied and modified to make non-standard beams.
   */
  private def getPrototypeBeam(rtplan: AttributeList): AttributeList = {
    val beamList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    beamList.filter(beamAl => beamNameOf(beamAl).equals(Config.PrototypeCustomBeamName)).head
  }

  /**
   * Get an unused beam number.
   */
  private def getAvailableBeamNumber(rtplan: AttributeList): Int = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val all = beamAlList.map(beamAl => beamAl.get(TagFromName.BeamNumber).getIntegerValues.head)
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
   * Add another entry with the given number to the PatientSetupSequence.
   */
  private def addPatientSetup(rtplan: AttributeList, PatientSetupNumber: Int): Unit = {
    val patientSetup = DicomUtil.clone(DicomUtil.seqToAttr(rtplan, TagFromName.PatientSetupSequence).head)
    val PatientSetupSequence = rtplan.get(TagFromName.PatientSetupSequence).asInstanceOf[SequenceAttribute]

    patientSetup.get(TagFromName.PatientSetupNumber).removeValues
    patientSetup.get(TagFromName.PatientSetupNumber).addValue(PatientSetupNumber)
    PatientSetupSequence.addItem(patientSetup)
  }

  private def insertBeam(rtplan: AttributeList, beamAl: AttributeList): Unit = {
    val beamList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)
    val index = beamList.indexWhere(b => beamNameOf(b).startsWith(Config.PrefixForMachineDependentBeamName))
    val seq = AttributeFactory.newAttribute(TagFromName.BeamSequence).asInstanceOf[SequenceAttribute]

    for (i <- 0 until beamList.size) {
      seq.addItem(beamList(i))
      if (i == index) seq.addItem(beamAl)
    }

    rtplan.put(seq)
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
      val fffText = if (machineEnergy.isFFF) "F" else "X"
      Config.PrefixForMachineDependentBeamName + numText + fffText
    }

    val BeamName = beamAl.get(TagFromName.BeamName)
    BeamName.removeValues
    BeamName.addValue(beamNameText)

    setFluence(beamAl, machineEnergy.isFFF)
    changeNominalBeamEnergy(beamAl, machineEnergy.photonEnergy_MeV.get)

    insertBeam(rtplan, beamAl)

    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagFromName.FractionGroupSequence).head
    val ReferencedBeamSequence = FractionGroupSequence.get(TagFromName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute]
    ReferencedBeamSequence.addItem(fraction)
    addPatientSetup(rtplan, BeamNumber)
  }

  private def showBeamList(rtplan: AttributeList) = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence)

    def showBeam(beamAl: AttributeList): String = {
      val name = beamNameOf(beamAl)
      val num = beamAl.get(TagFromName.BeamNumber).getIntegerValues.head

      val energy = getEnergy(beamAl)
      val fff = if (isFFFBeam(beamAl)) "F" else "X"
      val gantryAngleList = {
        val list = DicomUtil.findAllSingle(beamAl, TagFromName.GantryAngle).map(ga => ga.getDoubleValues).flatten
        list.map(ga => Util.fmtDbl(ga)).mkString("  ")
      }

      num.formatted("%3d") + "    " + "    " + name.formatted("%-16s") + "    " + energy.formatted("%5.1f") + "    " + fff + "    gantry angles: " + gantryAngleList
    }

    "Number of beams: " + beamAlList.size + "\n    " + beamAlList.map(beamAl => showBeam(beamAl)).mkString("\n    ")
  }

  private def reassignPlanEnergies(rtplan: AttributeList, planBeamList: Seq[PlanBeam], machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    // use this beam and its fraction reference to make non-standard beams
    val prototypeBeam = getPrototypeBeam(rtplan)

    logger.info("original rtplan\n" + showBeamList(rtplan))
    val prototypeFractionReference = getFractionReference(rtplan, prototypeBeam.get(TagFromName.BeamNumber).getIntegerValues.head)

    logger.info("machineEnergyList size: " + machineEnergyList.size + "\n    " + machineEnergyList.mkString("\n    "))
    removeUnsupportedBeams(rtplan, machineEnergyList)

    val unsupportedEnergyList = machineEnergyList.filter(me => !machineEnergyIsInPlan(rtplan, me))

    Trace.trace("unsupportedEnergyList size: " + unsupportedEnergyList.size + "\n    " + unsupportedEnergyList.map(me => me.toString).mkString("\n    "))

    unsupportedEnergyList.map(me => addBeam(rtplan, me, prototypeBeam, prototypeFractionReference))
    logger.info("customized rtplan\n" + showBeamList(rtplan))

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
  def makePlan(machine: Machine, planBeamList: Seq[PlanBeam], planSpecification: PlanSpecification, machineEnergyList: Seq[MachineBeamEnergy]): AttributeList = {

    val rtplan = DicomUtil.clone(getCollimatorCompatiblePlanForMachine(machine).get.dicomFile.attributeList.get)
    replaceAllUids(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, planSpecification.toleranceTable)

    val overrideList = Config.Phase2PlanAttributeOverrideList ++ Seq(
      new Config.PlanAttributeOverride(TagFromName.ToleranceTableLabel, planSpecification.toleranceTable),
      new Config.PlanAttributeOverride(TagFromName.PatientID, planSpecification.patientID),
      new Config.PlanAttributeOverride(TagFromName.PatientName, planSpecification.patientName),
      new Config.PlanAttributeOverride(TagFromName.TreatmentMachineName, planSpecification.machineName))

    // modify all attributes that get a constant value
    overrideList.map(ov => {
      DicomUtil.findAllSingle(rtplan, ov.tag).map(at => { at.removeValues; at.addValue(ov.value) })
    })

    removeVarianPrivateTagAttributes(rtplan)
    reassignPlanEnergies(rtplan, planBeamList, machineEnergyList)
    saveAnonymizedDicom(machine.institutionPK, rtplan)
    rtplan
  }

}
