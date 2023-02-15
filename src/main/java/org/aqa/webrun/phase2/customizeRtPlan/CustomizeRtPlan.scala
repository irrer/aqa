/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2.customizeRtPlan

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.VarianPrivateTag
import org.aqa.db.DicomAnonymous
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.MachineBeamEnergy
import org.aqa.db.MultileafCollimator
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure

object CustomizeRtPlan extends Logging {

  private val patternPhase2 = ".*phase *2.*"
  private val patternPhase3 = ".*phase *3.*"
  private val patternDailyQA = ".*daily.*qa.*"
  private val patternGapSkew = ".*gap.*skew.*"
  private val patternLOC = ".*loc.*"
  private val patternLOCBaseline = ".*baseline.*"
  private val patternLOCDelivery = ".*delivery.*"

  case class LOCRtplanPair(baseline: Config.PlanFileConfig, delivery: Config.PlanFileConfig) {
    def asSeq: Seq[AttributeList] = Seq(baseline.dicomFile.attributeList.get, delivery.dicomFile.attributeList.get)
    def baselineAl: AttributeList = baseline.dicomFile.attributeList.get
    def deliveryAl: AttributeList = delivery.dicomFile.attributeList.get
  }

  /**
    * Get the template rtplan for the given machine matching the given pattern.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  private def getCollimatorCompatiblePlanForMachine(machine: Machine, pattern: String): Seq[Config.PlanFileConfig] = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFile = Config.PlanFileList.filter(pf =>
      pf.procedure.toLowerCase.matches(pattern) &&
        pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) &&
        pf.collimatorModel.equalsIgnoreCase(collimator.model)
    )
    planFile
  }

  /**
    * Get the template rtplan for the given machine for Phase 2.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatiblePhase2PlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternPhase2).headOption
  }

  /**
    * Get the template rtplan for the given machine for Phase 3.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatiblePhase3PlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternPhase3).headOption
  }

  /**
    * Get the template rtplan for the given machine for Daily QA.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleDailyQAPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternDailyQA).headOption
  }

  /**
    * Get the template rtplan for the given machine for Gap Skew.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    *
    * Note that both the baseline and delivery entries have to be configured (regardless of RTPLAN file
    * existence) or this will return None.
    */
  def getCollimatorCompatibleGapSkewPlanForMachine(machine: Machine): Option[Config.PlanFileConfig] = {
    getCollimatorCompatiblePlanForMachine(machine, patternGapSkew).headOption
  }

  /**
    * Get the template rtplans for the given machine for LOC.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  def getCollimatorCompatibleLocPlanPairForMachine(machine: Machine): Option[LOCRtplanPair] = {
    val planList = getCollimatorCompatiblePlanForMachine(machine, patternLOC) // gets both baseline and delivery files

    val baseline = planList.find(pf => pf.procedure.toLowerCase.matches(patternLOCBaseline))
    val delivery = planList.find(pf => pf.procedure.toLowerCase.matches(patternLOCDelivery))
    val locRtplanPair = (baseline, delivery) match {
      case (Some(b), Some(d)) if b.dicomFile.attributeList.isDefined && d.dicomFile.attributeList.isDefined => Some(LOCRtplanPair(b, d))
      case _                                                                                                => None
    }
    locRtplanPair
  }

  /**
    * Save an anonymized version of this file, so when the user uploads RTIMAGES generated by
    * this plan, the plan will already be uploaded.
    */
  private def saveAnonymizedDicom(machine: Machine, userPK: Long, rtplan: AttributeList, procedurePK: Option[Long]): AttributeList = {
    val anon = AnonymizeUtil.anonymizeDicom(machine.institutionPK, rtplan)

    DicomSeries.makeDicomSeries(userPK, None, machine.machinePK, Seq(rtplan), procedurePK) match {
      case Some(dicomSeries) => dicomSeries.insert
      case _                 => logger.warn("Unable to create DicomSeries for machine " + machine + " userPK: " + userPK)
    }

    anon
  }

  /**
    * Replace all UIDs so that this RTPLAN will be new and unique.
    *
    * @param attributeList Replace all in this list.
    */
  private def replaceAllUIDs(attributeList: AttributeList): Unit = {
    // get list of attributes that are UIDs
    val uidSet = Config.ToBeAnonymizedList.keySet.filter(tag => ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
    val attrList = DicomUtil.findAll(attributeList, uidSet)
    val replaceMap = attrList.map(at => at.getSingleStringValueOrEmptyString).distinct.map(uid => (uid, UMROGUID.getUID)).toMap
    def replace(at: Attribute): Unit = {
      val uid = replaceMap(at.getSingleStringValueOrEmptyString)
      at.removeValues()
      at.addValue(uid)
    }
    attrList.foreach(at => replace(at))
  }

  /**
    * Given a planned beam's attribute list, get the energy it specifies.
    */
  private def getBeamEnergy(beamAl: AttributeList): Double = {
    DicomUtil.findAllSingle(beamAl, TagByName.NominalBeamEnergy).head.getDoubleValues.head
  }

  /**
    * Determine if this is an FFF beam.
    *
    * @param beamAl Represents one member of 300a,00b0 BeamSequence list.
    *
    * @return True if this is an FFF beam.
    */
  private def isFFFBeam(beamAl: AttributeList): Boolean = {
    val PrimaryFluenceModeSequence = DicomUtil.seqToAttr(beamAl, TagByName.PrimaryFluenceModeSequence)
    def isFFF(pfmSeq: AttributeList): Boolean = {
      val FluenceModeID = pfmSeq.get(TagByName.FluenceModeID)
      (FluenceModeID != null) && FluenceModeID.getSingleStringValueOrEmptyString.toLowerCase.contains("fff")
    }
    val fff = PrimaryFluenceModeSequence.map(pfmSeq => isFFF(pfmSeq)).reduce(_ || _)
    fff
  }

  /**
    * Given a planned beam's attribute list, determine if it is supported by the machine.
    */
  private def beamIsSupported(beamAl: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]) = {
    val beamEnergy = getBeamEnergy(beamAl)
    val fff = isFFFBeam(beamAl)
    machineEnergyList.exists(me => (me.photonEnergy_MeV.get == beamEnergy) && (me.isFFF == fff))
  }

  private def machineEnergyIsInPlan(rtplan: AttributeList, machineEnergy: MachineBeamEnergy): Boolean = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val energy = machineEnergy.photonEnergy_MeV.get
    val fff = machineEnergy.isFFF
    val isIn = beamAlList.filter(beamAl => (getBeamEnergy(beamAl) == energy) && (isFFFBeam(beamAl) == fff))
    isIn.nonEmpty
  }

  /**
    * Ensure that the number of beams in the FractionGroupSequence is correct.
    */
  private def setNumberOfBeamsInFractionGroupSequence(rtplan: AttributeList): Unit = {
    val noOfBeams = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence).size
    def setNumberOfBeams(al: AttributeList): Unit = {
      val NumberOfBeams = al.get(TagByName.NumberOfBeams)
      NumberOfBeams.removeValues()
      NumberOfBeams.addValue(noOfBeams)
    }
    DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).foreach(al => setNumberOfBeams(al))
  }

  /**
    * Delete the given beam and all references.  This includes:
    * <ul>
    * <li>BeamSequence</li>
    * <li>ReferencedBeamSequence</li>
    * <li>PatientSetupSequence</li>
    * </ul>
    *
    * @param rtplan Remove from this plan.
    * @param beamName The name of the beam.
    */
  private def removeBeamFromPlan(rtplan: AttributeList, beamName: String): Unit = {
    logger.info("Removing beam " + beamName)
    def deleteFractionSeq(BeamNumber: Int): Unit = {
      val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).head
      DicomUtil.removeSeq(FractionGroupSequence, TagByName.ReferencedBeamSequence, (al: AttributeList) => al.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == BeamNumber)
    }

    def deletePatientSetup(PatientSetupNumber: Int): Unit = {
      DicomUtil.removeSeq(rtplan, TagByName.PatientSetupSequence, (al: AttributeList) => al.get(TagByName.PatientSetupNumber).getIntegerValues.head == PatientSetupNumber)
    }

    // remove the beam from the BeamSequence
    def deleteBeamSeq(): Unit = {
      val removed = DicomUtil.removeSeq(rtplan, TagByName.BeamSequence, (al: AttributeList) => beamNameOf(al).equals(beamName))

      val BeamNumber = removed.head.get(TagByName.BeamNumber).getIntegerValues.head
      val PatientSetupNumber = removed.head.get(TagByName.ReferencedPatientSetupNumber).getIntegerValues.head

      deleteFractionSeq(BeamNumber)
      deletePatientSetup(PatientSetupNumber)
    }

    deleteBeamSeq()
    logger.info("CustomizeRtPlan removed beam " + beamName)
  }

  /**
    * Remove beams from the plan that are not supported by this machine.  Also remove their ControlPoint counterparts.
    */
  private def removeUnsupportedBeams(rtplan: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val unsupported = beamAlList.filter(beamAl => !beamIsSupported(beamAl, machineEnergyList))
    val unsupportedNameList = unsupported.map(beamAl => beamNameOf(beamAl))
    unsupportedNameList.foreach(beamName => removeBeamFromPlan(rtplan, beamName))
  }

  private def getFractionReference(rtplan: AttributeList, BeamNumber: Int): AttributeList = {
    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence)
    val ReferencedBeamSequence = FractionGroupSequence.flatMap(fractionAl => DicomUtil.seqToAttr(fractionAl, TagByName.ReferencedBeamSequence))

    def beamNumberMatches(fractionAl: AttributeList): Boolean = {
      val ReferencedBeamNumber = fractionAl.get(TagByName.ReferencedBeamNumber)
      (ReferencedBeamNumber != null) && (ReferencedBeamNumber.getIntegerValues.head == BeamNumber)
    }

    ReferencedBeamSequence.filter(fractionAl => beamNumberMatches(fractionAl)).head
  }

  /**
    * Represent a plan beam.  This class is public only to support testing.
    */
  case class PlanBeam(energy: Double, name: String, fff: Boolean) {
    private def fffAsText: String = { if (fff) " FFF" else "" }

    override def toString: String = {
      name + " : " + Util.fmtDbl(energy) + fffAsText
    }
  }

  /**
    * Encapsulate values specified by the user in the user interface.
    */
  case class PlanSpecification(toleranceTable: String, patientID: String, patientName: String, machineName: String, planName: String) {

    /**
      *  Modify all attributes that get a user specified value
      */
    def setOverrides(rtplan: AttributeList): Seq[IndexedSeq[Unit]] = {

      /**
        * When customizing a plan, override this DICOM attribute with the given value.
        */
      case class PlanAttributeOverride(tag: AttributeTag, value: String) {
        override def toString: String = DicomUtil.dictionary.getNameFromTag(tag) + " : " + value
      }

      val overrideList = Seq(
        PlanAttributeOverride(TagByName.ToleranceTableLabel, toleranceTable),
        PlanAttributeOverride(TagFromName.PatientID, patientID),
        PlanAttributeOverride(TagFromName.PatientName, patientName),
        PlanAttributeOverride(TagByName.TreatmentMachineName, machineName),
        PlanAttributeOverride(TagFromName.RTPlanLabel, planName),
        PlanAttributeOverride(TagByName.TableTopVerticalPosition, 0.toString)
      )

      overrideList.map(ov => {
        DicomUtil
          .findAllSingle(rtplan, ov.tag)
          .map(at => {
            at.removeValues()
            at.addValue(ov.value)
          })
      })
    }
  }

  private def getPlanBeamListX(machine: Machine, plan: Option[Config.PlanFileConfig]): List[PlanBeam] = {
    if (plan.isDefined) {
      val planAttrList = plan.get.dicomFile.attributeList.get

      def beamSeqToPlanBeam(beamAl: AttributeList): PlanBeam = {
        val name = beamNameOf(beamAl)
        val energy = DicomUtil.findAllSingle(beamAl, TagByName.NominalBeamEnergy).head.getDoubleValues.head
        val fff = {
          val FluenceModeID = DicomUtil.findAllSingle(beamAl, TagByName.FluenceModeID)
          if (FluenceModeID.isEmpty) false
          else FluenceModeID.map(fmi => fmi.getSingleStringValueOrEmptyString.toUpperCase.contains("FFF")).reduce(_ || _)
        }
        PlanBeam(energy, name, fff)
      }

      val planBeamList = DicomUtil.seqToAttr(planAttrList, TagByName.BeamSequence).map(beamSeq => beamSeqToPlanBeam(beamSeq)).sortBy(_.energy)

      logger.info("Energy list found in plan for machine " + machine.id + " :\n    " + planBeamList.mkString("\n    "))
      planBeamList.toList
    } else
      List[PlanBeam]() // no plan given means no beams
  }

  /**
    * For testing only
    */
  def testGetPlanBeamList(machine: Machine): Seq[PlanBeam] = {
    getPlanBeamListX(machine, getCollimatorCompatiblePhase2PlanForMachine(machine))
  }

  /**
    * Get the energy of a beam.
    */
  private def getEnergy(beamAl: AttributeList): Double = {

    def cpsEnergy(cp: AttributeList): Option[Double] = {
      val NominalBeamEnergy = cp.get(TagByName.NominalBeamEnergy)
      if (NominalBeamEnergy == null) None
      else Some(NominalBeamEnergy.getDoubleValues.head)
    }

    val cps = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence).flatMap(cp => cpsEnergy(cp)).max
    cps
  }

  /**
    * Get the name of the given beam specified by its attribute list.
    */
  private def beamNameOf(beamAl: AttributeList): String = {
    Util.normalizedBeamName(beamAl)
  }

  /**
    * Get the beam to be copied and modified to make non-standard beams.
    */
  private def getPrototypeBeam(rtplan: AttributeList): AttributeList = {
    val beamList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamList.filter(beamAl => beamNameOf(beamAl).equals(Config.PrototypeCustomBeamName)).head
  }

  /**
    * Get an unused beam number.
    */
  private def getAvailableBeamNumber(rtplan: AttributeList): Int = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val all = beamAlList.map(beamAl => beamAl.get(TagByName.BeamNumber).getIntegerValues.head)
    val available = (1 to (all.max + 1)).find(i => !all.contains(i))
    available.get
  }

  private def setFluence(beamAl: AttributeList, fff: Boolean): Unit = {

    /**
      * Remove both the DICOM standard and Varian fluence references.
      */
    def removeFluence() = {
      val PrimaryFluenceModeSequence = beamAl.get(TagByName.PrimaryFluenceModeSequence)
      if (PrimaryFluenceModeSequence != null) beamAl.remove(TagByName.PrimaryFluenceModeSequence)

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
      val FluenceMode = AttributeFactory.newAttribute(TagByName.FluenceMode)
      FluenceMode.addValue(value)
      al.put(FluenceMode)
    }

    def addFluenceModeID(al: AttributeList, value: String) = {
      val FluenceModeID = AttributeFactory.newAttribute(TagByName.FluenceModeID)
      FluenceModeID.addValue(value)
      al.put(FluenceModeID)
    }

    def addVarianCreator3285(al: AttributeList) = al.put(VarianPrivateTag.newVarianCreator3285("Varian Medical Systems VISION 3285"))
    def addVarianFluenceMode(al: AttributeList, mode: String) = al.put(VarianPrivateTag.newFluenceMode(mode))
    def addVarianFluenceModeID(al: AttributeList, mode: String) = al.put(VarianPrivateTag.newFluenceModeID(mode))

    def changeToFluenceStandard = {
      removeFluence()
      // standard attributes
      val al = addSequence(TagByName.PrimaryFluenceModeSequence)
      addFluenceMode(al, "STANDARD")

      // Varian attributes
      val alv = addSequence(VarianPrivateTag.PrimaryFluenceModeSequence)
      addVarianCreator3285(alv)
      addVarianFluenceMode(alv, "STANDARD")
    }

    def changeToFluenceFFF = {
      removeFluence()
      // standard attributes
      val al = addSequence(TagByName.PrimaryFluenceModeSequence)
      addFluenceMode(al, "NON_STANDARD")
      addFluenceModeID(al, "FFF")

      // Varian attributes
      val alv = addSequence(VarianPrivateTag.PrimaryFluenceModeSequence)
      addVarianCreator3285(alv)
      addVarianFluenceMode(alv, "NON_STANDARD")
      addVarianFluenceModeID(alv, "FFF")
    }

    (isFFFBeam(beamAl), fff) match {
      case (true, true)   => ; // no change necessary
      case (false, false) => ; // no change necessary
      case (true, false)  => changeToFluenceStandard
      case (false, true)  => changeToFluenceFFF
    }
  }

  private def changeNominalBeamEnergy(beamAl: AttributeList, energy: Double): Unit = {
    val controlPtSeq = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence)

    def changeOne(cpt: AttributeList): Unit = {
      val NominalBeamEnergy = cpt.get(TagByName.NominalBeamEnergy)

      if (NominalBeamEnergy != null) {
        NominalBeamEnergy.removeValues()
        NominalBeamEnergy.addValue(energy)
      }
    }

    controlPtSeq.foreach(cpt => changeOne(cpt))
  }

  private def changeDoseRate(beamAl: AttributeList, energy: Double): Unit = {
    val controlPtSeq = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence)

    def changeOne(cpt: AttributeList): Unit = {
      val DoseRateSet = cpt.get(TagByName.DoseRateSet)

      if (DoseRateSet != null) {
        DoseRateSet.removeValues()
        DoseRateSet.addValue(energy)
      }
    }

    controlPtSeq.foreach(cpt => changeOne(cpt))
  }

  /**
    * Add another entry with the given number to the PatientSetupSequence.
    */
  private def addPatientSetup(rtplan: AttributeList, PatientSetupNumber: Int): Unit = {
    val patientSetup = DicomUtil.clone(DicomUtil.seqToAttr(rtplan, TagByName.PatientSetupSequence).head)
    val PatientSetupSequence = rtplan.get(TagByName.PatientSetupSequence).asInstanceOf[SequenceAttribute]

    patientSetup.get(TagByName.PatientSetupNumber).removeValues()
    patientSetup.get(TagByName.PatientSetupNumber).addValue(PatientSetupNumber)
    PatientSetupSequence.addItem(patientSetup)
  }

  private def insertBeam(rtplan: AttributeList, beamAl: AttributeList): Unit = {
    val beamList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val index = beamList.indexWhere(b => beamNameOf(b).startsWith(Config.PrefixForMachineDependentBeamName))
    val seq = AttributeFactory.newAttribute(TagByName.BeamSequence).asInstanceOf[SequenceAttribute]

    for (i <- beamList.indices) {
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
    val ReferencedBeamNumber = fraction.get(TagByName.ReferencedBeamNumber)
    ReferencedBeamNumber.removeValues()
    ReferencedBeamNumber.addValue(BeamNumber)

    // modify the beam

    val beamNum = beamAl.get(TagByName.BeamNumber)
    beamNum.removeValues()
    beamNum.addValue(BeamNumber)

    val refPatSetupNumber = beamAl.get(TagByName.ReferencedPatientSetupNumber)
    refPatSetupNumber.removeValues()
    refPatSetupNumber.addValue(BeamNumber)

    val beamNameText = {
      val energy = machineEnergy.photonEnergy_MeV.get
      val numText = if (energy.round == energy) energy.round.toString else Util.fmtDbl(energy)
      val fffText = if (machineEnergy.isFFF) "F" else "X"
      Config.PrefixForMachineDependentBeamName + numText + fffText
    }

    val BeamNameAttr = beamAl.get(TagByName.BeamName)
    BeamNameAttr.removeValues()
    BeamNameAttr.addValue(beamNameText)

    setFluence(beamAl, machineEnergy.isFFF)
    changeNominalBeamEnergy(beamAl, machineEnergy.photonEnergy_MeV.get)
    changeDoseRate(beamAl, machineEnergy.maxDoseRate_MUperMin.get)

    insertBeam(rtplan, beamAl)

    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).head
    val ReferencedBeamSequence = FractionGroupSequence.get(TagByName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute]
    ReferencedBeamSequence.addItem(fraction)
    addPatientSetup(rtplan, BeamNumber)
  }

  private def showBeamList(rtplan: AttributeList) = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)

    def showBeam(beamAl: AttributeList): String = {
      val name = beamNameOf(beamAl)
      val num = beamAl.get(TagByName.BeamNumber).getIntegerValues.head

      val energy = getEnergy(beamAl)
      val fff = if (isFFFBeam(beamAl)) "F" else "X"
      val gantryAngleList = {
        val list = DicomUtil.findAllSingle(beamAl, TagByName.GantryAngle).flatMap(ga => ga.getDoubleValues)
        list.map(ga => Util.fmtDbl(ga)).mkString("  ")
      }

      num.formatted("%3d") + "    " + "    " + name.formatted("%-16s") + "    " + energy.formatted("%5.1f") + "    " + fff + "    gantry angles: " + gantryAngleList
    }

    "Number of beams: " + beamAlList.size + "\n    " + beamAlList.map(beamAl => showBeam(beamAl)).mkString("\n    ")
  }

  private def setRtplanDateTimeToNow(rtplan: AttributeList) = {
    val now = System.currentTimeMillis

    def setDateTime(dateTag: AttributeTag, timeTag: AttributeTag): Unit = {
      val dateAttr = rtplan.get(dateTag)
      if (dateAttr != null) {
        dateAttr.removeValues()
        dateAttr.addValue(DicomUtil.dicomDateFormat.format(now))
      }

      val timeAttr = rtplan.get(timeTag)
      if (timeAttr != null) {
        timeAttr.removeValues()
        timeAttr.addValue(DicomUtil.dicomTimeFormat.format(now))
      }
    }

    val dateTimeTagList = Seq(
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
      (TagFromName.StudyDate, TagFromName.StudyTime),
      (TagFromName.RTPlanDate, TagFromName.RTPlanTime)
    )

    dateTimeTagList.map(dt => setDateTime(dt._1, dt._2))
  }

  private def orderBeamsByRenaming(rtplan: AttributeList) = {

    val beamList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)

    def updateBeamName(bi: Int) = {
      val BeamName = beamList(bi).get(TagByName.BeamName)

      val name = BeamName.getSingleStringValueOrEmptyString
      val newName = (bi + 1).formatted("%02d") + ":" + name
      BeamName.removeValues()
      BeamName.addValue(newName)

      beamList(bi).put(VarianPrivateTag.newBeamSecondaryName(newName))
    }

    beamList.indices.map(bi => updateBeamName(bi))
  }

  private def reassignPlanEnergies(rtplan: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    // use this beam and its fraction reference to make non-standard beams
    val prototypeBeam = getPrototypeBeam(rtplan)

    logger.info("original rtplan\n" + showBeamList(rtplan))
    val prototypeFractionReference = getFractionReference(rtplan, prototypeBeam.get(TagByName.BeamNumber).getIntegerValues.head)

    logger.info("machineEnergyList size: " + machineEnergyList.size + "\n    " + machineEnergyList.mkString("\n    "))
    removeUnsupportedBeams(rtplan, machineEnergyList)

    val unsupportedEnergyList = machineEnergyList.filter(_.maxDoseRate_MUperMin.isDefined).filter(me => !machineEnergyIsInPlan(rtplan, me))

    logger.info("unsupportedEnergyList size: " + unsupportedEnergyList.size + "\n    " + unsupportedEnergyList.map(me => me.toString).mkString("\n    "))

    unsupportedEnergyList.foreach(me => addBeam(rtplan, me, prototypeBeam, prototypeFractionReference))
    logger.info("customized rtplan\n" + showBeamList(rtplan))

    setNumberOfBeamsInFractionGroupSequence(rtplan)
    orderBeamsByRenaming(rtplan)
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
      VarianPrivateTag.PlanIntegritySequence
    )

    privateTagList.map(tag => rtplan.remove(tag))
  }

  /**
    * Insert a new PatientProcedure if it is not already there.
    *
    * @param institutionPK For this institution.
    * @param patientIdAttr Anonymized patient ID.
    * @param procedure Should make or assure that there is a row for this procedure.
    */
  private def setupPatientProcedure(institutionPK: Long, patientIdAttr: Attribute, procedure: Option[Procedure]): Unit = {
    val ppList = PatientProcedure.listExtended(institutionPK)

    val patientID = patientIdAttr.getSingleStringValueOrEmptyString

    def alreadyOnPatientProcedureList = ppList.exists(pp => pp.dicomAnonymous.value.equals(patientID))

    val existingPatientIdList = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagByName.PatientID))

    // The relevant DicomAnonymous record.  If it exists, use it.  If not, then make one and put it in the database.
    val dicomAnon: DicomAnonymous = existingPatientIdList.find(da => da.value.equals(patientID)) match {
      case Some(da) => da
      case _ =>
        val da = DicomAnonymous.insert(institutionPK, patientIdAttr)
        da
    }

    if (procedure.isDefined && (!alreadyOnPatientProcedureList)) {
      val procPK = procedure.get.procedurePK.get
      val pp = new PatientProcedure(None, dicomAnon.dicomAnonymousPK.get, institutionPK, procPK, active = true)
      val newPatProc = pp.insert
      logger.info("Added patient procedure: " + newPatProc)
    }
  }

  /**
    * Given all the required information, create an rtplan that is compatible with the given machine for Phase2 or Phase3.
    */
  private def makePlanPhaseAny(
      machine: Machine,
      userPK: Long,
      planSpecification: PlanSpecification,
      machineEnergyList: Seq[MachineBeamEnergy],
      procedure: Procedure,
      pattern: String
  ): AttributeList = {

    val rtplan = DicomUtil.clone(getCollimatorCompatiblePlanForMachine(machine, pattern).head.dicomFile.attributeList.get)
    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    removeVarianPrivateTagAttributes(rtplan)
    reassignPlanEnergies(rtplan, machineEnergyList)
    val anonDicom = saveAnonymizedDicom(machine, userPK, rtplan, procedure.procedurePK)
    setupPatientProcedure(machine.institutionPK, anonDicom.get(TagByName.PatientID), Some(procedure))
    rtplan
  }

  /**
    * Given all the required information, create an rtplan that is compatible with the given machine for Phase2.
    */
  def makePlanPhase2(machine: Machine, userPK: Long, planSpecification: PlanSpecification, machineEnergyList: Seq[MachineBeamEnergy]): AttributeList = {
    makePlanPhaseAny(machine, userPK, planSpecification, machineEnergyList, Procedure.ProcOfPhase2.get, patternPhase2)
  }

  /**
    * Given all the required information, create an rtplan that is compatible with the given machine for Phase3.
    */
  def makePlanPhase3(machine: Machine, userPK: Long, planSpecification: PlanSpecification, machineEnergyList: Seq[MachineBeamEnergy]): AttributeList = {
    makePlanPhaseAny(machine, userPK, planSpecification, machineEnergyList, Procedure.ProcOfPhase3.get, patternPhase3)
  }

  /**
    * Given all the required information, create an rtplan that is compatible with the given machine for Daily QA.
    */
  def makePlanDailyQA(machine: Machine, userPK: Long, planSpecification: PlanSpecification): AttributeList = {

    val rtplan = DicomUtil.clone(getCollimatorCompatibleDailyQAPlanForMachine(machine).get.dicomFile.attributeList.get)
    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    removeVarianPrivateTagAttributes(rtplan)
    val anonDicom = saveAnonymizedDicom(machine, userPK, rtplan, Procedure.ProcOfBBbyEPID.get.procedurePK)

    setupPatientProcedure(machine.institutionPK, anonDicom.get(TagByName.PatientID), Procedure.ProcOfBBbyCBCT)
    rtplan
  }

  /**
    * Given all the required information, create an rtplan that is compatible with the given machine for gap skew.
    */
  def makePlanGapSkew(machine: Machine, userPK: Long, planSpecification: PlanSpecification): AttributeList = {

    val rtplan = DicomUtil.clone(getCollimatorCompatibleGapSkewPlanForMachine(machine).get.dicomFile.attributeList.get)
    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    removeVarianPrivateTagAttributes(rtplan)
    val anonDicom = saveAnonymizedDicom(machine, userPK, rtplan, Procedure.ProcOfGapSkew.get.procedurePK)

    setupPatientProcedure(machine.institutionPK, anonDicom.get(TagByName.PatientID), Procedure.ProcOfGapSkew)
    rtplan
  }

  /**
    * Given all the required information, create a pair of rtplans that are compatible with the given machine for LOC.
    */
  def makePlanLOC(machine: Machine, userPK: Long, planSpecification: PlanSpecification): CustomizeRtPlan.LOCRtplanPair = {

    val rtplanPair = getCollimatorCompatibleLocPlanPairForMachine(machine).get

    // change UIDs so that these plans will be considered new and unique from all others.
    rtplanPair.asSeq.foreach(rtplan => replaceAllUIDs(rtplan))

    // Force them to have the same study instance UID so we know they were made together.
    val studyAt = rtplanPair.delivery.dicomFile.attributeList.get.get(TagFromName.StudyInstanceUID)
    studyAt.removeValues()
    studyAt.addValue(Util.studyInstOfAl(rtplanPair.baseline.dicomFile.attributeList.get))

    rtplanPair.asSeq.map(rtplan => planSpecification.setOverrides(rtplan))
    rtplanPair.asSeq.map(rtplan => setRtplanDateTimeToNow(rtplan))
    rtplanPair.asSeq.foreach(rtplan => removeVarianPrivateTagAttributes(rtplan))

    saveAnonymizedDicom(machine, userPK, rtplanPair.baselineAl, Procedure.ProcOfLOC.get.procedurePK)

    val anonDicom = saveAnonymizedDicom(machine, userPK, rtplanPair.deliveryAl, Procedure.ProcOfLOC.get.procedurePK)
    setupPatientProcedure(machine.institutionPK, anonDicom.get(TagByName.PatientID), Procedure.ProcOfLOCBaseline)

    rtplanPair
  }

}
