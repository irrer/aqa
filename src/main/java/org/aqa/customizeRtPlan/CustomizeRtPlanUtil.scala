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

package org.aqa.customizeRtPlan

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
import org.aqa.db.MachineType
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure

import scala.collection.immutable

object CustomizeRtPlanUtil extends Logging {

  /**
    * Save an anonymized version of this file, so when the user uploads RTIMAGES generated by
    * this plan, the plan will already be uploaded.
    */
  def saveAnonymizedDicom(machine: Machine, userPK: Long, rtplan: AttributeList, procedurePK: Option[Long]): AttributeList = {
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
  def replaceAllUIDs(attributeList: AttributeList): Unit = {
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
    * @return True if this is an FFF beam.
    */
  def isFFFBeam(beamAl: AttributeList): Boolean = {
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
  private def beamIsSupported(beamAl: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Boolean = {
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
  def setNumberOfBeamsInFractionGroupSequence(rtplan: AttributeList): Unit = {
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
    * @param rtplan   Remove from this plan.
    * @param beamName The name of the beam.
    */
  def removeBeamFromPlan(rtplan: AttributeList, beamName: String): Unit = {
    logger.info("Removing beam " + beamName)

    def deleteFractionSeq(BeamNumber: Int): Unit = {
      val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).head
      val findRBS = (al: AttributeList) => {
        val attr = al.get(TagByName.ReferencedBeamNumber)
        (attr != null) && (al.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == BeamNumber)
      }
      DicomUtil.removeSeq(FractionGroupSequence, TagByName.ReferencedBeamSequence, findRBS)
    }

    def deletePatientSetup(PatientSetupNumber: Int): Unit = {
      DicomUtil.removeSeq(rtplan, TagByName.PatientSetupSequence, (al: AttributeList) => al.get(TagByName.PatientSetupNumber).getIntegerValues.head == PatientSetupNumber)
    }

    // remove the beam from the BeamSequence
    def deleteBeamSeq(): Unit = {
      val removed = DicomUtil.removeSeq(rtplan, TagByName.BeamSequence, (al: AttributeList) => beamNameOf(al).equals(beamName))

      if (removed.nonEmpty) {
        val BeamNumber = removed.head.get(TagByName.BeamNumber).getIntegerValues.head
        val PatientSetupNumber = removed.head.get(TagByName.ReferencedPatientSetupNumber).getIntegerValues.head

        deleteFractionSeq(BeamNumber)
        deletePatientSetup(PatientSetupNumber)
      }
    }

    deleteBeamSeq()
    logger.info("CustomizeRtPlan removed beam " + beamName)
  }

  /**
    * Remove beams from the plan that are not supported by this machine.  Also remove their ControlPoint counterparts.
    */
  def removeUnsupportedBeams(rtplan: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
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
    private def fffAsText: String = {
      if (fff) " FFF" else ""
    }

    override def toString: String = {
      name + " : " + Util.fmtDbl(energy) + fffAsText
    }
  }

  def getPlanBeamListX(machine: Machine, plan: Option[Config.PlanFileConfig]): List[PlanBeam] = {
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
  def beamNameOf(beamAl: AttributeList): String = {
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
    if (all.isEmpty)
      1
    else {
      val available = (1 to (all.max + 1)).find(i => !all.contains(i))
      available.get
    }
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
    val patientSetup = makePatientSetup(PatientSetupNumber)
    val PatientSetupSequence = rtplan.get(TagByName.PatientSetupSequence).asInstanceOf[SequenceAttribute]
    PatientSetupSequence.addItem(patientSetup)
  }

  private def appendBeam(rtplan: AttributeList, beamAl: AttributeList): Unit = {
    val seq = rtplan.get(TagByName.BeamSequence).asInstanceOf[SequenceAttribute]
    seq.addItem(beamAl)
  }

  /**
    * Given a prototype beam, rake a copy with the given characteristics.
    *
    * @param machineEnergy Use these parameters.
    * @param prototypeBeam Start with this beam.
    * @param BeamName New name of beam.
    * @param BeamNumber New number of beam.
    * @return Modified copy of prototype.
    */
  def makeBeam(machineEnergy: MachineBeamEnergy, prototypeBeam: AttributeList, BeamName: String, BeamNumber: Int): AttributeList = {
    val beamAl = DicomUtil.clone(prototypeBeam)

    def setBeamNumber(tag: AttributeTag): Unit = {
      val beamNum = beamAl.get(tag)
      beamNum.removeValues()
      beamNum.addValue(BeamNumber)
    }

    setBeamNumber(TagByName.BeamNumber)
    setBeamNumber(TagByName.ReferencedPatientSetupNumber)

    val BeamNameAttr = beamAl.get(TagByName.BeamName)
    BeamNameAttr.removeValues()
    BeamNameAttr.addValue(BeamName)

    setFluence(beamAl, machineEnergy.isFFF)
    changeNominalBeamEnergy(beamAl, machineEnergy.photonEnergy_MeV.get)
    val maxDoseRate: Double =
      machineEnergy.maxDoseRate_MUperMin match {
        case Some(mdr) => mdr
        case _         =>
          // if the dose rate is not defined, then use a reasonable value based on known characteristics of the machines
          val machine = Machine.get(machineEnergy.machinePK).get
          val machineType = MachineType.get(machine.machineTypePK).get
          val resolved = resolveEnergy(machineEnergy, machineType)
          resolved.maxDoseRate_MUperMin.get
      }
    changeDoseRate(beamAl, maxDoseRate)

    beamAl
  }

  /**
    * Make a patient setup attribute list (goes in PatientSetupSequence).  Note: byi
    * convention the patientSetupNumber is the same as the BeamNumber.
    * @param patientSetupNumber   value for PatientSetupNumber.  Usually the same as BeamNumber.
    * @return Newly created attribute list.
    */
  private def makePatientSetup(patientSetupNumber: Int): AttributeList = {

    val al = new AttributeList

    def putAttribute(tag: AttributeTag, value: String): Unit = {
      val attr = AttributeFactory.newAttribute(tag)
      attr.addValue(value)
      al.put(attr)
    }

    putAttribute(TagByName.PatientPosition, "HFS")
    putAttribute(TagByName.ReferencedBeamNumber, patientSetupNumber.toString)
    putAttribute(TagByName.PatientSetupNumber, patientSetupNumber.toString)
    putAttribute(TagByName.SetupTechnique, "ISOCENTRIC")

    al
  }

  /**
    * Add a beam that supports the given machine energy.  Do it by copying and modifying both prototypes.  The original prototypes are
    * not changed, but rtplan is changed.
    *
    * @param rtplan Add to this RTPLAN.
    * @param machineEnergy Make beam with these values.
    * @param prototypeBeam Made from a copy of this beam.
    * @param prototypeFractionReference fraction item.
    *
    * @return Cloned beam with new values.
    */
  def addBeam(
      rtplan: AttributeList,
      machineEnergy: MachineBeamEnergy,
      prototypeBeam: AttributeList,
      prototypeFractionReference: AttributeList,
      BeamNameOpt: Option[String] = None
  ): AttributeList = {
    val BeamNumber = getAvailableBeamNumber(rtplan)

    val fraction = makePatientSetup(BeamNumber)

    val BeamName =
      if (BeamNameOpt.isDefined)
        BeamNameOpt.get
      else {
        val energy = machineEnergy.photonEnergy_MeV.get
        val numText = if (energy.round == energy) energy.round.toString else Util.fmtDbl(energy)
        val fffText = if (machineEnergy.isFFF) "F" else "X"
        Config.PrefixForMachineDependentBeamName + numText + fffText
      }

    val beamAl = makeBeam(machineEnergy, prototypeBeam, BeamName: String, BeamNumber)

    appendBeam(rtplan, beamAl)

    val FractionGroupSequence = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).head
    val ReferencedBeamSequence = FractionGroupSequence.get(TagByName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute]
    ReferencedBeamSequence.addItem(fraction)
    addPatientSetup(rtplan, BeamNumber)
    setNumberOfBeamsInFractionGroupSequence(rtplan)

    beamAl
  }

  private def showBeamList(rtplan: AttributeList): String = {
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

      num.formatted("%3d") + "    " + "    " + name.format("%-16s") + "    " + energy.formatted("%5.1f") + "    " + fff + "    gantry angles: " + gantryAngleList
    }

    "Number of beams: " + beamAlList.size + "\n    " + beamAlList.map(beamAl => showBeam(beamAl)).mkString("\n    ")
  }

  def setRtplanDateTimeToNow(rtplan: AttributeList): Seq[Unit] = {
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

  def orderBeamsByRenaming(rtplan: AttributeList): immutable.IndexedSeq[Attribute] = {

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

  def reassignPlanEnergies(rtplan: AttributeList, machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
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

  def getMachineEnergyList(machinePK: Long): Seq[MachineBeamEnergy] = {
    def compareMBE(a: MachineBeamEnergy, b: MachineBeamEnergy): Boolean = {

      val compare = (a.photonEnergy_MeV, b.photonEnergy_MeV, a.fffEnergy_MeV, b.fffEnergy_MeV) match {
        case (Some(aPho), Some(bPho), _, _) if aPho != bPho => aPho < bPho
        case (Some(_), _, _, _)                             => true
        case (_, Some(_), _, _)                             => false
        case (_, _, Some(aFFF), Some(bFFF)) if aFFF != bFFF => aFFF < bFFF
        case (_, _, Some(_), _)                             => true
        case (_, _, _, Some(_))                             => false
        case _                                              => true
      }
      compare
    }

    val list = MachineBeamEnergy.getByMachine(machinePK).sortWith(compareMBE)
    list
  }

  /**
    * There are some Varian  tags that are in a proprietary format that is not possible to
    * duplicate.  The tags are not necessary for treatment so they are deleted so that they do not
    * invalidate the plan.
    */
  def removeVarianPrivateTagAttributes(rtplan: AttributeList): Unit = {

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
    * @param procedure     Should make or assure that there is a row for this procedure.
    */
  def setupPatientProcedure(institutionPK: Long, patientIdAttr: Attribute, procedure: Option[Procedure]): Unit = {
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
    * If the maxDoseRate is defined, then the energy is fine.  Otherwise, make a copy
    * of the energy with a reasonable value inserted.
    *
    * The value is based on the machine type.
    *
    * If the machine type is not recognized then an exception is thrown.
    *
    * The following is an email from Justin Mikell regarding how to handle missing dose rate:
    *
    * >  From: Mikell, Justin <mikell@wustl.edu>
    * >  Sent: Tuesday, November 14, 2023 12:21 PM
    * >  To: Irrer, Jim <irrer@med.umich.edu>
    * >  Subject: RE: AQA machine configuration
    * >
    * >  Good question,  The maximum allowable dose rate is defined by the machine class and energy.
    * >  I think it should be driven by the TYPE or machine class AND energy.
    * >  Varian only allows certain dose rates – they create so many pulses and then drop pulses to lower the dose rate from the max.
    * >  I think the maxes listed below should be sufficient for your plans.
    * >
    * >  For Varian Truebeam and C-Series: 600 MU/min for 6X, 10X, 15X, 18X: 600,500,400,300,200,100 MU/min. Some go lower, but I think for AQA testing purposes this range should be sufficient.
    * >
    * >
    * >  For the FFF beams on Truebeam:
    * >  6FFF: 1400,1200,1000,800,600,400 MU/min
    * >  10FFF: 2400, 2000, 1600, 1200, 800, 400 MU/min
    * >
    * >  For Halycon/Ethos:6FFF: 800Mu/min
    * >
    * >  Clinically we would typically plan with the maximum dose rate to decrease beam on time.
    * >  There are some games you can play with changing dose rate to make gantry go faster or slower (limited by dose rate or limited by max gantry speed).
    *
    * @param beamEnergy Check for this energy.
    * @param machineType Type of machine.  Note: This function could look this up in the database, but because
    *                    this operation may be performed multiple times with the same machine type, it is more
    *                    efficient to have the caller get it once and pass it as a parameter.
    *
    * @return Beam energy with dose rate defined.
    */
  def resolveEnergy(beamEnergy: MachineBeamEnergy, machineType: MachineType): MachineBeamEnergy = {
    val fff = beamEnergy.isFFF
    val trueBeam = machineType.isTrueBeam
    val cSeries = machineType.isCSeries

    val mbe = 0 match {
      case _ if beamEnergy.maxDoseRate_MUperMin.isDefined                  => beamEnergy
      case _ if (!fff) && (trueBeam || cSeries)                            => beamEnergy.copy(maxDoseRate_MUperMin = Some(600))
      case _ if fff && trueBeam && (beamEnergy.photonEnergy_MeV.get == 6)  => beamEnergy.copy(maxDoseRate_MUperMin = Some(1400))
      case _ if fff && trueBeam && (beamEnergy.photonEnergy_MeV.get == 10) => beamEnergy.copy(maxDoseRate_MUperMin = Some(2400))
      case _ if fff && trueBeam                                            => beamEnergy.copy(maxDoseRate_MUperMin = Some(1200)) // this accommodates other FFF photon energies
      case _ => // We can not make a good decision, so punt.
        val machine = Machine.get(beamEnergy.machinePK).get
        //noinspection SpellCheckingInspection
        val msg =
          s"Unexpected exception: Machine $machine has undefined maxDoseRate_MUperMin for machine beam energy $beamEnergy ." +
            s" Machine type: $machineType  .  This can be resolved by changing the machine's configuration via the web" +
            s" interface (see Administration --> Machines) to define (currently undefined) max dose rate."
        logger.error(msg)
        throw new RuntimeException(msg)
    }
    mbe
  }

  /**
    * Change RTPlanGeometry to "TREATMENT_DEVICE" so that we do not need patient position.  Also remove reference to RTSTRUCT.
    * @param rtplan Change this plan.
    */
  def fixRtplanGeometry(rtplan: AttributeList): Unit = {
    rtplan.remove(TagByName.RTPlanGeometry)
    val attr = AttributeFactory.newAttribute(TagByName.RTPlanGeometry)
    attr.addValue("TREATMENT_DEVICE")
    rtplan.put(attr)

    rtplan.remove(TagByName.ReferencedStructureSetSequence)
  }

}
