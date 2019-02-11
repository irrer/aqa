package org.aqa.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import WebUtil._
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

object CustomizeRtPlan {
  def reference(machinePK: Long) = { (new CustomizeRtPlan).pathOf + "?" + MachineUpdate.machinePKTag + "=" + machinePK }

  def redirect(machinePK: Long, response: Response): Unit = response.redirectSeeOther(reference(machinePK))

  def redirect(valueMap: ValueMapT, response: Response): Unit = redirect(valueMap(MachineUpdate.machinePKTag).toLong, response)
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

      DicomUtil.seqToAttr(planAttrList, TagFromName.BeamSequence)

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

  /**
   * Make the content for the machine energy selector.
   */
  private def getMachineEnergySelections(machineEnergyList: Seq[MachineBeamEnergy]): Seq[(String, String)] = {

    def machEnergyToString(machEnergy: MachineBeamEnergy): String = {
      val e = if (machEnergy.photonEnergy_MeV.isDefined) Util.fmtDbl(machEnergy.photonEnergy_MeV.get) else "0"
      val fff = if (machEnergy.fffEnergy_MeV.isDefined && machEnergy.fffEnergy_MeV.get != 0) " FFF" else ""
      e + fff
    }

    val pairs = machineEnergyList.zipWithIndex.map(ei => (ei._2.toString, machEnergyToString(ei._1)))
    pairs :+ (pairs.size.toString, "Do not deliver")
  }

  private val machEnergyPrefix = "MachEnergy"
  private def machEnergyTag(index: Int) = machEnergyPrefix + index

  /**
   * get list of beams in plan that are distinct (disregarding beam names)
   */
  private def planBeamListToDistinct(planBeamList: Seq[PlanBeam]): Seq[PlanBeam] = {
    val energyList = planBeamList.map(pb => new PlanBeam(pb.energy, "", pb.fff)).toSet.toList
    energyList.sortWith((a, b) => if (a.energy != b.energy) a.energy < b.energy else b.fff)
  }

  private def energyRowList(machine: Machine, machineEnergyList: Seq[MachineBeamEnergy], planBeamList: Seq[PlanBeam]): List[WebRow] = {
    val machineEnergySelectionList = getMachineEnergySelections(machineEnergyList)

    val distinctList = planBeamListToDistinct(planBeamList)

    val rowList = distinctList.indices.map(i => {
      val fff = if (distinctList(i).fff) " FFF" else ""

      val label = "For plan beams below with energy " + Util.fmtDbl(distinctList(i).energy) + fff + " use machine energy: "

      val planTable = {
        val matching = planBeamList.filter(pb => ((pb.energy == distinctList(i).energy) && pb.fff == distinctList(i).fff))

        val tableContents = edu.umro.ScalaUtil.Util.sizedGroups(matching.map(pb => <td>{ pb.name }</td>), 3).map(row => <tr>{ row }</tr>)

        <table class="table table-bordered">
          { tableContents }
        </table>
      }

      val elem = {
        <span title="Choose the machine energy to use for the planned energy">{ <label class="control-label">{ label }</label> }<br></br>{ planTable }</span>
      }

      val energyPlan = new WebPlainText(label, false, 4, 0, (ValueMapT) => elem)
      val energyMach = new WebInputSelect(machEnergyTag(i), false, 2, 0, (_) => machineEnergySelectionList, false)
      val row: WebRow = List(energyPlan, energyMach)
      row
    })
    rowList.toList
  }

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    new FormButton(name, 1, 0, subUrl, pathOf, buttonType)
  }

  private val createButton = makeButton("Create", false, ButtonType.BtnPrimary)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)
  private val backButton = makeButton("Back", false, ButtonType.BtnDefault)

  private val assignButtonList: WebRow = List(createButton, cancelButton, machinePK)

  /**
   * Find the machine energy value closest to each plan energy value and assign it.
   */
  private def automaticEnergyAssignments(valueMap: ValueMapT, machineEnergyList: Seq[MachineBeamEnergy], planBeamList: Seq[PlanBeam]) = {

    val distinctList = planBeamListToDistinct(planBeamList)

    def closestMachineEnergy(planEnergy: PlanBeam): String = {
      val machEnergyDefined = machineEnergyList.filter(me => me.photonEnergy_MeV.isDefined)
      if (machEnergyDefined.isEmpty)
        "0"
      else {
        val machMatchingFFF = {
          if (planEnergy.fff)
            machEnergyDefined.filter(me => me.fffEnergy_MeV.isDefined && me.fffEnergy_MeV.get != 0)
          else
            machEnergyDefined.filter(me => me.fffEnergy_MeV.isEmpty || me.fffEnergy_MeV.get == 0)
        }
        if (machMatchingFFF.isEmpty)
          "0"
        else {
          val closest = machMatchingFFF.minBy(me => (me.photonEnergy_MeV.get - planEnergy.energy).abs)
          machineEnergyList.indexOf(closest).toString
        }
      }
    }
    val energyMap = distinctList.indices.map(p => (machEnergyTag(p), closestMachineEnergy(distinctList(p)))).toMap
    energyMap
  }

  private def formSelect(valueMap: ValueMapT, response: Response, machine: Machine) = {
    val machineEnergyList = getMachineEnergyList(machine.machinePK.get)
    val planEnergyList = getPlanBeamList(machine).toList
    val form = new WebForm(pathOf, List(row0, row1, row2) ++ energyRowList(machine, machineEnergyList, planEnergyList) ++ List(assignButtonList))

    // if field is empty
    def empty(label: String) = valueMap.get(label).isEmpty || (valueMap(label).trim.size == 0)

    val defaultPatient = "$AQA_" + machine.machinePK.get

    def getRealMachineId = AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)

    val assignValMap = if (empty(machEnergyTag(0))) automaticEnergyAssignments(valueMap, machineEnergyList, planEnergyList) else emptyValueMap
    val patientIdMap = if (empty(patientID.label)) Map((patientID.label, defaultPatient)) else emptyValueMap
    val patientNameMap = if (empty(patientName.label)) Map((patientName.label, defaultPatient)) else emptyValueMap
    val machineNameMap = if (empty(machineName.label)) Map((machineName.label, getRealMachineId)) else emptyValueMap

    val valMap = valueMap ++ assignValMap ++ patientIdMap ++ patientNameMap ++ machineNameMap
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

  private def reassignPlanEnergies(rtplan: AttributeList, valueMap: ValueMapT, planBeamList: Seq[PlanBeam], machineEnergyList: Seq[MachineBeamEnergy]): Unit = {
    val distinctList = planBeamListToDistinct(planBeamList)

    def rowOf(planBeam: PlanBeam): Int = {
      val rowIndex = distinctList.indices.filter(i => {
        val d = distinctList(i)
        (d.energy == planBeam.energy) && (d.fff == planBeam.fff)
      }).head
      rowIndex
    }

    def machEnergyForBeam(planBeam: PlanBeam): Option[MachineBeamEnergy] = {
      val row = rowOf(planBeam)
      val machEnergyIndex = valueMap(machEnergyTag(row)).toInt
      if (machEnergyIndex < machineEnergyList.size) {
        val machEnergy = machineEnergyList(machEnergyIndex)
        Some(machEnergy)
      } else
        None
    }

    def changePlanEnergyToMachineEnergy(planBeam: PlanBeam, machEnergy: MachineBeamEnergy): Unit = {
      val beamAl = DicomUtil.seqToAttr(rtplan, TagFromName.BeamSequence).filter(al => al.get(TagFromName.BeamName).getSingleStringValueOrEmptyString.equals(planBeam.name)).head
      val controlPtSeq = DicomUtil.seqToAttr(beamAl, TagFromName.ControlPointSequence)

      def isFFFBeam: Boolean = {
        val PrimaryFluenceModeSequence = DicomUtil.seqToAttr(beamAl, TagFromName.PrimaryFluenceModeSequence)
        def isFFF(pfms: AttributeList): Boolean = {
          val FluenceModeID = pfms.get(TagFromName.FluenceModeID)
          (FluenceModeID != null) && FluenceModeID.getSingleStringValueOrEmptyString.toLowerCase.contains("fff")
        }
        val fff = PrimaryFluenceModeSequence.map(pfms => isFFF(pfms)).reduce(_ || _)
        fff
      }

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

      def changeOne(cpt: AttributeList): Unit = {
        val NominalBeamEnergy = cpt.get(TagFromName.NominalBeamEnergy)

        if (NominalBeamEnergy != null) {
          NominalBeamEnergy.removeValues
          NominalBeamEnergy.addValue(machEnergy.photonEnergy_MeV.get)
        }
      }

      controlPtSeq.map(cpt => changeOne(cpt))

      if (isFFFBeam != machEnergy.isFFF) {
        if (machEnergy.isFFF)
          changeToFluenceFFF
        else
          changeToFluenceStandard
      }
    }

    def reassignOnePlanEnergy(planBeam: PlanBeam): Unit = {
      val machEnergy = machEnergyForBeam(planBeam)
      if (machEnergy.isEmpty) {
        deleteBeamFromPlan(rtplan, planBeam.name)
      } else {
        // change the energy of the beam
        changePlanEnergyToMachineEnergy(planBeam, machEnergy.get)
      }
    }

    planBeamList.map(pb => reassignOnePlanEnergy(pb))
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
      val form = new WebForm(pathOf, List(row0, row1, row2) ++ energyRowList(machine, machineEnergyList, planEnergyList) ++ List(assignButtonList))
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
