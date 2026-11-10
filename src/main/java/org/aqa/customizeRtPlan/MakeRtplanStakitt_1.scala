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
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.fixRtplanGeometry
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.Config

import java.io.File

/**
  * Make RTPLAN for Stakitt.
  *
  */
class MakeRtplanStakitt_1 extends MakeRtplan {

  override def name: String = "Phase3Stakitt"

  override def planFileProcedureName: String = "Phase3Stakitt"

  override def procedure: Procedure = Procedure.ProcOfPhase3.get

  private def beamsOfPlan(rtplan: AttributeList): Seq[AttributeList] = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)

  /**
    * Keep the collimator centering beams and get rid of everything else.
    * @param rtplan For this plan
    */
  private def removeExtraneousBeams(rtplan: AttributeList): Unit = {

    val allBeamNames = DicomUtil.findAllTag(rtplan, TagByName.BeamName).map(_.getSingleStringValueOrEmptyString()).distinct

    val collimatorCenteringBeamsG0List = Seq(
      Config.CollimatorCentering090BeamName,
      Config.CollimatorCentering270BeamName,
      Config.CollimatorCenteringBeamNameG000C090,
      Config.CollimatorCenteringBeamNameG000C270
    )

    allBeamNames.foreach(beamName =>
      if (!collimatorCenteringBeamsG0List.contains(beamName))
        CustomizeRtPlanUtil.removeBeamFromPlan(rtplan, beamName)
    )
  }

  private def renameToPhase3(rtplan: AttributeList): Unit = {
    // val beamList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)

    val allBeamNameAttributes = DicomUtil.findAllTag(rtplan, TagByName.BeamName)

    def fixName(BeamName: Attribute): Unit = {
      val oldName = BeamName.getSingleStringValueOrEmptyString()

      if (oldName.equals(Config.CollimatorCentering090BeamName)) {
        BeamName.removeValues()
        BeamName.addValue(Config.CollimatorCenteringBeamNameG000C090)
      }

      if (oldName.equals(Config.CollimatorCentering270BeamName)) {
        BeamName.removeValues()
        BeamName.addValue(Config.CollimatorCenteringBeamNameG000C270)
      }
    }

    allBeamNameAttributes.foreach(fixName)
  }

  private val hdPlanAl = {
    val al = new AttributeList
    // val hdPlanFile = new File("""D:\tmp\stakittDicom\try\RTPLAN_CMN_HD.dcm""")
    val hdPlanFile = new File("""D:\tmp\stakittDicom\try\RTPLAN_CMN_MIL.dcm""")
    al.read(hdPlanFile)
    al
  }

  private def fetchStakittBeam(): AttributeList = {

    val beamList = beamsOfPlan(hdPlanAl)

    def isStakitt(beam: AttributeList): Boolean = {
      val beamName = DicomUtil.findAllTag(beam, TagByName.BeamName).head.getSingleStringValueOrEmptyString
      beamName.trim.toLowerCase().matches(".*stakit+.*")
    }

    val stakittBeam = beamList.find(isStakitt).get

    stakittBeam
  }

  private def addStakittBeam(rtplan: AttributeList, beam: AttributeList): Unit = {

    val stakittBeam = fetchStakittBeam()
    Trace.trace()

    // DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val seq = rtplan.get(TagByName.BeamSequence).asInstanceOf[SequenceAttribute]

    seq.addItem(stakittBeam)
    Trace.trace()

    // ------------------------------------------------------------------------------------

    val stakittBeamNumber = stakittBeam.get(TagByName.BeamNumber).getIntegerValues.head

    Trace.trace()
    val stakittReferencedBeam = {
      def referencesStakittBeam(al: AttributeList): Boolean = {
        val attr = al.get(TagByName.ReferencedBeamNumber)
        attr.getIntegerValues.head == stakittBeamNumber
      }
      Trace.trace()

      val fgs = DicomUtil.seqToAttr(hdPlanAl, TagByName.FractionGroupSequence).head
      Trace.trace()
      val refBeamSeq = {
        val list = DicomUtil.seqToAttr(fgs, TagByName.ReferencedBeamSequence)
        val rbs = list.find(referencesStakittBeam)
        rbs.get
      }
      Trace.trace()
      refBeamSeq
    }

    Trace.trace()
    val ReferencedBeamSequence = {
      val fgs = DicomUtil.seqToAttr(rtplan, TagByName.FractionGroupSequence).head
      val refBeamSeq = fgs.get(TagByName.ReferencedBeamSequence).asInstanceOf[SequenceAttribute]
      refBeamSeq
    }

    val BeamDose = AttributeFactory.newAttribute(TagByName.BeamDose)
    BeamDose.addValue(0.8)
    stakittReferencedBeam.put(BeamDose)

    val BeamMeterset = AttributeFactory.newAttribute(TagByName.BeamMeterset)
    BeamMeterset.addValue(50.0)
    stakittReferencedBeam.put(BeamMeterset)

    ReferencedBeamSequence.addItem(stakittReferencedBeam)

    Trace.trace()

    // ------------------------------------------------------------------------------------

    // add PatientSetupSequence

    val PatientSetupSequence = rtplan.get(TagByName.PatientSetupSequence).asInstanceOf[SequenceAttribute]
    val newPSS = DicomUtil.clone(PatientSetupSequence.getItem(0).getAttributeList)
    val PatientSetupNumber = newPSS.get(TagByName.PatientSetupNumber)
    PatientSetupNumber.removeValues()
    PatientSetupNumber.addValue(stakittBeamNumber)
    PatientSetupSequence.addItem(newPSS)
    Trace.trace()
  }

  private def removeDose(rtplan: AttributeList): Unit = {

    DicomUtil.removeAllInTree(rtplan, TagByName.DoseReferenceSequence)

    DicomUtil.removeAllInTree(rtplan, TagByName.BeamDose)
    DicomUtil.removeAllInTree(rtplan, TagByName.BeamMeterset)
    DicomUtil.removeAllInTree(rtplan, TagByName.VarianCreator3249)
    DicomUtil.removeAllInTree(rtplan, TagByName.MaximumTreatmentTime)
    DicomUtil.removeAllInTree(rtplan, TagByName.ReferencedPrimaryDoseRefUID)
    DicomUtil.removeAllInTree(rtplan, TagByName.ReferencedDoseReferenceSequence)

  }

  override def makeRtplan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, procName: String): AttributeList = {

    val rtplan = super.makeRtplan(machine, userPK, planSpecification, procName)

    removeExtraneousBeams(rtplan)

    renameToPhase3(rtplan)
    addStakittBeam(rtplan = rtplan, beam = fetchStakittBeam())

    CustomizeRtPlanUtil.orderBeamsByRenaming(rtplan)

    CustomizeRtPlanUtil.setNumberOfBeamsInFractionGroupSequence(rtplan)
    removeDose(rtplan)
    planSpecification.setOverrides(rtplan)
    fixRtplanGeometry(rtplan)
    CustomizeRtPlanUtil.replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    rtplan
  }

}
