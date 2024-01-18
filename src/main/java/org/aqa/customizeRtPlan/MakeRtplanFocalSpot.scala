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
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.customizeRtPlan.phase3plan.Beam
import org.aqa.customizeRtPlan.phase3plan.SPFocalSpot
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.restlet.Response

class MakeRtplanFocalSpot extends MakeRtplan {

  /**
    * Set the geometry to TREATMENT_DEVICE
    *
    * @param rtplan Modify this plan.
    */
  private def setRtplanGeometry(rtplan: AttributeList): Unit = {

    def setGeom(attr: Attribute): Unit = {
      attr.removeValues()
      attr.addValue("TREATMENT_DEVICE")
    }

    DicomUtil.findAllSingle(rtplan, TagByName.RTPlanGeometry).foreach(setGeom)
  }

  /**
    * Remove the referenced structure set, if there is one.
    *
    * @param rtplan Modify this plan.
    */
  private def removeReferencedStructureSetSequence(rtplan: AttributeList): Unit = {
    val attr = rtplan.get(TagByName.ReferencedStructureSetSequence)
    if (attr != null) {
      rtplan.remove(TagByName.ReferencedStructureSetSequence)
    }
  }

  override def name: String = "Focal Spot"

  override def planFileProcedureName: String = "FocalSpot"

  override def procedure: Procedure = Procedure.ProcOfFocalSpot.get

  private def addBeamList(rtplan: AttributeList, machine: Machine): Unit = {

    val energyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)

    def energySignature(beam: Beam): String = beam.beamEnergy.photonEnergy_MeV.get + " : " + beam.isFFF

    val prototypeList = Beam.beamsFromPlan(rtplan, machine).groupBy(energySignature).values.head

    val beamList = {

      val bl = energyList.flatMap(energy => SPFocalSpot.setEnergyOfSet(prototypeList, energy))

      /**
        * Sort so that all the non-FFF beams are first, and then the FFF beams.  Within that, beams are sorted by MV.
        * @param beam Beam to sort.
        * @return beam ordering
        */
      def sortSignature(beam: Beam): String = {
        val text = " FFF: %-5s".format(beam.isFFF.toString) + " MV: %05.1f".format(beam.beamEnergy.photonEnergy_MeV.get)
        text
      }

      val mlc090 = bl.filter(beam => SPFocalSpot.isMLC(beam.prototypeBeam) && (beam.colAngle_roundedDeg == 90))
      val mlc270 = bl.filter(beam => SPFocalSpot.isMLC(beam.prototypeBeam) && (beam.colAngle_roundedDeg == 270))
      val jaw090 = bl.filter(beam => SPFocalSpot.isJaw(beam.prototypeBeam) && (beam.colAngle_roundedDeg == 90))
      val jaw270 = bl.filter(beam => SPFocalSpot.isJaw(beam.prototypeBeam) && (beam.colAngle_roundedDeg == 270))

      val mlc090Sorted = mlc090.sortBy(sortSignature)
      val mlc270Sorted = mlc270.sortBy(sortSignature).reverse
      val jaw090Sorted = jaw090.sortBy(sortSignature).reverse
      val jaw270Sorted = jaw270.sortBy(sortSignature)

      val list = mlc090Sorted ++ mlc270Sorted ++ jaw270Sorted ++ jaw090Sorted

      list
    }

    val logText = {
      def beamToString(beam: Beam): String = {
        val mlcJaw = if (SPFocalSpot.isMLC(beam.prototypeBeam)) "MLC" else "Jaw"
        beam.toString + " | " + mlcJaw
      }
      "List of Focal Spot beams in the order that they will be delivered:\n" + beamList.map(beamToString).mkString("\n")
    }

    logger.info(logText)

    val fractionList = {
      val copy = DicomUtil.clone(rtplan)
      DicomUtil.seqToAttr(copy, TagByName.FractionGroupSequence).flatMap(fr => DicomUtil.seqToAttr(fr, TagByName.ReferencedBeamSequence))
    }

    val beamNameList = DicomUtil.findAllSingle(rtplan, TagByName.BeamName).map(attr => attr.getSingleStringValueOrNull).distinct

    beamNameList.foreach(beamName => CustomizeRtPlanUtil.removeBeamFromPlan(rtplan, beamName))

    def findProtoFraction(beam: Beam): AttributeList = {
      val beamNumber = beam.prototypeBeam.get(TagByName.BeamNumber).getIntegerValues.head
      fractionList.find(al => al.get(TagByName.ReferencedBeamNumber).getIntegerValues.head == beamNumber).get
    }

    beamList.foreach(beam => CustomizeRtPlanUtil.addBeam(rtplan, beam.beamEnergy, beam.prototypeBeam, findProtoFraction(beam), Some(beam.beamName)))
  }

  /**
    * Remove the references to patient setup.
    * @param rtplan From this plan.
    */
  private def removeReferencesToPatientSetupNumber(rtplan: AttributeList): Unit = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamAlList.map(beamAl => beamAl.remove(TagByName.ReferencedPatientSetupNumber))
  }

  override def makeRtplan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, procName: String): AttributeList = {
    val rtplan = super.makeRtplan(machine, userPK, planSpecification, procName)
    setRtplanGeometry(rtplan)
    removeReferencedStructureSetSequence(rtplan)
    CustomizeRtPlanUtil.removeVarianPrivateTagAttributes(rtplan)
    addBeamList(rtplan, machine)
    rtplan.remove(TagByName.DoseReferenceSequence)
    removeReferencesToPatientSetupNumber(rtplan)
    CustomizeRtPlanUtil.orderBeamsByRenaming(rtplan)
    rtplan
  }

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {
    super.showPlan(machine, userPK, planSpecification, response)
  }
}
