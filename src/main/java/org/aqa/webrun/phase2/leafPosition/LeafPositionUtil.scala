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

package org.aqa.webrun.phase2.leafPosition

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.webrun.phase2.Phase2Util

/**
  * General utilities for leaf position.
  */
object LeafPositionUtil extends Logging {

  /**
    * Get the sorted, distinct of all leaf position boundaries (positions of sides of leaves) from the plan for this beam in isoplane mm.
    */
  private def allLeafPositionBoundaries_mm(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagByName.BeamLimitingDeviceSequence)
    def getLeafPositionBoundaries(bldps: AttributeList): Seq[Double] = {
      val at = bldps.get(TagByName.LeafPositionBoundaries)
      if (at == null)
        Seq[Double]()
      else
        at.getDoubleValues.toSeq
    }
    val LeafPositionBoundaries_mm = BeamLimitingDeviceSequence.flatMap(bldps => getLeafPositionBoundaries(bldps)).distinct.sorted
    LeafPositionBoundaries_mm
  }

  /**
    * Get the jaw boundaries parallel to the sides of the collimator leaves.
    */
  private def jawBoundaries(horizontal: Boolean, beamName: String, plan: AttributeList): (Double, Double) = {
    val ControlPointSequence = DicomUtil.seqToAttr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagByName.ControlPointSequence)
    val BeamLimitingDevicePositionSequence =
      ControlPointSequence.filter(cps => cps.get(TagByName.BeamLimitingDevicePositionSequence) != null).flatMap(cps => DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence))

    def isJaw(BeamLimitingDevicePosition: AttributeList): Boolean = {
      val deviceType = BeamLimitingDevicePosition.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString

      // val requiredType = if (horizontal) "X" else "Y"  // TODO : I think this is correct, but do not want to make the change without extensive testing.
      // deviceType.equalsIgnoreCase(requiredType) || deviceType.equalsIgnoreCase("ASYM" + requiredType)  // TODO Allow using asymmetric jaws. I think this is correct, but do not want to make the change without extensive testing.

      // TODO original code.  I think these two lines of code are wrong.
      val requiredType = if (horizontal) "Y" else "X"
      deviceType.equalsIgnoreCase(requiredType)
    }

    val jawList = BeamLimitingDevicePositionSequence.filter(bldp => isJaw(bldp))
    val LeafJawPositions = jawList.flatMap(jl => jl.get(TagByName.LeafJawPositions).getDoubleValues)

    (LeafJawPositions.min, LeafJawPositions.max)
  }

  /**
    * Get a list of all the leaf sides (not ends) defined in the plan (in isoplane mm) that are not obscured by the jaws and appear on the imager.
    */
  def listOfLeafPositionBoundariesInPlan_mm(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {

    val all = allLeafPositionBoundaries_mm(horizontal, beamName, plan)
    val jawBounds = jawBoundaries(horizontal, beamName, plan)
    // TODO exploring how to reject Halcyon leaves that are outside the field of view.
    //    val min = Math.max(jawBounds._1, trans.minImage_mm.getX)
    //    val max = Math.min(jawBounds._2, trans.maxImage_mm.getX)
    val min = jawBounds._1
    val max = jawBounds._2
    val exposed = all.filter(side => (side >= min) && (side <= max)).distinct.sorted
    exposed
  }

  /**
    * Get the list of leaf widths.  If all leaves are the same width, then this will return a list with one member.
    */
  def getLeafWidthList_mm(leafSideList_mm: Seq[Double]): Seq[Double] = {
    leafSideList_mm.dropRight(1).zip(leafSideList_mm.drop(1)).map(ab => (ab._1 - ab._2).abs).distinct.sorted
  }

  /**
    * Get a sorted list of all the distinct leaf ends in mm as isoplane coordinates.
    */
  def leafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {

    def meterWeightSetNonZero(ctrlPtSeq: AttributeList): Boolean = {
      val CumulativeMetersetWeight = ctrlPtSeq.get(TagByName.CumulativeMetersetWeight).getDoubleValues.head
      CumulativeMetersetWeight > 0
    }

    val ControlPointSequence = DicomUtil.seqToAttr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagByName.ControlPointSequence).filter(cps => meterWeightSetNonZero(cps))

    def isMLCX1(ctrlPtSeq: AttributeList): Boolean = {
      ctrlPtSeq.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.equals("MLCX1")
    }

    if (DicomUtil.isHalcyon(plan)) {
      def centerOfLeafJaw(al: AttributeList): Double = {
        val all = al.get(TagByName.LeafJawPositions).getDoubleValues
        (all.min + all.max) / 2.0
      }
      val bldsList = ControlPointSequence.flatMap(cps => DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)).filter(cps => isMLCX1(cps))

      val endList = bldsList.map(m => centerOfLeafJaw(m)).distinct.sorted
      endList
    } else {
      val withEnergy = ControlPointSequence.filter(cp => cp.get(TagByName.CumulativeMetersetWeight).getDoubleValues.head != 0)
      val BeamLimitingDevicePositionSequence = withEnergy.flatMap(cps => DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence))

      def isMlc(BeamLimitingDevicePosition: AttributeList): Boolean = {
        val deviceType = BeamLimitingDevicePosition.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
        val requiredType = if (horizontal) "MLCX" else "MLCY"
        deviceType.equalsIgnoreCase(requiredType)
      }

      def betweenLeaves(list: Array[Double]): Double = {
        val midPoint = list.sum / list.length
        if (list.length != 2)
          throw new RuntimeException("Leaf pair list should specify exactly 2 distinct values, but are: " + list.mkString(",  "))
        midPoint
      }

      val leafEndPairList = BeamLimitingDevicePositionSequence.filter(bldp => isMlc(bldp)).map(bldp => bldp.get(TagByName.LeafJawPositions).getDoubleValues.distinct.sorted)
      val endList = leafEndPairList.map(betweenLeaves).distinct
      logger.info("Leaf Position RTPLAN leaf end list: " + endList.mkString("  "))

      endList
    }
  }

}
