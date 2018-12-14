package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import org.aqa.webrun.phase2.Phase2Util
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ScalaUtil.Trace

/**
 * General utilities for leaf position.
 */
object LeafPositionUtil extends Logging {

  /**
   * Get the sorted, distinct of all leaf position boundaries (positions of sides of leaves) from the plan for this beam in isoplane mm.
   */
  private def allLeafPositionBoundaries_mm(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val BeamLimitingDeviceSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.BeamLimitingDeviceSequence)
    def getLeafPositionBoundaries(bldps: AttributeList): Seq[Double] = {
      val at = bldps.get(TagFromName.LeafPositionBoundaries)
      if (at == null)
        Seq[Double]()
      else
        at.getDoubleValues.toSeq
    }
    val LeafPositionBoundaries_mm = BeamLimitingDeviceSequence.map(bldps => getLeafPositionBoundaries(bldps)).flatten.distinct.sorted
    LeafPositionBoundaries_mm
  }

  /**
   * Get a list of all the leaf sides (not ends) defined in the plan (in isoplane mm) that are not obscured by the jaws.
   */
  def listOfLeafPositionBoundariesInPlan_mm(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {

    val ControlPointSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.ControlPointSequence)
    val BeamLimitingDevicePositionSequence = ControlPointSequence.map(cps => Util.seq2Attr(cps, TagFromName.BeamLimitingDevicePositionSequence)).flatten

    def isJaw(BeamLimitingDevicePosition: AttributeList): Boolean = {
      val deviceType = BeamLimitingDevicePosition.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
      val requiredType = if (horizontal) "Y" else "X"
      deviceType.equalsIgnoreCase(requiredType)
    }

    val jawList = BeamLimitingDevicePositionSequence.filter(bldp => isJaw(bldp))
    val LeafJawPositions = jawList.map(jl => jl.get(TagFromName.LeafJawPositions).getDoubleValues).flatten

    val min = LeafJawPositions.min
    val max = LeafJawPositions.max

    val all = allLeafPositionBoundaries_mm(horizontal, beamName, plan)
    val exposed = all.filter(side => (side > min) && (side < max)).distinct.sorted
    exposed
  }

  /**
   * Get the list of leaf widths.  If all leaves are the same width, then this will return a list with one member.
   */
  def getLeafWidthList_mm(leafSideList_mm: Seq[Double]) = {
    leafSideList_mm.dropRight(1).zip(leafSideList_mm.drop(1)).map(ab => (ab._1 - ab._2).abs).distinct.sorted
  }

  /**
   * Get a sorted list of all the distinct leaf ends in mm as isoplane coordinates.
   */
  def leafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val ControlPointSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.ControlPointSequence)
    val withEnergy = ControlPointSequence.filter(cp => cp.get(TagFromName.CumulativeMetersetWeight).getDoubleValues.head != 0)
    val BeamLimitingDevicePositionSequence = withEnergy.map(cps => Util.seq2Attr(cps, TagFromName.BeamLimitingDevicePositionSequence)).flatten

    def isMlc(BeamLimitingDevicePosition: AttributeList): Boolean = {
      val deviceType = BeamLimitingDevicePosition.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString
      val requiredType = if (horizontal) "MLCX" else "MLCY"
      deviceType.equalsIgnoreCase(requiredType)
    }

    val endList = BeamLimitingDevicePositionSequence.filter(bldp => isMlc(bldp)).map(bldp => bldp.get(TagFromName.LeafJawPositions).getDoubleValues.head).distinct.sorted
    endList
  }

}