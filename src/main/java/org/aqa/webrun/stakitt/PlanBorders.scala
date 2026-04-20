package org.aqa.webrun.stakitt

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.Util

/**
  * List of planned leaf ands and leaf boundaries (sides).
  * @param xLeafEndList List of leaf ends (x coordinates).
  * @param yLeafBoundaryList List of leaf boundaries (x coordinates).
  */

case class PlanBorders(xLeafEndList: Seq[Double], yLeafBoundaryList: Seq[Double]) extends Logging {
  override def toString: String = {
    s"leafEndList ${xLeafEndList.size} : " + xLeafEndList.map(Util.fmtDbl).mkString("  ") + "\n" +
      s"leafBorderList ${yLeafBoundaryList.size} : " + yLeafBoundaryList.map(Util.fmtDbl).mkString("  ")
  }
}

object PlanBorders extends Logging {

  /**
    * Make a list of all planned edges for comparison to those measured in the image.
    * @param rtimage DICOM image.
    * @param rtplan RTPLAN for DICOM image.
    * @return
    */
  def make(rtimage: AttributeList, rtplan: AttributeList): PlanBorders = {

    // beam's attribute list
    val beam: AttributeList = Util.getBeamOfRtimage(rtplan, rtimage).get

    /**
      * Make a list of planned edges, including the lo and hi staggered edges.
      * @return List of edges.
      */
    def makeXEdgeList(): Seq[Double] = {
      // make a list of all leaf ends
      val sortedEdgeList = DicomUtil.findAllTag(beam, TagByName.LeafJawPositions).map(_.getDoubleValues).filter(_.length > 2).flatten.distinct.sorted

      val measuredEdgeList = sortedEdgeList.drop(2).dropRight(2)

      measuredEdgeList
    }

    /**
      * Make a list of all Y edges.
      * @return
      */
    def makeYEdgeList(): Seq[Double] = {

      def toSeq(attr: Attribute): Seq[AttributeList] = DicomUtil.alOfSeq(attr.asInstanceOf[SequenceAttribute])

      val positionSequenceList = DicomUtil.findAllTag(beam, TagByName.BeamLimitingDevicePositionSequence).flatMap(toSeq)

      def isYJaw(al: AttributeList): Boolean = {
        val name = al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString().trim
        name.equalsIgnoreCase("Y") || name.equalsIgnoreCase("ASYMY")
      }

      val yJawPositionList = positionSequenceList.filter(isYJaw).flatMap(_.get(TagByName.LeafJawPositions).getDoubleValues)

      val yMin = yJawPositionList.min
      val yMax = yJawPositionList.max

      val allLeafPositionBoundaries = DicomUtil.findAllTag(beam, TagByName.LeafPositionBoundaries).flatMap(_.getDoubleValues).distinct
      val visibleLeafBoundaries = allLeafPositionBoundaries.map(b => Math.clamp(b, yMin, yMax)).distinct.sorted
      visibleLeafBoundaries
    }

    val planBorders = PlanBorders(makeXEdgeList(), makeYEdgeList())

    planBorders
  }

}
