package org.aqa.webrun.phase2.leafPosition

import org.aqa.Logging
import scala.xml.Elem
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.db.LeafPosition
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import edu.umro.ImageUtil.DicomImage
import org.aqa.IsoImagePlaneTranslator
import java.awt.geom.Point2D
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.LocateRidge
import java.awt.geom.Rectangle2D
import java.awt.Rectangle
import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageUtil

object LeafPositionAnalysis extends Logging {

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  /**
   * Get a sorted list of all the distinct leaf ends in mm as isoplane coordinates.
   */
  private def leafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
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

  /**
   * Get the sorted, distinct leaf position boundaries (positions of sides of leaves) from the plan for this beam in isoplane mm.
   */
  private def LeafPositionBoundaries(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = {
    val BeamLimitingDeviceSequence = Util.seq2Attr(Phase2Util.getBeamSequenceOfPlan(beamName, plan), TagFromName.BeamLimitingDeviceSequence)
    //val BeamLimitingDevicePositionSequence = BeamLimitingDeviceSequence.map(cps => Util.seq2Attr(cps, TagFromName.BeamLimitingDevicePositionSequence)).flatten
    def getLeafPositionBoundaries(bldps: AttributeList): Seq[Double] = {
      val at = bldps.get(TagFromName.LeafPositionBoundaries)
      if (at == null)
        Seq[Double]()
      else
        at.getDoubleValues.toSeq
    }
    val LeafPositionBoundaries = BeamLimitingDeviceSequence.map(bldps => getLeafPositionBoundaries(bldps)).flatten.distinct.sorted
    LeafPositionBoundaries
  }

  /** Hook for testing */
  def testLeafEnds(horizontal: Boolean, beamName: String, plan: AttributeList): Seq[Double] = leafEnds(horizontal, beamName, plan)

  /**
   * Given the coarsely approximated leaf side positions, locate them more precisely using cubic interpolation.
   *
   * @return Values returned are in pixels.
   */
  private def preciseLeafSides(coarseList_pix: Seq[Double], horizontal: Boolean, profile: IndexedSeq[Float], dicomImage: DicomImage): Seq[Double] = {

    val searchDistance_pix = {
      val minWidth = coarseList_pix.tail.zip(coarseList_pix.dropRight(1)).map(ab => (ab._1 - ab._2).abs).min
      (minWidth / 3).round.toInt
    }

    def coarseToPrecise(coarse_pix: Double): Double = {
      val y = coarse_pix.toInt
      val range = (y - searchDistance_pix until y + searchDistance_pix)
      val indicies = range.map(y => y.toDouble).toArray
      val data = range.map(y => profile(y).toDouble).toArray
      val max = ImageUtil.profileMaxCubic(indicies, data)
      max
    }

    val comList = coarseList_pix.map(cp => coarseToPrecise(cp))
    comList
  }

  /**
   * Get the list of leaf widths.  If all leaves are the same width, then this will return a list with one member.
   */
  def getLeafWidthList_mm(leafSideList_mm: Seq[Double]) = {
    leafSideList_mm.dropRight(1).zip(leafSideList_mm.drop(1)).map(ab => (ab._1 - ab._2).abs).distinct.sorted
  }

  private def leafSides(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList): Seq[Double] = {

    val leafWidthList_mm = getLeafWidthList_mm(LeafPositionBoundaries(horizontal, beamName, plan))

    val profile = if (horizontal) dicomImage.rowSums else dicomImage.columnSums
    val coarseList_pix = LeafPositionCoarseLeafSides.coarseLeafSides(horizontal, profile, imageAttrList, leafWidthList_mm.head, leafWidthList_mm.last, dicomImage)

    val precise = preciseLeafSides(coarseList_pix, horizontal, profile, dicomImage)
    precise
  }

  /**
   * Hook for testing
   */
  def testLeafSides(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList): Seq[Double] =
    leafSides(horizontal, beamName, imageAttrList, dicomImage, plan)

  /**
   * Get a precise measurement of the given end.  All parameters are in pixel coordinates.
   *
   * @param topSide: Top side of leaf.
   *
   * @param bottomSide: Bottom side of leaf.
   *
   * @param end: Expected leaf position.
   */
  private def measureEnd(minorSide: Double, majorSide: Double, horizontal: Boolean, end: Double, pixelArray: IndexedSeq[IndexedSeq[Float]], XminLeafWidth_pix: Double): Double = {
    val minLeafWidth_pix = 22.0 * 2 // TODO
    // make a rectangle around the area of interest
    val measuredEndPosition_pix: Double = {
      if (horizontal) {
        val rectangle = new Rectangle2D.Double(end - minLeafWidth_pix / 2, minorSide, minLeafWidth_pix, majorSide - minorSide)
        LocateRidge.locateVertical(pixelArray, rectangle)
      } else {
        val rectangle = new Rectangle2D.Double(minorSide, end - minLeafWidth_pix / 2, majorSide - minorSide, minLeafWidth_pix)
        LocateRidge.locateHorizontal(pixelArray, rectangle)
      }
    }
    measuredEndPosition_pix
  }

  /**
   * Calculate the leaf end positions in the given beam.
   */
  private def measureBeam(beamName: String, outputPK: Long, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList): Seq[LeafPosition] = {

    // true if collimator is horizontal
    val horizontal = Phase2Util.isHorizontal(imageAttrList)
    val translator = new IsoImagePlaneTranslator(imageAttrList)
    val SOPInstanceUID = Util.sopOfAl(imageAttrList)

    val leafSideList = leafSides(horizontal, beamName, imageAttrList, dicomImage, plan).sorted // sort just to make sure
    val leafEndList_mm = leafEnds(horizontal, beamName, plan)
    val leafEndList_pix = leafEndList_mm.map(e => if (horizontal) translator.iso2PixCoordX(e) else translator.iso2PixCoordY(e))

    def x2mm(pix: Double) = if (horizontal) translator.pix2IsoCoordX(pix) else translator.pix2IsoCoordY(pix)
    def y2mm(pix: Double) = if (horizontal) translator.pix2IsoCoordY(pix) else translator.pix2IsoCoordX(pix)

    /**
     * Make a row for the database
     */
    def makeLeafPosition(measuredEndPosition_pix: Double, leafPositionIndex: Int, leafIndex: Int): LeafPosition = {

      val measuredEndPosition_mm = x2mm(measuredEndPosition_pix)
      val expectedEndPosition_mm = leafEndList_mm(leafPositionIndex)
      val offset_mm = measuredEndPosition_mm - expectedEndPosition_mm
      val measuredMinorSide_mm = y2mm(leafSideList(leafIndex))
      val measuredMajorSide_mm = y2mm(leafSideList(leafIndex + 1))

      val lp = new LeafPosition(
        None, //   leafPositionPK
        outputPK,
        SOPInstanceUID,
        beamName,
        leafIndex,
        leafPositionIndex,
        offset_mm, // difference from expected location: measuredEndPosition_mm - expectedEndPosition_mm
        measuredEndPosition_mm,
        expectedEndPosition_mm,
        measuredMinorSide_mm,
        measuredMajorSide_mm)

      lp
    }

    val pixelArray = dicomImage.getSubArray(new Rectangle(0, 0, dicomImage.width, dicomImage.height))

    val minLeafWidth_mm = 5.0 // TODO
    val leafPositionList = for (leafIndex <- leafSideList.indices.dropRight(1); leafPositionIndex <- leafEndList_pix.indices) yield {
      val measuredEndPosition_pix = measureEnd(leafSideList(leafIndex), leafSideList(leafIndex + 1), horizontal, leafEndList_pix(leafPositionIndex), pixelArray, minLeafWidth_mm)
      makeLeafPosition(measuredEndPosition_pix, leafPositionIndex, leafIndex)
    }
    leafPositionList
  }

  /**
   * Hook for testing
   */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage, plan: AttributeList): Seq[LeafPosition] =
    measureBeam(beamName, outputPK, attributeList, image, plan)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, LeafPositionResult] = {
    ???
  }

}