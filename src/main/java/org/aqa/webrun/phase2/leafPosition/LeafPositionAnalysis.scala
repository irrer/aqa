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
//import java.awt.geom.Point2D
//import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import edu.umro.ImageUtil.LocateRidge
import java.awt.geom.Rectangle2D
import java.awt.Rectangle
//import edu.umro.ScalaUtil.Trace
import edu.umro.ImageUtil.ImageUtil
import org.aqa.Config

/**
 * Perform analysis of leaf position (picket fence).
 */
object LeafPositionAnalysis extends Logging {

  /**
   * Locate the sides of the leaves in the given image and return a sorted list.  Values are in pixels.
   */
  def XleafSides_pix(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage,
    plan: AttributeList, translator: IsoImagePlaneTranslator,
    leafEndList_pix: Seq[Double]): Seq[Double] = {

    val sideListPlan = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan).sorted

    val sideListPix = {
      if (horizontal)
        sideListPlan.map(side => translator.iso2PixCoordY(side))
      else
        sideListPlan.map(side => translator.iso2PixCoordY(side))
    }

    val minLeafWidth = sideListPix.indices.tail.map(i => sideListPix(i) - sideListPix(i - 1)).min
    val searchMargin = minLeafWidth / 2

    val lo = (leafEndList_pix.head - searchMargin).round.toInt
    val hi = (leafEndList_pix.last + searchMargin).round.toInt
    val narrow = minLeafWidth.round.toInt
    val wide = (leafEndList_pix.last - leafEndList_pix.head + searchMargin).round.toInt

    def positionOf(side: Int): Double = {
      if (horizontal) {
        val x = lo
        val y = (side - searchMargin).round.toInt
        val rect = new Rectangle(x, y, wide, narrow)
        val profile = dicomImage.getSubArray(rect).map(row => row.sum).map(f => f.toDouble).toArray
        val indicies = (0 until narrow).map(i => i.toDouble + y).toArray
        val max = ImageUtil.profileMaxCubic(indicies, profile)
        max
      } else {
        val y = lo
        val x = (side - searchMargin).round.toInt
        val rect = new Rectangle(lo, y, wide, narrow)
        val pixelMatrix = dicomImage.getSubArray(rect)
        def colSum(x: Int) = { for (y <- pixelMatrix.indices) yield { pixelMatrix(y)(x) } }.sum
        val profile = (0 until wide).map(x => colSum(x)).map(f => f.toDouble).toArray
        val indicies = (0 until narrow).map(i => i.toDouble + x).toArray
        val max = ImageUtil.profileMaxCubic(indicies, profile)
        max
      }
    }

    val sideList = sideListPix.map(side => positionOf(side.round.toInt))

    sideList
  }

  /**
   * Locate the sides of the leaves in the given image and return a sorted list.  Values are in pixels.
   */
  def leafSides_pix(horizontal: Boolean, beamName: String, imageAttrList: AttributeList, dicomImage: DicomImage,
    plan: AttributeList, translator: IsoImagePlaneTranslator,
    leafEndList_pix: Seq[Double]): Seq[Double] = {

    val sideListPlan = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan).sorted

    val sideListPix = {
      if (horizontal)
        sideListPlan.map(side => translator.iso2PixCoordY(side))
      else
        sideListPlan.map(side => translator.iso2PixCoordY(side))
    }
    sideListPix
  }

  /**
   * Get a precise measurement of the given end.  All parameters are in pixel coordinates.
   *
   * @param topSide: Top side of leaf.
   *
   * @param bottomSide: Bottom side of leaf.
   *
   * @param end: Expected leaf position.
   *
   * @param minLeafWidth_pix: Width of thinnest leaf in pixels.  Used to establish search distance from either side of leaf end.
   *
   * @param interleafMargin_pix: Distance in pixels between the rectangle defined for measurement and the measured side of the leaf.
   */
  private def measureEnd(minorSide: Double, majorSide: Double, horizontal: Boolean, end: Double,
    pixelArray: IndexedSeq[IndexedSeq[Float]], minLeafWidth_pix: Double, interleafIsolation_pix: Double): Double = {
    // make a rectangle around the area of interest
    val searchDistancePix = minLeafWidth_pix * 2
    val measuredEndPosition_pix: Double = {
      if (horizontal) {
        val rectangle = new Rectangle2D.Double(end - minLeafWidth_pix / 2, minorSide + interleafIsolation_pix, minLeafWidth_pix, majorSide - minorSide - interleafIsolation_pix * 2)
        LocateRidge.locateVertical(pixelArray, rectangle)
      } else {
        val rectangle = new Rectangle2D.Double(minorSide + interleafIsolation_pix, end - minLeafWidth_pix / 2, majorSide - minorSide - interleafIsolation_pix * 2, minLeafWidth_pix)
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

    val leafEndList_mm = LeafPositionUtil.leafEnds(horizontal, beamName, plan)
    val leafEndList_pix = leafEndList_mm.map(e => if (horizontal) translator.iso2PixCoordX(e) else translator.iso2PixCoordY(e))

    val leafSideList_pix = leafSides_pix(horizontal, beamName, imageAttrList, dicomImage, plan, translator, leafEndList_pix).sorted // sort just to make sure

    //    getLeafWidthList_mm(leafSideList_mm)

    def x2mm(pix: Double) = if (horizontal) translator.pix2IsoCoordX(pix) else translator.pix2IsoCoordY(pix)
    def y2mm(pix: Double) = if (horizontal) translator.pix2IsoCoordY(pix) else translator.pix2IsoCoordX(pix)

    /**
     * Make a row for the database
     */
    def makeLeafPosition(measuredEndPosition_pix: Double, leafPositionIndex: Int, leafIndex: Int): LeafPosition = {

      val measuredEndPosition_mm = x2mm(measuredEndPosition_pix)
      val expectedEndPosition_mm = leafEndList_mm(leafPositionIndex)
      val offset_mm = measuredEndPosition_mm - expectedEndPosition_mm
      val status = if (offset_mm.abs > Config.LeafPositionMaxError_mm) ProcedureStatus.fail else ProcedureStatus.pass
      val measuredMinorSide_mm = y2mm(leafSideList_pix(leafIndex))
      val measuredMajorSide_mm = y2mm(leafSideList_pix(leafIndex + 1))

      val lp = new LeafPosition(
        None, //   leafPositionPK
        outputPK,
        SOPInstanceUID,
        beamName,
        leafIndex + 1, // +1 for 1 relative indexing
        leafPositionIndex + 1, // +1 for 1 relative indexing
        offset_mm, // difference from expected location: measuredEndPosition_mm - expectedEndPosition_mm
        status.toString,
        measuredEndPosition_mm,
        expectedEndPosition_mm,
        measuredMinorSide_mm,
        measuredMajorSide_mm)

      lp
    }

    val pixelArray = dicomImage.getSubArray(new Rectangle(0, 0, dicomImage.width, dicomImage.height))
    val minLeafWidth_mm = LeafPositionUtil.getLeafWidthList_mm(LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan)).min
    val minLeafWidth_pix = if (horizontal) translator.iso2PixDistY(minLeafWidth_mm) else translator.iso2PixDistY(minLeafWidth_mm)

    val interleafIsolation_pix = if (horizontal) translator.iso2PixDistY(Config.LeafPositionIsolationDistance_mm) else translator.iso2PixDistX(Config.LeafPositionIsolationDistance_mm)

    val leafPositionList = for (leafIndex <- leafSideList_pix.indices.dropRight(1); leafPositionIndex <- leafEndList_pix.indices) yield {
      val measuredEndPosition_pix = measureEnd(leafSideList_pix(leafIndex), leafSideList_pix(leafIndex + 1),
        horizontal, leafEndList_pix(leafPositionIndex), pixelArray, minLeafWidth_pix, interleafIsolation_pix)
      makeLeafPosition(measuredEndPosition_pix, leafPositionIndex, leafIndex)
    }
    leafPositionList
  }

  /**
   * Hook for testing
   */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage, plan: AttributeList): Seq[LeafPosition] =
    measureBeam(beamName, outputPK, attributeList, image, plan)

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  case class BeamResults(beamName: String, resultList: Seq[LeafPosition]) {
    val pass = resultList.find(!_.pass).isEmpty
    val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
  }

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, LeafPositionResult] = {

    try {
      logger.info("Starting analysis of " + subProcedureName)
      val planAttrList = runReq.rtplan.attributeList.get

      val outputPK = extendedData.output.outputPK.get

      // val beamNameRtimageList = runReq.derivedMap.filter(img => Config.LeafPositionBeamNameList.contains(img._1)).toList
      val beamNameList = Config.LeafPositionBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))

      val beamResultList = beamNameList.map(beamName =>
        new BeamResults(beamName, measureBeam(beamName, outputPK, runReq.derivedMap(beamName).attributeList, runReq.derivedMap(beamName).pixelCorrectedImage, planAttrList)))

      val resultList = beamResultList.map(_.resultList).flatten

      logger.info("Analyzed " + beamResultList.size + " beams producing " + resultList.size + " leaf position measurements.")

      LeafPosition.insertSeq(resultList)

      // make sure all were processed and that they all passed
      val pass = beamResultList.map(_.pass).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      logger.info("Making HTML for " + subProcedureName)
      val elem = LeafPositionHTML.makeDisplay(extendedData, runReq, beamResultList, pass)
      val pcr = Right(new LeafPositionResult(elem, procedureStatus, resultList))
      logger.info("Finished analysis of " + subProcedureName)
      pcr
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of Metadata: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }

}