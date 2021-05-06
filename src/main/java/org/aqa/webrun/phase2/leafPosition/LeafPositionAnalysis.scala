package org.aqa.webrun.phase2.leafPosition

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateRidge
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.db.LeafPosition
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.Rectangle
import java.awt.geom.Rectangle2D
import scala.xml.Elem

/**
  * Perform analysis of leaf position (picket fence).
  */
object LeafPositionAnalysis extends Logging {

  /**
    * Locate the sides of the leaves in the given image and return a sorted list.  Values are in pixels.
    */
  def leafSides_pix(
      horizontal: Boolean,
      beamName: String,
      dicomImage: DicomImage,
      plan: AttributeList,
      translator: IsoImagePlaneTranslator
  ): Seq[Double] = {

    val sideListPlanned_pix = {
      val sideListPlan_mm = LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan, translator).sorted
      if (horizontal)
        sideListPlan_mm.map(side => translator.iso2PixCoordY(side))
      else
        sideListPlan_mm.map(side => translator.iso2PixCoordX(side))
    }

    val planIndices = sideListPlanned_pix.indices.toList

    val profile = if (horizontal) dicomImage.rowSums else dicomImage.columnSums

    // val leafWidthList_mm = LeafPositionUtil.getLeafWidthList_mm(LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan, translator))

    // val coarseSideList_pix = LeafPositionCoarseLeafSides.coarseLeafSides(horizontal, profile, imageAttrList, leafWidthList_mm.min, leafWidthList_mm.max, dicomImage)
    case class LeafSide(coarse_pix: Double) {
      val planIndex: Int = planIndices.minBy(i => (sideListPlanned_pix(i) - coarse_pix).abs)
      val planned: Double = sideListPlanned_pix(planIndex)

      val isBetweenPeaks: Boolean = (planIndex > 0) && (planIndex < planIndices.last)

      private def indexOf(offset: Int) = {
        ((planned + sideListPlanned_pix(planIndex + offset)) / 2).round.toInt
      }

      private def minorValleyIndex = indexOf(-1)
      private def majorValleyIndex = indexOf(1)

      private def minorValleyHeight = {
        profile(minorValleyIndex)
      }
      private def majorValleyHeight = profile(majorValleyIndex)

      /**
        * The score indicates how well defined the peak is, which is the difference between the height of this peak and the height of
        * the adjacent valleys.  There must be a peak on either side of this peak or it will get a score of -1.
        */
      val score: Double = {
        val coarseInt_pix = coarse_pix.round.toInt
        if (isBetweenPeaks && (coarseInt_pix >= 0) && (coarseInt_pix < profile.size) && (minorValleyIndex >= 0) && (majorValleyIndex < profile.size)) {
          val coarsePixHeight = profile(coarseInt_pix)
          val s = (coarsePixHeight * 2) - (minorValleyHeight + majorValleyHeight)
          s
        } else -1
      }

      lazy val precisePosition: Double = {
        try {
          val xList = profile.indices.slice(minorValleyIndex, minorValleyIndex + majorValleyIndex - minorValleyIndex).map(i => i.toDouble)
          val yList = profile.slice(minorValleyIndex, minorValleyIndex + majorValleyIndex - minorValleyIndex).map(y => y.toDouble)

          ImageUtil.profileMaxCubic(xList.toList.toArray, yList.toList.toArray)
        } catch {
          case _: Throwable => -1.0
        }
      }

      def adjustToNearest(bestList: Seq[LeafSide]): Double = {
        if (bestList.size < 2) {
          logger.error("Need at least 2 leaf sides for beam " + beamName + " but only found " + bestList.size)
        }

        val byCloseness = bestList.sortBy(b => (b.planned - planned).abs).take(2)
        val ca = byCloseness.head
        val cb = byCloseness(1)

        /** Determine scale by looking at measured vs planned distance. */
        val scale = {
          val measuredDst = ca.precisePosition - cb.precisePosition
          val plannedDst = ca.planned - cb.planned
          (measuredDst / plannedDst).abs
        }

        val plannedDist = planned - ca.planned
        val calculatedDist = plannedDist * scale

        val calculatedPosition = calculatedDist + ca.precisePosition

        calculatedPosition
      }

      override def toString: String = {
        "pix: " + coarse_pix.round.formatted("%5d") +
          "    precise: " + precisePosition.formatted("%8.3f") +
          "    leaf: " + planIndex.formatted("%2d") +
          "    isBetween: " + isBetweenPeaks.toString.formatted("%1s") +
          "    planned: " + Util.fmtDbl(planned) +
          "    score: " + Util.fmtDbl(score)
      }
    }

    val leafSidesList = sideListPlanned_pix.map(p => LeafSide(p)).sortBy(_.score)

    val usable = leafSidesList.filter(_.isBetweenPeaks).sortBy(_.coarse_pix)
    val groupSize = usable.size / 3
    val bestLower = usable.take(groupSize).maxBy(_.score)
    val bestUpper = usable.takeRight(groupSize).maxBy(_.score)
    val bestList = Seq(bestLower, bestUpper)

    val preciseList = leafSidesList.sortBy(_.coarse_pix).map(leaf => leaf.adjustToNearest(bestList))
    preciseList
  }

  /**
    * Get a precise measurement of the given end.  All parameters are in pixel coordinates.
    *
    * @param minorSide: Top side of leaf.
    *
    * @param majorSide: Bottom side of leaf.
    *
    * @param horizontal: Collimator orientation.  Horizontal means leaves move left <-> right.
    *
    * @param end: Expected leaf position.
    *
    * @param minLeafWidth_pix: Width of thinnest leaf in pixels.  Used to establish search distance from either side of leaf end.
    *
    * @param interLeafIsolation_pix: Distance in pixels between the rectangle defined for measurement and the measured side of the leaf.
    */
  private def measureEnd(
      minorSide: Double,
      majorSide: Double,
      horizontal: Boolean,
      end: Double,
      pixelArray: IndexedSeq[IndexedSeq[Float]],
      minLeafWidth_pix: Double,
      interLeafIsolation_pix: Double
  ): Double = {
    val width = pixelArray.head.size
    val height = pixelArray.size
    // make a rectangle around the area of interest
    val measuredEndPosition_pix: Double = {
      if (horizontal) {
        val rectangle = new Rectangle2D.Double(
          end - minLeafWidth_pix / 2,
          Seq(0.0, minorSide + interLeafIsolation_pix).max,
          minLeafWidth_pix,
          Seq(height - 1.0, majorSide - minorSide - interLeafIsolation_pix * 2).min
        )
        LocateRidge.locateVertical(pixelArray, rectangle)
      } else {
        val rectangle = new Rectangle2D.Double(
          Seq(0.0, minorSide + interLeafIsolation_pix).max,
          end - minLeafWidth_pix / 2,
          Seq(width - 1.0, majorSide - minorSide - interLeafIsolation_pix * 2).min,
          minLeafWidth_pix
        )
        LocateRidge.locateHorizontal(pixelArray, rectangle)
      }
    }
    measuredEndPosition_pix
  }

  private def getLeafSidesFromPlanAsPix(rtplan: AttributeList, translator: IsoImagePlaneTranslator): Seq[Double] = {
    def hasMLCX2Type(bld: AttributeList) = {
      val t = bld.get(TagByName.RTBeamLimitingDeviceType)
      //noinspection SpellCheckingInspection
      (t != null) && t.getSingleStringValueOrEmptyString.equals("MLCX2")
    }

    val BeamSequence = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    val BeamLimitingDeviceSequence = BeamSequence.flatMap(bs => DicomUtil.seqToAttr(bs, TagByName.BeamLimitingDeviceSequence))
    val leafBoundaryList_mm = BeamLimitingDeviceSequence.filter(hasMLCX2Type).head.get(TagByName.LeafPositionBoundaries).getDoubleValues.toSeq

    val leafBoundaryList_pix = leafBoundaryList_mm.map(lb_mm => translator.iso2PixCoordY(lb_mm)).sorted
    leafBoundaryList_pix
  }

  /**
    * Calculate the leaf end positions in the given beam.
    */
  private def measureBeam(beamName: String, outputPK: Long, imageAttrList: AttributeList, dicomImage: DicomImage, plan: AttributeList, collimatorCentering: CollimatorCentering): Seq[LeafPosition] = {

    // true if collimator is horizontal
    val horizontal = Phase2Util.isHorizontal(imageAttrList)
    val translator = new IsoImagePlaneTranslator(imageAttrList)
    val SOPInstanceUID = Util.sopOfAl(imageAttrList)

    val collCenterOffset = if (horizontal) collimatorCentering.xCollimatorCenter_mm else collimatorCentering.yCollimatorCenter_mm
    val leafEndList_mm = LeafPositionUtil.leafEnds(horizontal, beamName, plan).map(le => le + collCenterOffset)
    val leafEndList_pix = leafEndList_mm.map(e => if (horizontal) translator.iso2PixCoordX(e) else translator.iso2PixCoordY(e))

    val leafSideList_pix = {
      if (DicomUtil.isHalcyon(plan)) {
        val sideOffset = if (horizontal) collimatorCentering.yCollimatorCenter_mm else collimatorCentering.xCollimatorCenter_mm
        getLeafSidesFromPlanAsPix(plan, translator).map(ls => ls + sideOffset)
      } else
        leafSides_pix(horizontal, beamName, dicomImage, plan, translator).sorted // sort just to make sure
    }

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
        measuredMajorSide_mm
      )

      lp
    }

    val pixelArray = dicomImage.getSubArray(new Rectangle(0, 0, dicomImage.width, dicomImage.height))
    val minLeafWidth_mm = LeafPositionUtil.getLeafWidthList_mm(LeafPositionUtil.listOfLeafPositionBoundariesInPlan_mm(horizontal, beamName, plan, translator)).min
    val minLeafWidth_pix = if (horizontal) translator.iso2PixDistY(minLeafWidth_mm) else translator.iso2PixDistY(minLeafWidth_mm)

    val interLeafIsolation_pix = if (horizontal) translator.iso2PixDistY(Config.LeafPositionIsolationDistance_mm) else translator.iso2PixDistX(Config.LeafPositionIsolationDistance_mm)

    val leafPositionList = for (leafIndex <- leafSideList_pix.indices.dropRight(1); leafPositionIndex <- leafEndList_pix.indices) yield {
      val measuredEndPosition_pix = measureEnd(
        leafSideList_pix(leafIndex),
        leafSideList_pix(leafIndex + 1),
        horizontal,
        leafEndList_pix(leafPositionIndex),
        pixelArray,
        minLeafWidth_pix,
        interLeafIsolation_pix
      )
      makeLeafPosition(measuredEndPosition_pix, leafPositionIndex, leafIndex)
    }
    leafPositionList
  }

  /**
    * Hook for testing
    */
  def testMeasureBeam(beamName: String, outputPK: Long, attributeList: AttributeList, image: DicomImage, plan: AttributeList, collimatorCentering: Option[CollimatorCentering]): Seq[LeafPosition] = {
    if (collimatorCentering.isDefined) measureBeam(beamName, outputPK, attributeList, image, plan, collimatorCentering.get)
    else Seq[LeafPosition]()
  }

  case class LeafPositionResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[LeafPosition]) extends SubProcedureResult(sum, sts, subProcedureName)

  val subProcedureName = "Leaf Position"

  case class BeamResults(beamName: String, resultList: Seq[LeafPosition]) {
    val pass: Boolean = resultList.forall(_.pass)
    val status: ProcedureStatus.ProcedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
  }

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, LeafPositionResult] = {

    try {
      logger.info("Starting analysis of " + subProcedureName + "  for machine " + extendedData.machine.id)
      val planAttrList = runReq.rtplan

      val outputPK = extendedData.output.outputPK.get

      val beamNameList = Config.LeafPositionBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))

      val beamResultList = beamNameList.map(beamName =>
        BeamResults(
          beamName,
          measureBeam(beamName, outputPK, runReq.derivedMap(beamName).attributeList, runReq.derivedMap(beamName).pixelCorrectedImage, planAttrList, collimatorCentering)
        )
      )

      val resultList = beamResultList.flatMap(_.resultList)

      logger.info("Analyzed " + beamResultList.size + " beams producing " + resultList.size + " leaf position measurements.")

      LeafPosition.insertSeq(resultList)

      // make sure all were processed and that they all passed
      val pass = beamResultList.map(_.pass).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      logger.info("Making HTML for " + subProcedureName)
      val elem = LeafPositionHTML.makeDisplay(extendedData, runReq, beamResultList, pass) // TODO this takes 13 seconds.  Run in parallel?
      val pcr = Right(LeafPositionResult(elem, procedureStatus, resultList))
      logger.info("Finished analysis of " + subProcedureName + "  for machine " + extendedData.machine.id)
      pcr
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of Metadata: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }

}
