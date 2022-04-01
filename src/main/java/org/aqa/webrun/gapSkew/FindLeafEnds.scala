/*
 * Copyright 2022 Regents of the University of Michigan
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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.GapSkew
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util

import java.awt.Rectangle

case class FindLeafEnds(extendedData: ExtendedData, rtimage: AttributeList, minPixel: Float, maxPixel: Float, rtplan: AttributeList) extends Logging {

  /** True if collimator is horizontal. */
  private val isHorizontal: Boolean = Phase2Util.isHorizontal(rtimage)

  private val translator = new IsoImagePlaneTranslator(rtimage)

  private val dicomImage = new DicomImage(rtimage)

  private val collimatorAngle_deg: Double = Util.collimatorAngle(rtimage)
  private val collimatorAngleRounded_deg: Int = Util.angleRoundedTo90(collimatorAngle_deg)

  // The sorted list of leaf sides in the RTPLAN in pixels.
  private val leafSidesFromPlan_mm: Seq[Double] = {
    DicomUtil.findAllSingle(rtplan, TagByName.LeafPositionBoundaries).head.getDoubleValues.sorted
  }

  val edgesFromPlan: EdgesFromPlan.OrientedEdgePair = EdgesFromPlan.edgesFromPlan(rtimage, rtplan)

  /**
    * Given a bounding box that contains the leaf end and is between the sides of the leaf (roughly containing the
    * width of two leaves), determine where the end of the leaf is in pixels.
    *
    * @param box_pix A bounding box that contains the leaf end and is between the sides of the leaf in pixels.
    *            Note: This could be a Rectangle2D.Double so as to weight partial pixels on either side.
    * @return The position of the leaf in mm in isoplane.
    */
  private def endOfLeaf_iso(box_pix: Rectangle): Leaf = {
    if (isHorizontal) {
      throw new RuntimeException("endOfLeaf_pix: Not implemented for horizontally positioned collimator.")
    } else {
      val profile = dicomImage.getSubimage(box_pix).rowSums

      val leafEndY = LocateEdge.locateEdge(profile, (profile.min + profile.max) / 2)

      val endPosition_pix = box_pix.getY + leafEndY
      val endPosition_mm = {
        val y = -translator.pix2IsoCoordY(endPosition_pix)
        y // if (collimatorAngleRounded_deg == 270) y else -y
      }

      Leaf(endPosition_mm, translator.pix2IsoCoordX(box_pix.getX), translator.pix2IsoDistX(box_pix.getWidth))
    }
  }

  /**
    * The 4 measured positions.  Also the associated data required to construct the HTML.
    */
  val leafSet: LeafSet =
    if (isHorizontal)
      throw new RuntimeException("FindLeafSides : Not implemented for horizontally positioned collimator.")
    else {
      val leafWidth_mm = (leafSidesFromPlan_mm.head - leafSidesFromPlan_mm(1)).abs
      val leafEndFinding_pix = translator.iso2PixDistY(Config.GapSkewLeafEndPenumbra_mm)

      val height = leafEndFinding_pix

      val yJaws_mm = GapSkewUtil.yRtimageJawPositions_mm(rtimage)

      val yLeftJaw_mm = { // Y coordinate of Y jaw on right side of imaging field.  Will be Y1 or Y2 depending on the collimator angle
        if (collimatorAngleRounded_deg == 90)
          -yJaws_mm(1)
        else
          yJaws_mm.head
      }

      val yRightJaw_mm = { // Y coordinate of Y jaw on left side of imaging field.  Will be Y1 or Y2 depending on the collimator angle
        if (collimatorAngleRounded_deg == 90)
          -yJaws_mm.head
        else
          yJaws_mm(1)
      }

      val yLeftField_mm = translator.pix2IsoCoordX(0) // Y coordinate of left side of imaging field
      val yRightField_mm = translator.pix2IsoCoordX(dicomImage.width - 1) // Y coordinate of right side of imaging field

      // leftmost edge of left-hand bounding boxes
      val xLeftPosition_mm = {
        val min_mm =
          if (yLeftJaw_mm > yLeftField_mm)
            yLeftJaw_mm + Config.GapSkewPenumbraThickness_mm
          else
            yLeftField_mm

        val leftLeaf_mm = leafSidesFromPlan_mm.filter(_ >= min_mm).min
        val rightLeaf_mm = leafSidesFromPlan_mm.filter(_ > leftLeaf_mm).min
        val lft_mm = (leftLeaf_mm + rightLeaf_mm) / 2.0
        lft_mm
      }

      // width of left-handed bounding boxes
      val xLeftWidth_mm = {
        val min_mm = leafSidesFromPlan_mm.filter(_ <= (xLeftPosition_mm + Config.GapSkewMinimumMeasurementLength_mm)).max
        val leftLeaf_mm = leafSidesFromPlan_mm.filter(_ >= min_mm).min
        val rightLeaf_mm = leafSidesFromPlan_mm.filter(_ > leftLeaf_mm).min
        val lft_mm = (leftLeaf_mm + rightLeaf_mm) / 2.0
        lft_mm - xLeftPosition_mm
      }

      // rightmost edge of right-hand bounding boxes
      val xRightPosition_mm = {
        val max_mm =
          if (yRightJaw_mm < yRightField_mm)
            yRightJaw_mm - Config.GapSkewPenumbraThickness_mm
          else
            yRightField_mm

        val rightLeaf_mm = leafSidesFromPlan_mm.filter(_ <= max_mm).max
        val leftLeaf_mm = leafSidesFromPlan_mm.filter(_ < rightLeaf_mm).max
        val rt_mm = (rightLeaf_mm + leftLeaf_mm) / 2.0
        rt_mm
      }

      // width of right-handed bounding boxes
      val xRightWidth_mm = {
        val leftLeaf_mm = leafSidesFromPlan_mm.filter(_ <= (xRightPosition_mm - Config.GapSkewMinimumMeasurementLength_mm)).max
        val rightLeaf_mm = leafSidesFromPlan_mm.filter(_ > leftLeaf_mm).min
        val rt_mm = (leftLeaf_mm + rightLeaf_mm) / 2.0
        xRightPosition_mm - rt_mm
      }

      val leftRectPosition_pix = translator.iso2PixCoordX(xLeftPosition_mm)
      val leftRectWidth_pix = translator.iso2PixDistX(xLeftWidth_mm)

      val rightRectPosition_pix = translator.iso2PixCoordX(xRightPosition_mm - xRightWidth_mm)
      val rightRectWidth_pix = translator.iso2PixDistX(xRightWidth_mm)

      val yTop = edgesFromPlan.topOrLeft.get.position_mm
      val yBottom = edgesFromPlan.bottomOrRight.get.position_mm

      val yTop_pix = translator.iso2PixCoordY(-yTop) - height / 2
      val yBottom_pix = translator.iso2PixCoordY(-yBottom) - height / 2

      val topLeftRect = Util.rectD(leftRectPosition_pix, yTop_pix, leftRectWidth_pix, height)
      val topRightRect = Util.rectD(rightRectPosition_pix, yTop_pix, rightRectWidth_pix, height)
      val bottomLeftRect = Util.rectD(leftRectPosition_pix, yBottom_pix, leftRectWidth_pix, height)
      val bottomRightRect = Util.rectD(rightRectPosition_pix, yBottom_pix, rightRectWidth_pix, height)

      val beamName = Phase2Util.getBeamNameOfRtimage(plan = rtplan, rtimage).get
      logger.info("GapSkew processing beam: " + Util.beamNumber(rtimage) + " : " + beamName)

      val topLeft = endOfLeaf_iso(topLeftRect)
      val topRight = endOfLeaf_iso(topRightRect)
      val bottomLeft = endOfLeaf_iso(bottomLeftRect)
      val bottomRight = endOfLeaf_iso(bottomRightRect)

      val bufferedImage =
        GapSkewAnnotateImage(
          dicomImage,
          collimatorAngleRounded_deg,
          beamName = beamName,
          translator,
          minPixel = minPixel,
          maxPixel = maxPixel,
          topLeft = topLeft,
          topRight = topRight,
          bottomLeft = bottomLeft,
          bottomRight = bottomRight
        ).annotate

      val measurementSeparation_mm = (xRightPosition_mm - xRightWidth_mm / 2.0) - (xLeftPosition_mm + xLeftWidth_mm / 2.0)

      val gapSkew = GapSkew(
        gapSkewPK = None,
        outputPK = extendedData.output.outputPK.get,
        rtimageUID = Util.sopOfAl(rtimage),
        beamName = beamName,
        collimatorAngle_deg = collimatorAngle_deg,
        measurementWidth_mm = leafWidth_mm * 2,
        measurementSeparation_mm = measurementSeparation_mm,
        //
        topLeftEdgeTypeName = Some(edgesFromPlan.topOrLeft.get.edgeType.name),
        topLeftValue_mm = Some(topLeft.yPosition_mm),
        topLeftPlanned_mm = Some(edgesFromPlan.topOrLeft.get.position_mm),
        //
        topRightEdgeTypeName = Some(edgesFromPlan.topOrLeft.get.edgeType.name),
        topRightValue_mm = Some(topRight.yPosition_mm),
        topRightPlanned_mm = Some(edgesFromPlan.topOrLeft.get.position_mm),
        //
        bottomLeftEdgeTypeName = Some(edgesFromPlan.bottomOrRight.get.edgeType.name),
        bottomLeftValue_mm = Some(bottomLeft.yPosition_mm),
        bottomLeftPlanned_mm = Some(edgesFromPlan.bottomOrRight.get.position_mm),
        //
        bottomRightEdgeTypeName = Some(edgesFromPlan.bottomOrRight.get.edgeType.name),
        bottomRightValue_mm = Some(bottomRight.yPosition_mm),
        bottomRightPlanned_mm = Some(edgesFromPlan.bottomOrRight.get.position_mm)
      )

      val set = LeafSet(
        bufferedImage,
        rtimage,
        rtplan,
        leafWidth_mm * 2,
        gapSkew
      )

      logger.info("MLC leaf end positions: " + set)
      set
    }
}
