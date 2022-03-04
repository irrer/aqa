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

  // The sorted list of leaf sides in the RTPLAN in pixels.
  private val leafSidesFromPlanAsPix: Seq[Double] = {
    DicomUtil.findAllSingle(rtplan, TagByName.LeafPositionBoundaries).head.getDoubleValues.sorted
  }

  val edgesFromPlan: EdgesFromPlan.EndPair = EdgesFromPlan.edgesFromPlan(rtimage, rtplan)

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
      val endPosition_mm = -translator.pix2IsoCoordY(endPosition_pix)

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
      val leafWidth_mm = (leafSidesFromPlanAsPix.head - leafSidesFromPlanAsPix(1)).abs
      val leafWidth_pix = translator.iso2PixDistX(leafWidth_mm) // outer leaf width in pixels
      val leafSideFinding_pix = translator.iso2PixDistY(Config.GapSkewLeafSideFinding_mm)

      val height = leafSideFinding_pix
      val width = leafWidth_pix * 2
      val halfLeaf_pix = leafWidth_pix / 2

      val xLeft_pix = halfLeaf_pix
      val xRight_pix = dicomImage.width - halfLeaf_pix - 2 * leafWidth_pix

      val collimatorAngle = Util.angleRoundedTo90(DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head)
      val yTop = if (collimatorAngle == 90) edgesFromPlan.X2 else edgesFromPlan.X1
      val yBottom = if (collimatorAngle == 90) edgesFromPlan.X1 else edgesFromPlan.X2

      val yTop_pix = translator.iso2PixCoordY(-yTop.limit_mm) - height / 2
      val yBottom_pix = translator.iso2PixCoordY(-yBottom.limit_mm) - height / 2

      val topLeftRect = Util.rectD(xLeft_pix, yTop_pix, width, height)
      val topRightRect = Util.rectD(xRight_pix, yTop_pix, width, height)
      val bottomLeftRect = Util.rectD(xLeft_pix, yBottom_pix, width, height)
      val bottomRightRect = Util.rectD(xRight_pix, yBottom_pix, width, height)

      val beamName = Phase2Util.getBeamNameOfRtimage(plan = rtplan, rtimage).get
      logger.info("GapSkew processing beam: " + Util.beamNumber(rtimage) + " : " + beamName)

      val topLeft = endOfLeaf_iso(topLeftRect)
      val topRight = endOfLeaf_iso(topRightRect)
      val bottomLeft = endOfLeaf_iso(bottomLeftRect)
      val bottomRight = endOfLeaf_iso(bottomRightRect)

      val bufferedImage = GapSkewAnnotateImage(dicomImage, translator, minPixel, maxPixel, topLeft, topRight, bottomLeft, bottomRight).annotate

      val gapSkew = GapSkew(
        gapSkewPK = None,
        outputPK = extendedData.output.outputPK.get,
        rtplanSOPInstanceUID = Util.sopOfAl(rtplan),
        rtimageSeriesInstanceUID = Util.serInstOfAl(rtimage),
        rtimageUID = Util.sopOfAl(rtimage),
        beamName = beamName,
        topLeftX_mm = topLeft.xCenter_mm,
        topLeftY_mm = topLeft.yPosition_mm,
        topRightX_mm = topRight.xCenter_mm,
        topRightY_mm = topRight.yPosition_mm,
        topPlannedY_mm = yTop.limit_mm,
        topEdgeType = yTop.edgeType,
        bottomLeftX_mm = bottomLeft.xCenter_mm,
        bottomLeftY_mm = bottomLeft.yPosition_mm,
        bottomRightX_mm = bottomRight.xCenter_mm,
        bottomRightY_mm = bottomRight.yPosition_mm,
        bottomPlannedY_mm = yBottom.limit_mm,
        bottomEdgeType = yBottom.edgeType
      )

      val set = LeafSet(
        bufferedImage,
        rtimage,
        rtplan,
        yTop.limit_mm,
        yBottom.limit_mm,
        topLeft,
        topRight,
        bottomLeft,
        bottomRight,
        gapSkew
      )

      logger.info("MLC leaf end positions: " + set)
      set
    }
}
