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

package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import org.aqa.db.GapSkew
import org.aqa.webrun.phase2.Phase2Util

import java.awt.image.BufferedImage

/**
  * Describe a set of leaf results.
  * @param image Image showing analysis results.
  * @param topLeft Positioned in top left.
  * @param topRight Positioned in top right.
  * @param bottomLeft Positioned in bottom left.
  * @param bottomRight Positioned in bottom right.
  */
case class LeafSet(
    image: BufferedImage,
    attributeList: AttributeList,
    rtplan: AttributeList,
    leafPositionRtplanTop_mm: Double,
    leafPositionRtplanBottom_mm: Double,
    topLeft: Leaf,
    topRight: Leaf,
    bottomLeft: Leaf,
    bottomRight: Leaf,
    gapSkew: GapSkew
) {

  /** Beam name. */
  val beamName: String = Phase2Util.getBeamNameOfRtimage(rtplan, attributeList).get

  /** Distance in mm from center of left measurement to center of right measurement. */
  val measurementLength_mm: Double = bottomRight.xLeftPosition_mm - bottomLeft.xLeftPosition_mm

  /** Skew in degrees due to difference in top pair of edges. */
  val topSkew_deg: Double = {
    val delta_mm = topRight.yPosition_mm - topLeft.yPosition_mm
    val skew = Math.acos(delta_mm / measurementLength_mm)
    skew
  }

  /** Skew in degrees due to difference in bottom pair of edges. */
  val bottomSkew_deg: Double = {
    val delta_mm = bottomRight.yPosition_mm - bottomLeft.yPosition_mm
    val skew = Math.acos(delta_mm / measurementLength_mm)
    skew
  }

  override def toString: String = {
    def fmt(d: Double) = d.formatted("%10.4f")
    def fmtAngle(d: Double) = d.formatted("%10.7f")
    "beam name: " + beamName +
      "    topSkew_deg: " + fmtAngle(topSkew_deg) +
      "    top delta: " + fmt(topLeft.yPosition_mm - topRight.yPosition_mm) +
      "    topLeft: " + fmt(topLeft.yPosition_mm) +
      "    topRight: " + fmt(topRight.yPosition_mm) +
      "    bottomSkew_deg: " + fmtAngle(bottomSkew_deg) +
      "    bottom delta: " + fmt(bottomLeft.yPosition_mm - bottomRight.yPosition_mm) +
      "    bottomLeft: " + fmt(bottomLeft.yPosition_mm) +
      "    bottomRight: " + fmt(bottomRight.yPosition_mm)
  }
}
