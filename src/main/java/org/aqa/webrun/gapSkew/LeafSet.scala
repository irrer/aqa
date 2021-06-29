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
    bottomRight: Leaf
) {

  // beam name
  val beamName: String = Phase2Util.getBeamNameOfRtimage(rtplan, attributeList).get

  override def toString: String = {
    "beam name: " + beamName
    "\n    topLeft: " + topLeft +
      "\n    topRight: " + topRight +
      "\n    bottomLeft: " + bottomLeft +
      "\n    bottomRight: " + bottomRight
  }
}
