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
