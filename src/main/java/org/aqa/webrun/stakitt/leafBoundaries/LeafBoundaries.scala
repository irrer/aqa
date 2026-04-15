package org.aqa.webrun.stakitt.leafBoundaries

import com.pixelmed.dicom.AttributeList
import org.aqa.Logging

/**
  * Calculate the Y coordinates of the AOIs for the given Stakitt image.
  *
  * All calculations are done in pixels (as opposed to mm / isoplane).
  *
  * @param rtimage Stakitt image.
  * @param xBorderList List of X coordinates for AOIs.
  */
case class LeafBoundaries(rtimage: AttributeList, xBorderList: Seq[Double]) extends Logging {

  private val di = LBDicomImages(rtimage, xBorderList)

  /** Inflection points on the left */
  val yPointListLo_pix: LeafBoundariesMeasuredAndAdjusted = FindLeafBoundaries(di.dicomImageLo, "Lo").findLeafBoundaries_pix()

  /** Inflection points on the right */
  val yPointListHi_pix: LeafBoundariesMeasuredAndAdjusted = FindLeafBoundaries(di.dicomImageHi, "Hi").findLeafBoundaries_pix()

}
