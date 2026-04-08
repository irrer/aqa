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
  val yPointListLo_pix: Seq[Double] = FindBoundariesByMidpoints(di.dicomImageLo, "Lo").findLeafBoundaries()

  /** Inflection points on the right */
  val yPointListHi_pix: Seq[Double] = FindBoundariesByMidpoints(di.dicomImageHi, "Hi").findLeafBoundaries()

  /** List of Y coordinates that mark the edges of the leaf AOIs.  These are extracted by using the profile of the rows of pixels across the entire image. */
  val yPointList_pix: Seq[Double] = yPointListLo_pix

}
