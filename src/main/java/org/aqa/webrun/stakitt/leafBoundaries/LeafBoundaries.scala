package org.aqa.webrun.stakitt.leafBoundaries

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
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

  private val verticalFieldExtent: CoarseVerticalFieldExtent = CoarseVerticalFieldExtent(new DicomImage(rtimage))

  /** Inflection points on the left */
  val yPointListLo_pix: LeafBoundariesMeasuredAndAdjusted = FindLeafBoundaries(di.dicomImageLo, "X1", verticalFieldExtent).findLeafBoundaries_pix()

  /** Inflection points on the right */
  val yPointListHi_pix: LeafBoundariesMeasuredAndAdjusted = FindLeafBoundaries(di.dicomImageHi, "X2", verticalFieldExtent).findLeafBoundaries_pix()

}
