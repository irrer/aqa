package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage
import org.aqa.Logging

import java.awt.Rectangle

/**
  * Find the vertical limits of a Stakitt field.
  * @param image For this image.
  */
case class CoarseVerticalFieldExtent(image: DicomImage) extends Logging {

  // make an image of the vertical center of the original image.
  private val centerImage: DicomImage = {
    val quarterX = image.width / 4
    val halfX = image.width / 2
    val rect = new Rectangle(quarterX, 0, halfX, image.height)
    image.getSubimage(rect)
  }

  private val verticalProfile: Seq[Float] = {
    val p = centerImage.rowSums
    val min = p.min
    val rng = p.max - p.min
    p.map(v => (v - min) / rng)
  }

  // half of profile height
  private val mid: Double = {
    val sampleSize = 10 // take enough to get a representative sample
    val verticalProfileSorted: Seq[Float] = verticalProfile.sorted

    val lo = verticalProfileSorted.take(sampleSize).sum / sampleSize // lowest values
    val hi = verticalProfileSorted.takeRight(sampleSize).sum / sampleSize // highest values
    (lo + hi) / 2
  }

  private val halfHeight = centerImage.height / 2

  /**
    * Top edge of field in pixels.
    */
  val top_pix: Int = {
    val profile = verticalProfile.take(halfHeight).toIndexedSeq
    profile.indices.find(i => (profile(i) < mid) && (profile(i + 1) > mid)).get
  }

  /**
    * Bottom edge of field in pixels.
    */
  val bottom_pix: Int = {
    val profile = verticalProfile.drop(halfHeight).toIndexedSeq
    profile.indices.find(i => (profile(i) > mid) && (profile(i + 1) < mid)).get + halfHeight
  }

  /**
    * Halfway between top and bottom.
    */
  val mean_pix: Double = (top_pix + bottom_pix) / 2.0

}
