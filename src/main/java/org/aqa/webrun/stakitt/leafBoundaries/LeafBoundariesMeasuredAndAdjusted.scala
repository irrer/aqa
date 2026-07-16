package org.aqa.webrun.stakitt.leafBoundaries

import edu.umro.ImageUtil.DicomImage

/**
  * Results for the measurements of one side of a Stakitt image.
  * @param peakValleyMeanList_pix List of boundary (side) measurements calculated each found by finding the midpoint between
  *                               an adjacent peak and valley. The boundaries at the ends of the profile do not have both a
  *                               peak and a valley, so they are extrapolated from the adjacent boundaries. This is mostly
  *                               for diagnostics and debugging.
  *
  * @param adjusted_pix List boundaries derived from the <code>peakValleyMeanList_pix</code> list without the extrapolated
  *                     values. The problem is that the profile is asymmetric, in that the peaks are 'pointier' than the
  *                     valleys, which produces a staggered result of wide-narrow boundaries. The derivation process is to
  *                     take small groups of boundaries and use the least squares method to estimate what the actual
  *                     position of the boundary in center of the group should be.
  *
  * @param imageOfStaggeredLeaves Portion of image that was used to measure boundaries.
  *
  * @param name Indicates which side of the whole image is being represented.
  *
  * @param profileScaled Profile of <code>imageOfStaggeredLeaves</code> from Y2 to Y1 scaled from 0 to 100. This is mostly
  *                      for diagnostics and debugging.
  *
  * @param coarseVerticalFieldExtent The Y1 and Y2 (upper and lower) bounds of the whole field, as established by the Y1
  *                                  and Y2 jaws (or possibly collimator leaf boundaries).  This is the same for both the
  *                                  X1 and X2 profiles.
  */
case class LeafBoundariesMeasuredAndAdjusted( //
    peakValleyMeanList_pix: Seq[Double],
    adjusted_pix: Seq[Double],
    imageOfStaggeredLeaves: DicomImage,
    name: String,
    profileScaled: Seq[Float],
    coarseVerticalFieldExtent: CoarseVerticalFieldExtent
) {}
