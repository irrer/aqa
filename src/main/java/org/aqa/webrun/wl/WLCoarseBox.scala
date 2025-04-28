package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config

import java.awt.Rectangle

case class WLCoarseBox(rtimage: AttributeList) {

  private val trans = new IsoImagePlaneTranslator(rtimage)

  private case class StartAndLen(start: Int, len: Int) {}

  /**
    * Coarsely locate the edges of the box by looking for the
    * low areas in the given profile of the image.  The profile will
    * be either vertical or horizontal, and is the sum of all pixels
    * in that orientation.
    */
  private def coarseBoxLocate(profile: IndexedSeq[Float], resolution: Double => Double): StartAndLen = {

    val halfPenumbra = resolution(Config.WLBoxEdgeTolerance_mm).round.toInt

    val mid = (profile.max + profile.min) / 2

    val start = profile.indexWhere(_ >= mid) // get the start of the box
    val finish = profile.lastIndexWhere(_ >= mid) // get the finish of the box

    val startWithPenumbra = Math.max(start - halfPenumbra, 0) // start of box with penumbra, within bounds of image
    val finishWithPenumbra = Math.min(finish + halfPenumbra, profile.size - 1) // finish of box with penumbra, within bounds of image

    StartAndLen(startWithPenumbra, finishWithPenumbra - startWithPenumbra)
  }

  /**
   * Define a rectangle around the box that includes an extra border for the penumbra.
   * @return Bounds for area of interest.
   */
  def locate(): Rectangle = {
    val image = new DicomImage(rtimage)

    val xStartLen = coarseBoxLocate(image.columnSums, trans.iso2PixDistX)
    val yStartLen = coarseBoxLocate(image.rowSums, trans.iso2PixDistY)

    val rect = new Rectangle(xStartLen.start, yStartLen.start, xStartLen.len, yStartLen.len)

    rect
  }

}
