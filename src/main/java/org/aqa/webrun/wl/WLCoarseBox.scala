package org.aqa.webrun.wl

import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.awt.Rectangle

case class WLCoarseBox(image: DicomImage, trans: IsoImagePlaneTranslator, wlMsg: Option[WLMessage]) extends Logging {

  val columnSums: IndexedSeq[Float] = image.columnSums
  val rowSums: IndexedSeq[Float] = image.rowSums

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

    val start = profile.indexWhere(_ <= mid) // get the start of the box
    val finish = profile.lastIndexWhere(_ <= mid) // get the finish of the box

    val startWithPenumbra = Math.max(start - halfPenumbra, 0) // start of box with penumbra, within bounds of image
    val finishWithPenumbra = Math.min(finish + halfPenumbra, profile.size - 1) // finish of box with penumbra, within bounds of image

    StartAndLen(startWithPenumbra, finishWithPenumbra - startWithPenumbra)
  }

  /**
    * Define a rectangle around the box that includes an extra border for the penumbra.
    * @return Bounds for area of interest.
    */
  private def locate(): Rectangle = {
    val xStartLen = coarseBoxLocate(image.columnSums, trans.iso2PixDistX)
    val yStartLen = coarseBoxLocate(image.rowSums, trans.iso2PixDistY)

    val rect = new Rectangle(xStartLen.start, yStartLen.start, xStartLen.len, yStartLen.len)

    if (wlMsg.isDefined) {

      wlMsg.get.info(
        "Rectangle defining coarse box location in mm: " +
          "    left: " + Util.fmtDbl(trans.pix2IsoCoordX(rect.x)) +
          "    top " + Util.fmtDbl(trans.pix2IsoCoordY(rect.y)) +
          "    width: " + Util.fmtDbl(trans.pix2IsoDistX(rect.width)) +
          "    height: " + Util.fmtDbl(trans.pix2IsoDistY(rect.height))
      )

      wlMsg.get.info(
        "Rectangle defining coarse box center in mm: " +
          "    x: " + Util.fmtDbl(trans.pix2IsoCoordX(rect.x + (rect.width / 2))) +
          "    y: " + Util.fmtDbl(trans.pix2IsoCoordY(rect.y + (rect.height / 2)))
      )
    }

    rect
  }

  val rectangle: Rectangle = locate()

}
