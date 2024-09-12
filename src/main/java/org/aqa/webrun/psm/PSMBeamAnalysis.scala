package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomBeam
import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.Config

import java.awt.Color
import java.awt.Point
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.Rectangle
import java.io.File
import javax.vecmath.Point2i


/**
 *
 * @param rtplan    DICOM RTPLAN for delivering beams.
 * @param pointList List of coordinates serving as a template for the circle in the center of the beam.
 * @param trans     For translating between pixel and isoplane coordinates.
 * @param rtimage   Beam image.
 */
case class PSMBeamAnalysis(rtplan: AttributeList, pointList: Seq[Point2i], trans: IsoImagePlaneTranslator, rtimage: AttributeList) extends Logging {

  def drawCircle(bufImage: BufferedImage, center_iso: Point2D.Double): Unit = {
    val gc = ImageUtil.getGraphics(bufImage)

    gc.setColor(Color.black)
    val x_pix = trans.iso2PixCoordX(center_iso.getX - Config.PSMRadius_mm).toInt
    val y_pix = trans.iso2PixCoordY(center_iso.getY - Config.PSMRadius_mm).toInt
    val width_pix = trans.iso2PixDistX(Config.PSMRadius_mm * 2).toInt
    val height_pix = trans.iso2PixDistY(Config.PSMRadius_mm * 2).toInt
    gc.drawOval(x_pix, y_pix, width_pix, height_pix)
  }

  private def drawText(bufImage: BufferedImage, beamName: String, center_iso: Point2D.Double): Unit = {

    val gc = ImageUtil.getGraphics(bufImage)
    gc.setColor(Color.black)

    val text = s"$beamName    Center: ${Util.fmtDbl(center_iso.getX)}, ${Util.fmtDbl(center_iso.getY)}  "
    logger.info(s"Beam Result: $text")
    ImageText.setFont(gc, "SansSerif", 30)
    ImageText.drawTextCenteredAt(gc, bufImage.getWidth / 2, 40, text)
  }

  private def annotate(bufImage: BufferedImage, beamName: String, center_iso: Point2D.Double): Unit = {
    drawCircle(bufImage, center_iso)
    drawText(bufImage, beamName, center_iso)
  }


  /**
   * Find the center of mass of the beam.
   *
   * @param dicomImage Contains pixel array.
   * @param planBeam   Specification of how to deliver the beam.  Extracted from the RTPLAN.
   * @param beamName   Name of beam.
   * @return Coordinates of center of mass of the beam in pixel coordinates.
   */
  private def findCenter_pix(dicomImage: DicomImage, planBeam: DicomBeam, beamName: String): Point2D.Double = {

    /* Build a message to show the results of intermediate calculations in the log. */
    val msg: StringBuffer = new StringBuffer("%-10s".format(beamName))

    def fmtPoint(point: Point2D.Double): String = {
      def df(d: Double) = "%14.8f".format(d)

      s"${df(point.getX)} , ${df(point.getY)} "
    }

    /**
     * Use the whole image to find the coarse (approximate) center of the field.
     *
     * A horizontal and vertical profile are made.  For each, the mean value is
     * calculated and then all values that are below that are set to 0.  By doing
     * this, the background is ignored, which makes it very robust.  Because it
     * also ignores some of the penumbra, it is also less accurate, but serves well
     * as a first approximation.
     */
    def findCoarseCenter(): Point2D.Double = {

      def findCenterOfProfile(profile: Seq[Float]): Float = {
        val mid = (profile.max + profile.min) / 2
        val zero = 0.toFloat
        // set values that are below the mean to zero
        val prof = profile.map(v => if (v < mid) zero else v)
        val center = ImageUtil.centerOfMass(prof.toIndexedSeq)
        center
      }

      val xCenterCoarse_pix = findCenterOfProfile(dicomImage.columnSums)
      val yCenterCoarse_pix = findCenterOfProfile(dicomImage.rowSums)

      val coarseCenter_pix = new Point2D.Double(xCenterCoarse_pix.toDouble, yCenterCoarse_pix.toDouble)
      msg.append(s"    coarse: ${fmtPoint(coarseCenter_pix)}    ")
      coarseCenter_pix
    }

    /**
     * Find the center of mass of the beam by using an AOI (area of interest) shaped as two narrow
     * rectangles  spanning the beam, one vertical and the other horizontal.
     *
     * The rectangle must be narrow because some beams may have an artifact in the corner (when the
     * beam is in the corner), and the only way (I know of) to handle this is to stay away from it.
     *
     * @param center_pix    Use this as the center of the AOI in pixel coordinates.
     * @param thickness_iso Specifies how wide the rectangle should be in mm.
     * @return
     */
    def findFineCenter(center_pix: Point2D.Double, thickness_iso: Double): Point2D.Double = {

      val xC = center_pix.getX
      val yC = center_pix.getY


      // translate thickness to pixels and guarantee that it is at least one pixel wide or tall.
      val vertThickness_pix = Math.max(trans.iso2PixDistY(thickness_iso).toInt, 1)
      val horzThickness_pix = Math.max(trans.iso2PixDistX(thickness_iso).toInt, 1)

      // Width and height of area to measure.  This will span the entire beam plus the penumbra.
      val xFieldSize_iso = (planBeam.x2Jaw.get - planBeam.x1Jaw.get) + Config.PenumbraThickness_mm
      val yFieldSize_iso = (planBeam.y2Jaw.get - planBeam.y1Jaw.get) + Config.PenumbraThickness_mm

      // Convert to pix coordinates.
      val xFieldSize_pix = trans.iso2PixDistX(xFieldSize_iso)
      val yFieldSize_pix = trans.iso2PixDistY(yFieldSize_iso)

      // --------------------------------------------------------------------------------

      // Find the X coordinate using a wide, short, AOI.
      val xMaxRadius_pix = Math.min(xC, dicomImage.width - xC)
      val xRadius_pix = Math.min(xFieldSize_pix / 2, xMaxRadius_pix)
      if (xRadius_pix != (xFieldSize_pix / 2))
        Trace.trace(s"$beamName shortened X AOI: $xRadius_pix    instead of   ${xFieldSize_pix / 2}")
      val xFieldPos_pix = (xC - xRadius_pix).toInt
      val horzRectangle = new Rectangle(xFieldPos_pix, (yC - (vertThickness_pix / 2.0)).toInt, (xRadius_pix * 2).toInt, vertThickness_pix)
      val xSubImageFine = dicomImage.getSubimage(horzRectangle)
      val xCenterOfMassFine_pix = ImageUtil.centerOfMass(xSubImageFine.columnSums) + xFieldPos_pix

      // --------------------------------------------------------------------------------

      // Find the Y coordinate using a tall, skinny, AOI.
      val yMaxRadius_pix = Math.min(yC, dicomImage.height - yC)
      val yRadius_pix = Math.min(yFieldSize_pix / 2, yMaxRadius_pix)
      if (yRadius_pix != (yFieldSize_pix / 2))
        Trace.trace(s"$beamName shortened Y AOI: $yRadius_pix    instead of   ${yFieldSize_pix / 2}")
      val yFieldPos_pix = (yC - yRadius_pix).toInt
      val vertRectangle = new Rectangle((xC - (horzThickness_pix / 2.0)).toInt, yFieldPos_pix, horzThickness_pix, (yRadius_pix * 2).toInt)
      val ySubImageFine = dicomImage.getSubimage(vertRectangle)
      val yCenterOfMassFine_pix = ImageUtil.centerOfMass(ySubImageFine.rowSums) + yFieldPos_pix

      // --------------------------------------------------------------------------------

      val centerFine_pix = new Point2D.Double(xCenterOfMassFine_pix, yCenterOfMassFine_pix)

      // Shows how different the initial locatis from the new one.
      val dist = centerFine_pix.distance(center_pix)
      // Build a message for the log.
      msg.append(s"      dist: ${"%f11.8".format(dist)}   ${fmtPoint(centerFine_pix)} ")

      centerFine_pix
    }


    // make a band of pixel this thick in mm.
    val thickness_iso = 8.0

    val coarseCenter_pix = findCoarseCenter() // start with the more robust but less accurate method.
    val center1_pix = findFineCenter(coarseCenter_pix, thickness_iso / 4) // use a very narrow AOI to avoid possible artifacts.
    val center2_pix = findFineCenter(center1_pix, thickness_iso / 2) // use a less narrow AOI because we're probably farther from an artifact.
    val center3_pix = findFineCenter(center2_pix, thickness_iso) // use a less narrow AOI because we're probably farther from an artifact.

    logger.info(s"Beam centering progression: $msg")

    center3_pix
  }


  def measure(dir: File): Point2D.Double = {

    // grab part of RTPLAN that specifies this beam
    val rtplanBeam = DicomBeam(rtplan, rtimage)

    // name of beam
    val beamName = Util.normalizeBeamName(rtplanBeam.beamAl.get(TagByName.BeamName).getSingleStringValueOrEmptyString)

    // Make sure that the collimator angle is 0.  If not, then the X1,X2,Y1,Y2 jaws will not be in the expected orientation
    val collimatorAngle = Util.angleRoundedTo90(rtplanBeam.rtimageCollimatorAngle)
    if (collimatorAngle != 0) {
      throw new RuntimeException("Collimator angle is required to be zero.")
    }

    val dicomImage = new DicomImage(rtimage)

    val center_pix = findCenter_pix(dicomImage, rtplanBeam, beamName)

    val pngFile = new File(dir, beamName + ".png")

    val center_iso = trans.pix2Iso(center_pix)


    if (false) {
      // @formatter:off
      val topPlanned_iso    = center_iso.getY - rtplanBeam.y2Jaw.get
      val bottomPlanned_iso = center_iso.getY - rtplanBeam.y1Jaw.get
      val leftPlanned_iso   = center_iso.getX + rtplanBeam.x1Jaw.get
      val rightPlanned_iso  = center_iso.getX + rtplanBeam.x2Jaw.get
      // @formatter:on

      val tblr = MeasureTBLREdges.TBLR(topPlanned_iso, bottomPlanned_iso, leftPlanned_iso, rightPlanned_iso)

      val bi: Option[BufferedImage] = try {
        val ar = MeasureTBLREdges.measure(
          dicomImage,
          translator = trans,
          expected_mm = Some(tblr),
          collimatorAngle = 0,
          annotate = dicomImage,
          floodOffset = new Point,
          thresholdPercent = 0.5,
          markCenter = false
        )
        Trace.trace(s" $beamName    ar: ${ar.measurementSet}")
        Some(ar.bufferedImage)
      }
      catch {
        case t: Throwable =>
          Trace.trace(s"Badness $beamName: ${fmtEx(t)}")
          None
      }

      val bufImage = {
        if (bi.isEmpty)
          dicomImage.toDeepColorBufferedImage(0.06)
        else
          bi.get
      }

      annotate(bufImage, beamName, center_iso)

      Util.writePng(bufImage, pngFile)
      logger.info(s"Wrote file $pngFile")
    }

    center_iso
  }
}
