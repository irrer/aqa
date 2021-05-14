package org.aqa.webrun.gapSkew

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ImageUtil.LocateMax
import edu.umro.ImageUtil.Profile
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.phase2.Phase2Util

import java.awt.Color
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.io.File
import javax.vecmath.Point2d

case class FindLeafEnds(attributeList: AttributeList, rtplan: AttributeList) extends Logging {

  /** True if collimator is horizontal. */
  private val isHorizontal: Boolean = Phase2Util.isHorizontal(attributeList)

  private val translator = new IsoImagePlaneTranslator(attributeList)

  private def d2i(d: Double): Int = d.round.toInt

  /**
    * Make an awt Rectangle from double precision values.  This is mostly a convenience function.
    *
    * @param x      X
    * @param y      Y
    * @param width  Width
    * @param height Height
    * @return a Rectangle
    */
  private def rectD(x: Double, y: Double, width: Double, height: Double): Rectangle = {
    new Rectangle(d2i(x), d2i(y), d2i(width), d2i(height))
  }

  private val dicomImage = new DicomImage(attributeList)

  /** Create annotated image.  Removing a large number of high and low pixels sets the window and level to
    * as best to obviate the leakage at the leaves' sides.
    */
  private val bufferedImage: BufferedImage = dicomImage.toDeepColorBufferedImage(percentToDrop = 20.0)

  Util.addGraticules(bufferedImage, translator, Color.yellow)

  // The sorted list of leaf sides in the RTPLAN in pixels.
  private val leafSidesFromPlanAsPix: Seq[Double] = {
    DicomUtil.findAllSingle(rtplan, TagByName.LeafPositionBoundaries).head.getDoubleValues.sorted
  }

  // private case class BeamLimit(limit: Double, isLeaf: Boolean) {}
  private case class EndPair(min: Double, max: Double) {}

  /**
    * Get the positions of ends of the leaves in mm in isoplane.
    */
  private val endPairIso: EndPair = {
    val beamSequence = Phase2Util.getBeamSequence(rtplan, Util.beamNumber(attributeList))
    val cps = DicomUtil.seqToAttr(beamSequence, TagByName.ControlPointSequence).head
    val beamLimitList = DicomUtil.seqToAttr(cps, TagByName.BeamLimitingDevicePositionSequence)
    val end = beamLimitList.maxBy(bl => bl.get(TagByName.LeafJawPositions).getDoubleValues.length)
    val endList = end.get(TagByName.LeafJawPositions).getDoubleValues
    EndPair(endList.min, endList.max)
  }

  logger.info("Leaf ends positioned at " + endPairIso.min + ", " + endPairIso.max)

  private val endPairPix: EndPair = {
    if (isHorizontal)
      throw new RuntimeException("EndPair: Not implemented for horizontally positioned collimator.")
    else {
      val both = Seq(translator.iso2PixCoordY(-endPairIso.min), translator.iso2PixCoordY(-endPairIso.max))
      EndPair(both.min, both.max)
    }
  }

  /**
    * Mark the leaf ends and label with their positions.
    *
    * @param left_pix     Leftmost position of marker line in pixels
    * @param right_pix    Rightmost position of marker line in pixels
    * @param position_pix Vertical position of leaf end in pixels.
    */
  private def annotateMeasurement(left_pix: Double, right_pix: Double, position_pix: Double): Unit = {
    val graphics = ImageUtil.getGraphics(bufferedImage)

    val pos = d2i(position_pix)
    if (isHorizontal) {
      throw new RuntimeException("annotateMeasurement: Not implemented for horizontally positioned collimator.")
    } else {
      // draw white+black+white lines
      val x1 = d2i(left_pix)
      val x2 = d2i(right_pix)

      graphics.setColor(Color.black)
      graphics.drawLine(x1, pos, x2, pos)
      graphics.setColor(Color.white)
      graphics.drawLine(x1, pos - 1, x2, pos - 1)
      graphics.drawLine(x1, pos + 1, x2, pos + 1)

      val text = (-translator.pix2IsoCoordY(position_pix)).formatted("%6.2f").trim
      val textDim = ImageText.getTextDimensions(graphics, text)

      val boundingBoxHeight = translator.iso2PixDistY(Config.GapSkewLeafSideFinding_mm)

      val border_px = 3

      // center coordinates for text
      val xCenter = (left_pix + right_pix) / 2.0
      val yCenter = position_pix - 5 - boundingBoxHeight - textDim.getCenterY - border_px * 2

      def makeBackground(): Unit = {
        val x = xCenter - textDim.getWidth / 2.0 - border_px
        val y = yCenter - textDim.getHeight / 2.0 - border_px
        val width = textDim.getWidth + border_px * 2
        val height = textDim.getHeight + border_px * 2
        graphics.setColor(Color.black)
        graphics.fillRect(d2i(x), d2i(y), d2i(width), d2i(height))
      }

      makeBackground()
      graphics.setColor(Color.white)
      ImageText.drawTextCenteredAt(graphics, xCenter, yCenter, text)

    }
  }

  /**
    * Make a profile that can be graphed to show the user what the edge looks like.  X values of
    * Point2d are the Y position in mm, Y values are the CU.
    *
    * @param endPosition_pix Vertical position in pixels where the edge was found.
    * @param x_pix Horizontal position of bounding box (inside leaf) in pixels used to find edge.
    * @param width Width of bounding box (inside leaf) in pixels used to find edge.
    * @return
    */
  private def makeProfile(endPosition_pix: Double, x_pix: Double, width: Float): Seq[Point2d] = {

    // number of points to put in the profile.  This was chosen as enough to show detail without
    // being overly resource intensive.
    val numberOfPoints = 600

    // maximum height.  Actual height may be smaller because of cropping.
    val maxHeight = translator.iso2PixDistY(30.0)
    val y_pix = Math.max(endPosition_pix - maxHeight / 2, 0.0)
    val bottom_pix = Math.min(maxHeight + y_pix, dicomImage.height - 1)
    val height_pix = bottom_pix - y_pix

    val rectangle_pix = rectD(x_pix, y_pix, width, height_pix)

    val profileValueSeq = dicomImage.getSubimage(rectangle_pix).rowSums.map(_ / width)

    val spline = Profile(profileValueSeq).cubicSpline

    val increment = height_pix / numberOfPoints

    def yToProfileY(i: Int): Double = -translator.pix2IsoCoordY((i * increment) + rectangle_pix.getY)

    def yToCu(i: Int): Double = spline.evaluate(i * increment)

    (0 until numberOfPoints).map(i => new Point2d(yToProfileY(i), yToCu(i)))

  }

  /**
    * Given a bounding box that contains the leaf end and is between the sides of the leaf, determine
    * where the end of the leaf is in pixels.
    *
    * @param box A bounding box that contains the leaf end and is between the sides of the leaf.
    *            TODO: This could be a Rectangle2D.Double so as to weight partial pixels on either side.
    * @return The position of the leaf in pixels.
    */
  private def endOfLeaf_iso(box: Rectangle): Leaf = {

    val subImage = dicomImage.getSubimage(box)
    if (isHorizontal) {
      throw new RuntimeException("endOfLeaf_pix: Not implemented for horizontally positioned collimator.")
    } else {
      val profile = subImage.columnSums
      val max = profile.max
      // the minimums are well defined, but the locateMax function looks for maximums, so flip the
      // profile vertically which effectively looks for minimums.
      val profileFlipped = profile.map(max - _)

      val half = profile.size / 2

      val leafA = LocateMax.locateMax(profileFlipped.take(half))
      val leafB = LocateMax.locateMax(profileFlipped.drop(half)) + half

      val penumbraHeight_pix = translator.iso2PixDistY(Config.GapSkewLeafEndPenumbra_mm)

      val x = box.x + leafA + Config.GapSkewLeafSidePad_mm
      val width = (leafB - leafA) - (Config.GapSkewLeafSidePad_mm * 2)
      val y = box.getCenterY - penumbraHeight_pix / 2
      val height = penumbraHeight_pix
      val endBoundingRectangle = rectD(x, y, width, height)

      // averages of pixel intensity across the profile of the edge
      val profileAverages = dicomImage.getSubimage(endBoundingRectangle).rowSums.map(_ / width.toFloat)

      val end = LocateEdge.locateEdge(profileAverages, (profileAverages.min + profileAverages.max) / 2)

      val endPosition_pix = y + end

      annotateMeasurement(x, x + width, endPosition_pix)

      val endPosition_mm = -translator.pix2IsoCoordY(endPosition_pix)
      val chartProfile = makeProfile(endPosition_pix, x, width.toFloat)

      Leaf(endPosition_mm, chartProfile)
    }
  }

  /** Minimum leaf position specified in the RTPLAN. */
  private val leafPositionRtplanTop_mm: Double = endPairIso.max

  /** Maximum leaf position specified in the RTPLAN. */
  private val leafPositionRtplanBottom_mm: Double = endPairIso.min

  val leafSet: LeafSet =
    if (isHorizontal)
      throw new RuntimeException("FindLeafSides : Not implemented for horizontally positioned collimator.")
    else {
      val leafWidth_mm = (leafSidesFromPlanAsPix.head - leafSidesFromPlanAsPix(1)).abs
      val leafWidth_pix = translator.iso2PixDistX(leafWidth_mm)
      val leafSideFinding_pix = translator.iso2PixDistY(Config.GapSkewLeafSideFinding_mm)

      val height = leafSideFinding_pix
      val width = leafWidth_pix * 2

      val xLeft = leafWidth_pix / 2
      val xRight = dicomImage.width - xLeft - 2 * leafWidth_pix
      val yTop = endPairPix.min - height / 2
      val yBottom = endPairPix.max - height / 2

      val topLeftRect = rectD(xLeft, yTop, width, height)
      val topRightRect = rectD(xRight, yTop, width, height)
      val bottomLeftRect = rectD(xLeft, yBottom, width, height)
      val bottomRightRect = rectD(xRight, yBottom, width, height)

      Trace.trace("Beam Name: " + Util.beamNumber(attributeList)) // TODO rm

      // makeProfileData  TODO

      val set = LeafSet(
        bufferedImage,
        attributeList,
        rtplan,
        leafPositionRtplanTop_mm,
        leafPositionRtplanBottom_mm,
        endOfLeaf_iso(topLeftRect),
        endOfLeaf_iso(topRightRect),
        endOfLeaf_iso(bottomLeftRect),
        endOfLeaf_iso(bottomRightRect)
      )

      logger.info("MLC leaf end positions: " + set)
      set
    }
}

object FindLeafEnds {
  def main(args: Array[String]): Unit = {
    val image = new DicomFile(new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_003_2020-03-23T19-12-25.000.dcm""")).attributeList.get
    val rtplan = new DicomFile(new File("""D:\tmp\aqa\GapSkew\dicom\GapSkewRtPlans\RP.1.2.246.352.71.5.824327626427.245627.20140602132138.dcm""")).attributeList.get

    new FindLeafEnds(image, rtplan)
  }
}
