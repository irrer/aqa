/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util

import java.awt.geom.Point2D
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point
import java.awt.geom.Rectangle2D
import scala.collection.immutable

/*


  The diagrams below show how collimator labels should appear for different collimator
  angles.  The labeling is independent of gantry angle.

.....................................................................................
                                        .
                                        .
                0 / 360                 .                       90
                                        .
                                        .
                   Y2                   .                       X2
          --------------------          .              --------------------
          |                  |          .              |                  |
          |                  |          .              |                  |
          |                  |          .              |                  |
       X1 |                  | X2       .           Y2 |                  | Y1
          |                  |          .              |                  |
          |                  |          .              |                  |
          |                  |          .              |                  |
          --------------------          .              --------------------
                   Y1                   .                       X1
                                        .
                                        .
                                        .
.....................................................................................
                                        .
                                        .
                                        .
                  180                   .                    270
                                        .
                                        .
                   Y1                   .                     X1
          --------------------          .            --------------------
          |                  |          .            |                  |
          |                  |          .            |                  |
          |                  |          .            |                  |
       X2 |                  | X1       .         Y1 |                  | Y2
          |                  |          .            |                  |
          |                  |          .            |                  |
          |                  |          .            |                  |
          --------------------          .            --------------------
                   Y2                   .                     X2
                                        .
                                        .
                                        .
.....................................................................................

 */

/**
  * Measure the four edges in an image (TBLR : top, bottom, left, right) in pixels.
  */
object MeasureTBLREdges extends Logging {

  case class X1X2Y1Y2(X1: Double, X2: Double, Y1: Double, Y2: Double) {
    def minus(other: X1X2Y1Y2): X1X2Y1Y2 = X1X2Y1Y2(X1 - other.X1, X2 - other.X2, Y1 - other.Y1, X2 - other.X2)

    def toSeq: Seq[Double] = Seq(X1, X2, Y1, Y2)

    def pix2iso(translator: IsoImagePlaneTranslator): X1X2Y1Y2 = {
      X1X2Y1Y2(translator.pix2IsoCoordX(X1), translator.pix2IsoCoordX(X2), translator.pix2IsoCoordY(Y1), translator.pix2IsoCoordY(Y2))
    }

    override def toString: String = {
      val fmt = "%8.3f"
      "X1: " + X1.formatted(fmt) + "    X2: " + X2.formatted(fmt) + "    Y1: " + Y1.formatted(fmt) + "    Y2: " + Y2.formatted(fmt)
    }

    def toTBLR(collimatorAngle: Double): TBLR = {
      Util.angleRoundedTo90(collimatorAngle) match {
        case 0   => TBLR(-Y2, -Y1, X1, X2)
        case 90  => TBLR(-X2, -X1, -Y2, -Y1)
        case 180 => TBLR(Y1, Y2, -X2, -X1)
        case 270 => TBLR(X1, X2, Y1, Y2)
      }
    }
  }

  private def TBLRtoX1X2Y1Y2(collimatorAngle: Double, tblr: TBLR): X1X2Y1Y2 = {
    Util.angleRoundedTo90(collimatorAngle) match {
      case 0   => X1X2Y1Y2(tblr.left, tblr.right, tblr.bottom, tblr.top)
      case 90  => X1X2Y1Y2(tblr.bottom, tblr.top, tblr.right, tblr.left)
      case 180 => X1X2Y1Y2(tblr.right, tblr.left, tblr.top, tblr.bottom)
      case 270 => X1X2Y1Y2(tblr.top, tblr.bottom, tblr.left, tblr.right)
    }
  }

  /**
    * Given a collimator angle, return the respective names of the top, bottom, left, and right edges.
    */
  private def edgeNames(collimatorAngle: Double): Seq[String] = {
    Util.angleRoundedTo90(collimatorAngle) match {
      case 0   => Seq("Y2", "Y1", "X1", "X2")
      case 90  => Seq("X2", "X1", "Y2", "Y1")
      case 180 => Seq("Y1", "Y2", "X2", "X1")
      case 270 => Seq("X1", "X2", "Y1", "Y2")
    }
  }

  case class TBLR(top: Double, bottom: Double, left: Double, right: Double) {
    val center = new Point2D.Double((right + left) / 2, (top + bottom) / 2)
    val width: Double = (right - left).abs
    val height: Double = (bottom - top).abs

    def floodRelative(floodOffset: Point): TBLR = {
      def transX(x: Double) = x + floodOffset.getX
      def transY(y: Double) = y + floodOffset.getY
      TBLR(transY(top), transY(bottom), transX(left), transX(right))
    }

    def minus(other: TBLR): TBLR = TBLR(top - other.top, bottom - other.bottom, left - other.left, right - other.right)

    def addOffset(point: Point2D.Double): TBLR = TBLR(top + point.getY, bottom + point.getY, left + point.getX, right + point.getX)

    def toX1X2Y1Y2(collimatorAngle: Double): X1X2Y1Y2 = TBLRtoX1X2Y1Y2(collimatorAngle, this)
    //def toX1X2Y1Y2 = TBLRtoX1X2Y1Y2(270, this)

    def pix2iso(translator: IsoImagePlaneTranslator): TBLR = {
      TBLR(translator.pix2IsoCoordY(top), translator.pix2IsoCoordY(bottom), translator.pix2IsoCoordX(left), translator.pix2IsoCoordX(right))
    }

    def iso2Pix(translator: IsoImagePlaneTranslator): TBLR = {
      TBLR(translator.iso2PixCoordY(top), translator.iso2PixCoordY(bottom), translator.iso2PixCoordX(left), translator.iso2PixCoordX(right))
    }

    /**
      * Grow (positive value) or shrink (negative value) the rectangle this
      * describes by adding or subtracting the given amount to all four sides.
      */
    def resize(change: Double): TBLR = {
      TBLR(top - change, bottom + change, left - change, right + change)
    }

    /**
      * Convert to Rectangle
      */
    def toRectangle: Rectangle2D.Double = {
      new Rectangle2D.Double(left, top, right - left, bottom - top)
    }

    override def toString: String = {
      "top: " + Util.fmtDbl(top) +
        "    bottom: " + Util.fmtDbl(bottom) +
        "    left: " + Util.fmtDbl(left) +
        "    right: " + Util.fmtDbl(right)
    }
  }

  private def getCollimatorPositionsFromPlan(rtimage: AttributeList, rtplan: AttributeList): X1X2Y1Y2 = {

    val BeamSequence = Phase2Util.getBeamSequence(rtplan, rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head)
    val ControlPoint = DicomUtil.seqToAttr(BeamSequence, TagByName.ControlPointSequence).head
    val BeamLimitingDevicePositionSequence = DicomUtil.seqToAttr(ControlPoint, TagByName.BeamLimitingDevicePositionSequence)

    def getLeafPositionBoundaries(deviceTypeName: String): Seq[Double] = {
      val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(BeamSequence, TagByName.BeamLimitingDeviceSequence)
      def nameMatch(bld: AttributeList): Boolean = bld.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.equals(deviceTypeName)

      val LeafPositionBoundaries = BeamLimitingDeviceSequence.find(bld => nameMatch(bld)).get.get(TagByName.LeafPositionBoundaries).getDoubleValues
      LeafPositionBoundaries.toSeq
    }

    case class Collimator(deviceTypeName: String, xLeafPositionBoundaries: Seq[Double]) {
      def this(name: String) = this(name, getLeafPositionBoundaries(name))

      val LeafPositionBoundaries: Seq[Double] = {
        val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(BeamSequence, TagByName.BeamLimitingDeviceSequence)
        def nameMatch(bld: AttributeList): Boolean = bld.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.equals(deviceTypeName)

        val lpb = BeamLimitingDeviceSequence.find(bld => nameMatch(bld)).get.get(TagByName.LeafPositionBoundaries).getDoubleValues
        lpb.toSeq
      }

      val leafPairCount: Int = LeafPositionBoundaries.size - 1
      val LeafJawPositions: Seq[Double] = {
        val BeamLimitingDevicePosition = BeamLimitingDevicePositionSequence.find(bld => bld.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.equals(deviceTypeName)).get
        val ljp = BeamLimitingDevicePosition.get(TagByName.LeafJawPositions).getDoubleValues
        ljp.toSeq
      }

      private def boundsCheck(index: Int): Unit = {
        if ((index < 0) || (index >= leafPairCount)) throw new IllegalArgumentException("index " + index + " out of bounds")
      }

      def posnLeft(index: Int): Double = {
        boundsCheck(index)
        LeafJawPositions(index)
      }

      def posnRight(index: Int): Double = {
        boundsCheck(index)
        LeafJawPositions(index + leafPairCount)
      }

      def posnTop(index: Int): Double = {
        boundsCheck(index)
        LeafPositionBoundaries(index)
      }

      def posnBottom(index: Int): Double = {
        boundsCheck(index)
        LeafPositionBoundaries(index + 1)
      }

      def leafGap(index: Int): Double = (posnRight(index) - posnLeft(index)).abs

      val zeroLimit = 0.001

      val nonZeroLeafGapIndexList: immutable.IndexedSeq[Int] = (0 until leafPairCount).filter(lp => leafGap(lp) > zeroLimit)

      /** Sorted list of indexes of leaves that have the most frequently occurring leaf gap. */
      val sortedIndexListOfMostFrequentNonZeroLeafGap: immutable.IndexedSeq[Int] = nonZeroLeafGapIndexList.groupBy(i => (leafGap(i) * 10).round.toInt).toList.maxBy(_._2.size)._2.sorted
    }

    val mlcx1 = new Collimator("MLCX1")
    val mlcx2 = new Collimator("MLCX2")
    val indexOfMostFrequentNonZeroLeafGap = mlcx1.sortedIndexListOfMostFrequentNonZeroLeafGap.head

    val x1 = mlcx1.posnLeft(indexOfMostFrequentNonZeroLeafGap)
    val x2 = mlcx1.posnRight(indexOfMostFrequentNonZeroLeafGap)

    val y1 = Seq(mlcx1.posnTop(mlcx1.sortedIndexListOfMostFrequentNonZeroLeafGap.head), mlcx2.posnTop(mlcx2.sortedIndexListOfMostFrequentNonZeroLeafGap.head)).max
    val y2 = Seq(mlcx1.posnBottom(mlcx1.sortedIndexListOfMostFrequentNonZeroLeafGap.last), mlcx2.posnBottom(mlcx2.sortedIndexListOfMostFrequentNonZeroLeafGap.last)).min

    val x1x2y1y2 = X1X2Y1Y2(x1, x2, y1, y2)

    x1x2y1y2
  }

  /**
    * Get the collimator (jaw) positions from the image.  This assumes that the positions given in the image are valid.
    */
  private def getCollimatorPositionsFromImage(rtimage: AttributeList): X1X2Y1Y2 = {
    val ExposureSequence = DicomUtil.seqToAttr(rtimage, TagByName.ExposureSequence).head
    val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(ExposureSequence, TagByName.BeamLimitingDeviceSequence)

    def getPair(nameList: Seq[String]): Array[Double] = {
      def filterByName(s: AttributeList) = nameList.contains(s.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString)
      val list = BeamLimitingDeviceSequence.filter(s => filterByName(s)).head.get(TagByName.LeafJawPositions).getDoubleValues
      list
    }

    val xPair = getPair(Util.xOrientation)
    val yPair = getPair(Util.yOrientation)

    X1X2Y1Y2(xPair(0), xPair(1), yPair(0), yPair(1))

  }

  //  def planCollimatorPositions(beamName: String, plan: AttributeList): X1X2Y1Y2 = {
  //    val tmt = DicomUtil.TreatmentMachineType.attrListToTreatmentMachineType(plan)
  //    val beamSeq = DicomUtil.seqToAttr(plan, TagFromName.BeamSequence).find(bs => Util.normalizedBeamName(bs).equals(beamName)).get
  //    val controlPtSeq = DicomUtil.seqToAttr(beamSeq, TagFromName.ControlPointSequence).head
  //    val BeamLimitingDeviceSequence = DicomUtil.seqToAttr(controlPtSeq, TagFromName.BeamLimitingDevicePositionSequence)
  //    getCollimatorPositions(BeamLimitingDeviceSequence, tmt)
  //  }

  def imageCollimatorPositions(rtimage: AttributeList, rtplan: AttributeList): MeasureTBLREdges.X1X2Y1Y2 = {
    val colPosn =
      if (DicomUtil.isHalcyon(rtplan))
        getCollimatorPositionsFromPlan(rtimage, rtplan)
      else
        getCollimatorPositionsFromImage(rtimage)

    colPosn
  }

  case class AnalysisResult(measurementSet: TBLR, bufferedImage: BufferedImage)

  private def getCoarseTopRectangle(image: DicomImage, centerOfMass: Point2D, translator: IsoImagePlaneTranslator): Rectangle = {
    val width = Math.ceil(translator.iso2PixDistX(Config.CollimatorCenteringCoarseBandWidth_mm)).toInt
    val x = (centerOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, 0, width, image.height / 2)
  }

  private def getCoarseBottomRectangle(image: DicomImage, centerOfMass: Point2D, translator: IsoImagePlaneTranslator): Rectangle = {
    val width = Math.ceil(translator.iso2PixDistX(Config.CollimatorCenteringCoarseBandWidth_mm)).toInt
    val x = (centerOfMass.getX - (width / 2.0)).round.toInt
    new Rectangle(x, image.height / 2, width, image.height / 2)
  }

  //

  private def getCoarseLeftRectangle(image: DicomImage, centerOfMass: Point2D, translator: IsoImagePlaneTranslator): Rectangle = {
    val height = Math.ceil(translator.iso2PixDistY(Config.CollimatorCenteringCoarseBandWidth_mm)).toInt
    val y = (centerOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(0, y, image.width / 2, height)
  }

  private def getCoarseRightRectangle(image: DicomImage, centerOfMass: Point2D, translator: IsoImagePlaneTranslator): Rectangle = {
    val height = Math.ceil(translator.iso2PixDistY(Config.CollimatorCenteringCoarseBandWidth_mm)).toInt
    val y = (centerOfMass.getY - (height / 2.0)).round.toInt
    new Rectangle(image.width / 2, y, image.width / 2, height)
  }

  //

  /**
    * Calculate the point at the given percent between the highest and lowest pixel value.
    *
    * @param image: Contains range of pixel values
    *
    * @param thresholdPercent: Must be from 0 to 1 non-inclusive, indicates where the actual edge should be considered to be.
    */
  private def calcPercentPixelValue(image: DicomImage, thresholdPercent: Double): Double = {
    if (!((thresholdPercent > 0) && (thresholdPercent < 1))) throw new IllegalArgumentException("thresholdPercent is " + thresholdPercent + " but must be 0 < t < 1.")
    val pixelCount = ((Config.PenumbraPlateauPixelsPerMillion / 1000000.0) * image.width * image.height).round.toInt
    if (pixelCount < 1) {
      val ex = new IllegalArgumentException("Image has zero pixels, impossible to calculate threshold")
      val msg = ex.toString + "\n" + fmtEx(ex)
      logger.error(msg)
      throw ex
    }
    val min = image.minPixelValues(pixelCount).sum / pixelCount
    val max = image.maxPixelValues(pixelCount).sum / pixelCount
    val pctThresh = ((max - min) * thresholdPercent) + min
    pctThresh
  }

  private def coarseMeasure(image: DicomImage, halfwayPixelValue: Double, translator: IsoImagePlaneTranslator): TBLR = {
    val centerOfMass = new Point2D.Double(ImageUtil.centerOfMass(image.columnSums), ImageUtil.centerOfMass(image.rowSums))

    val coarseTopRectangle = getCoarseTopRectangle(image, centerOfMass, translator)
    val coarseTopEdge = LocateEdge.locateEdge(image.getSubimage(coarseTopRectangle).rowSums, halfwayPixelValue * coarseTopRectangle.getWidth)

    val coarseBottomRectangle = getCoarseBottomRectangle(image, centerOfMass, translator)
    val coarseBottomEdge = LocateEdge.locateEdge(image.getSubimage(coarseBottomRectangle).rowSums, halfwayPixelValue * coarseBottomRectangle.getWidth) + coarseBottomRectangle.getY

    val coarseLeftRectangle = getCoarseLeftRectangle(image, centerOfMass, translator)
    val coarseLeftEdge = LocateEdge.locateEdge(image.getSubimage(coarseLeftRectangle).columnSums, halfwayPixelValue * coarseLeftRectangle.getHeight)

    val coarseRightRectangle = getCoarseRightRectangle(image, centerOfMass, translator)
    val coarseRightEdge = LocateEdge.locateEdge(image.getSubimage(coarseRightRectangle).columnSums, halfwayPixelValue * coarseRightRectangle.getHeight) + coarseRightRectangle.getX

    TBLR(coarseTopEdge, coarseBottomEdge, coarseLeftEdge, coarseRightEdge)
  }

  private val annotationColor = Color.black

  private val dashedLine = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array(1, 4), 0)
  private val solidLine = new BasicStroke

  private def annotateTopBottom(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, name: String, scaledEdge: Double, rect: Rectangle, floodOffset: Point): Unit = {

    val xOff = floodOffset.getX.round.toInt
    val yOff = floodOffset.getY.round.toInt

    val xRight = rect.getX.round.toInt + xOff
    val xLeft = (rect.getX + rect.getWidth).round.toInt + xOff
    val xMid = (xRight + xLeft) / 2

    val yTop = rect.getY.round.toInt + yOff
    val yBottom = (rect.getY + rect.getHeight).round.toInt + yOff
    val yMid = pixelEdge.round.toInt + yOff

    graphics.setStroke(dashedLine)
    graphics.drawLine(xRight, yMid, xLeft, yMid) // horizontal line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xRight, yTop, xRight, yBottom) // vertical line at left side of line
    graphics.drawLine(xLeft, yTop, xLeft, yBottom) // vertical line at right side of line

    val text = name + scaledEdge.formatted(" %7.2f")

    val textOffset = 16
    val y = pixelEdge.round.toInt + yOff
    val yText = if (y > bufImg.getHeight / 2) y + textOffset else y - textOffset
    ImageText.drawTextCenteredAt(graphics, xMid, yText, text)
  }

  private def annotateRightLeft(bufImg: BufferedImage, graphics: Graphics2D, pixelEdge: Double, name: String, scaledEdge: Double, rect: Rectangle, floodOffset: Point): Unit = {

    val xOff = floodOffset.getX.round.toInt
    val yOff = floodOffset.getY.round.toInt

    val xRight = rect.getX.round.toInt + xOff
    val xLeft = (rect.getX + rect.getWidth).round.toInt + xOff
    val xMid = pixelEdge.round.toInt + xOff

    val yTop = rect.getY.round.toInt + yOff
    val yBottom = (rect.getY + rect.getHeight).round.toInt + yOff
    val yMid = (yTop + yBottom) / 2

    graphics.setStroke(dashedLine)
    graphics.drawLine(xMid, yTop, xMid, yBottom) // vertical line through edge that was found
    graphics.setStroke(solidLine)
    graphics.drawLine(xRight, yTop, xLeft, yTop) // horizontal line at left side of line
    graphics.drawLine(xRight, yBottom, xLeft, yBottom) // horizontal line at right side of line

    val text = name + scaledEdge.formatted(" %7.2f")

    val textOffset = 30
    val x = pixelEdge.round.toInt + xOff
    val xText = if (x > bufImg.getWidth / 2) x + textOffset else x - textOffset
    ImageText.drawTextCenteredAt(graphics, xText, yMid, text)
  }

  private def annotateCenter(bufImg: BufferedImage, graphics: Graphics2D, transMeasurementSet: TBLR): Unit = {
    // center of image (not center of edges)
    val pixelImageCenter = new Point2D.Double(bufImg.getWidth / 2.0, bufImg.getHeight / 2.0)

    def fmt(d: Double) = d.formatted("%7.2f").trim

    graphics.setStroke(solidLine)
    graphics.setColor(annotationColor)
    val pixCenterX = pixelImageCenter.getX.round.toInt
    val pixCenterY = pixelImageCenter.getY.round.toInt

    val len = 5
    val gap = 2
    graphics.drawLine(pixCenterX, pixCenterY - (len + gap), pixCenterX, pixCenterY - gap)
    graphics.drawLine(pixCenterX, pixCenterY + len + gap, pixCenterX, pixCenterY + gap)
    graphics.drawLine(pixCenterX - (len + gap), pixCenterY, pixCenterX - gap, pixCenterY)
    graphics.drawLine(pixCenterX + gap, pixCenterY, pixCenterX + len + gap, pixCenterY)

    val ascent = graphics.getFontMetrics.getAscent

    def drawText(text: String, textRow: Int): Unit = {
      val textRect = graphics.getFontMetrics.getStringBounds(text, graphics)
      val x = pixelImageCenter.getX - textRect.getCenterX
      val y = pixelImageCenter.getY + len + gap + ascent + (textRect.getHeight * textRow)
      graphics.drawString(text, x.toFloat, y.toFloat)
    }

    drawText("Collimator Center: " + fmt(transMeasurementSet.center.getX) + ", " + fmt(transMeasurementSet.center.getY), 0)
  }

  /**
    * Make an annotated image that illustrates the edges.
    */
  private def makeAnnotatedImage(
      image: DicomImage,
      measurementSet: TBLR,
      transMeasurementSet: TBLR,
      collimatorAngle: Double,
      topRect: Rectangle,
      bottomRect: Rectangle,
      rightRect: Rectangle,
      leftRect: Rectangle,
      floodOffset: Point
  ): BufferedImage = {
    //val bufImg = image.toBufferedImage(imageColor)
    val bufImg = image.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
    Config.applyWatermark(bufImg)
    val graphics = ImageUtil.getGraphics(bufImg)
    graphics.setColor(annotationColor)

    val names = edgeNames(collimatorAngle)
    annotateTopBottom(bufImg, graphics, measurementSet.top, names.head, transMeasurementSet.top, topRect, floodOffset)
    annotateTopBottom(bufImg, graphics, measurementSet.bottom, names(1), transMeasurementSet.bottom, bottomRect, floodOffset)
    annotateRightLeft(bufImg, graphics, measurementSet.left, names(2), transMeasurementSet.left, leftRect, floodOffset)
    annotateRightLeft(bufImg, graphics, measurementSet.right, names(3), transMeasurementSet.right, rightRect, floodOffset)

    annotateCenter(bufImg, graphics, transMeasurementSet)
    bufImg
  }

  private def topBottomRectangles(coarse: TBLR, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val nsHeight = penumbraY // top and bottom height
    val nsWidth = (coarse.right - coarse.left) - penumbraX // top and bottom width
    val nsX = coarse.left + (penumbraX / 2)

    val topRectangle = new Rectangle(nsX.round.toInt, (coarse.top - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    val bottomRectangle = new Rectangle(nsX.round.toInt, (coarse.bottom - (penumbraY / 2)).round.toInt, nsWidth.round.toInt, nsHeight.round.toInt)
    (topRectangle, bottomRectangle)
  }

  private def leftRightRectangles(coarse: TBLR, penumbraX: Double, penumbraY: Double): (Rectangle, Rectangle) = {
    val ewHeight = (coarse.bottom - coarse.top) - penumbraY // right and left height
    val ewWidth = penumbraX // right and left height
    val ewY = coarse.top + (penumbraX / 2)

    val leftRectangle = new Rectangle((coarse.left - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    val rightRectangle = new Rectangle((coarse.right - (penumbraX / 2)).round.toInt, ewY.round.toInt, ewWidth.round.toInt, ewHeight.round.toInt)
    (leftRectangle, rightRectangle)
  }

  /**
    * Measure the four edges in the image, and create an annotated image.
    *
    * @param image: Image to analyze.  Should have already been corrected for flood field if necessary.
    *
    * @param translator: IsoImagePlaneTranslator.
    *
    * @param expected_mm: Expected edge position in mm in isoplane.
    *
    * @param annotate: Image containing pixels to be annotated.
    *
    * @param floodOffset: XY offset of image to annotate.
    */
  def measure(
      image: DicomImage,
      translator: IsoImagePlaneTranslator,
      expected_mm: Option[TBLR],
      collimatorAngle: Double,
      annotate: DicomImage,
      floodOffset: Point,
      thresholdPercent: Double
  ): AnalysisResult = {
    val threshold = calcPercentPixelValue(image, thresholdPercent)

    val coarse = {
      if (expected_mm.isEmpty) coarseMeasure(image, threshold, translator)
      else {

        TBLR(
          translator.iso2PixCoordY(expected_mm.get.top),
          translator.iso2PixCoordY(expected_mm.get.bottom),
          translator.iso2PixCoordX(expected_mm.get.left),
          translator.iso2PixCoordX(expected_mm.get.right)
        )
      }
    }

    val penumbraX = translator.iso2PixDistX(Config.PenumbraThickness_mm) // penumbra thickness in pixels X
    val penumbraY = translator.iso2PixDistY(Config.PenumbraThickness_mm) // penumbra thickness in pixels Y

    val nsRect = topBottomRectangles(coarse, penumbraX, penumbraY)
    val ewRect = leftRightRectangles(coarse, penumbraX, penumbraY)

    val topEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._1).rowSums, threshold * nsRect._1.width) + nsRect._1.y
    val bottomEdge = LocateEdge.locateEdge(image.getSubimage(nsRect._2).rowSums, threshold * nsRect._2.width) + nsRect._2.y
    val leftEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._1).columnSums, threshold * ewRect._1.height) + ewRect._1.x
    val rightEdge = LocateEdge.locateEdge(image.getSubimage(ewRect._2).columnSums, threshold * ewRect._2.height) + ewRect._2.x

    val measurementSet = TBLR(topEdge, bottomEdge, leftEdge, rightEdge)
    val transMeasurementSet = measurementSet.floodRelative(floodOffset).pix2iso(translator)

    val bufferedImage = makeAnnotatedImage(annotate, measurementSet, transMeasurementSet, collimatorAngle, nsRect._1, nsRect._2, ewRect._1, ewRect._2, floodOffset)
    AnalysisResult(measurementSet, bufferedImage)
  }

  //  def measure(image: DicomImage, translator: IsoImagePlaneTranslator, expected_mm: Option[TBLR], collimatorAngle: Double, annotate: DicomImage, floodOffset: Point): AnalysisResult = {
  //    measure(image, translator, expected_mm, collimatorAngle, annotate, floodOffset, 0.5)
  //  }

}
