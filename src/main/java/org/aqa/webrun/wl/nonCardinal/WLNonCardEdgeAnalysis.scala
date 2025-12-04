/*
 * Copyright 2025 Regents of the University of Michigan
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

package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.ScaledImage
import edu.umro.ScalaUtil.Trace
import org.aqa.AQALine
import org.aqa.BiCubicImage
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.webrun.wl.WLCoarseBox
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLPreprocessImage

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.Rectangle
import java.io.File
import javax.vecmath.Point2d

case class WLNonCardEdgeAnalysis( //
    preprocessedImage: DicomImage,
    al: AttributeList,
    wlMessage: Option[WLMessage] = None
) extends Logging {

  private val collAngle = Util.collimatorAngle(al)

  private val trans = new IsoImagePlaneTranslator(al)

  private val biCubicImage = BiCubicImage(preprocessedImage)

  /** The center of the edges as calculated by finding the center of mass.  This should be accurate to within 3 pixels. */
  private def locateCoarseCenter(): Point2d = {
    val rect = WLCoarseBox(preprocessedImage, trans, wlMsg = None).locate()
    new Point2d(rect.getCenterX, rect.getCenterY)
    new Point2d(rect.getCenterX + 5, rect.getCenterY - 8)
  }

  /**
    * Find the approximate positions of the 4 edges by projecting a band of points in each of the 4 directions
    * from the coarse center.  These are parallel and perpendicular to the coarse center.
    *
    * @return List of approximated edges.
    */
  private def approximateLocationOfEdges(coarseCenter: Point2d): WLNonCardEdgeSet = {
    // number of pixels in one mm
    val pixPerMm = trans.iso2PixDistX(1)

    // use this granularity of pixels to get initial location of edges.
    val approximateResolution = 0.5

    // Width of band to look for edges.
    val pixBandWidth = 4 * pixPerMm

    val maxLength = preprocessedImage.width + preprocessedImage.height

    val xLine = AQALine(coarseCenter, collAngle)
    val yLine = xLine.perpendicular

    val x1 = WLNonCardEdge("Approximate X1", xLine, 0, maxLength, biCubicImage, pixBandWidth, approximateResolution)
    val x2 = WLNonCardEdge("Approximate X2", xLine, 0, -maxLength, biCubicImage, pixBandWidth, approximateResolution)
    val y1 = WLNonCardEdge("Approximate Y1", yLine, 0, -maxLength, biCubicImage, pixBandWidth, approximateResolution)
    val y2 = WLNonCardEdge("Approximate Y2", yLine, 0, maxLength, biCubicImage, pixBandWidth, approximateResolution)

    val edgeSet: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    edgeSet
  }

  case class AnnotatedEdgeAoi(line: AQALine, offsetStart: Double, offsetFinish: Double, width: Double, position: Double) {}

  /**
    * Precisely locate the four edges using a larger number of pixels and a higher sampling resolution.
    *
    * @param approximateEdgeLocations Approximate locations of edges.
    * @return
    */
  private def preciseLocationOfEdges(approximateEdgeLocations: WLNonCardEdgeSet): WLNonCardEdgeSet = {
    val xLine = AQALine(approximateEdgeLocations.center, collAngle)
    val yLine = xLine.perpendicular

    // use this granularity of pixels to get initial location of edges.
    val preciseResolution = 0.1

    val penumbra_pix = trans.iso2PixDistX(Config.PenumbraThickness_mm) / 2

    val ael = approximateEdgeLocations

    val distanceX = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) + penumbra_pix
    val distanceY = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) + penumbra_pix

    val xWidth = ael.Y1.edgeCenter.distance(ael.Y2.edgeCenter) - penumbra_pix
    val yWidth = ael.X1.edgeCenter.distance(ael.X2.edgeCenter) - penumbra_pix

    val x1 = WLNonCardEdge("X1", xLine, 0, distanceX, biCubicImage, xWidth, preciseResolution)
    val x2 = WLNonCardEdge("X2", xLine, 0, -distanceX, biCubicImage, xWidth, preciseResolution)
    val y1 = WLNonCardEdge("Y1", yLine, 0, -distanceY, biCubicImage, yWidth, preciseResolution)
    val y2 = WLNonCardEdge("Y2", yLine, 0, distanceY, biCubicImage, yWidth, preciseResolution)

    val edgeSet: WLNonCardEdgeSet = WLNonCardEdgeSet(x1, x2, y1, y2)

    edgeSet
  }

  /**
    * Calculate the rectangle to enclose the region of the image that contains all the areas of interest
    * that were used for edge measurement.
    *
    * @param border_pix Number of extra pixels to serve as a border separating the AOIs from the image edge.
    * @return Bounding rectangle.
    */
  private def calcAoiBounds(border_pix: Int): Rectangle = {
    def listCoordinates(edge: WLNonCardEdge): Seq[Point2d] = {
      Seq(
        edge.loLoAoi, //
        edge.loHiAoi, //
        edge.hiLoAoi, //
        edge.hiHiAoi
      )
    }

    val coordinateList = edgeSet.edgeList.flatMap(listCoordinates)

    val minX = (coordinateList.map(_.getX).min - border_pix).round.toInt
    val maxX = (coordinateList.map(_.getX).max + border_pix).round.toInt
    val minY = (coordinateList.map(_.getY).min - border_pix).round.toInt
    val maxY = (coordinateList.map(_.getY).max + border_pix).round.toInt

    val width = maxX - minX
    val height = maxY - minY

    val boundingRectangle = new Rectangle(minX, minY, width, height)

    boundingRectangle
  }

  // main processing comprised of three steps

  // Find the coarse center using center of mass.
  private val coarseCenter: Point2d = locateCoarseCenter()

  /** Approximate position of the 4 edges.  Testing shows that this is accurate to about 0.05 pixels.  But we can do better! */
  private val approximateEdgeLocationList: WLNonCardEdgeSet = approximateLocationOfEdges(coarseCenter)

  private val preciseEdgeLocations = preciseLocationOfEdges(approximateEdgeLocationList)

  val edgeSet: WLNonCardEdgeSet = preciseEdgeLocations
}

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object WLNonCardEdgeAnalysis {

  def main(args: Array[String]): Unit = {

    Trace.trace

    val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0006.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/WLNonCardNon45_20250625_Peyton/20250625_G180C30T0.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/0010.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""") // rotated 315
    // val file = new File("""D:/tmp/wl/nonorth/1/0006.dcm""") // rotated 45
    // val file = new File("""D:/tmp/wl/nonorth/1/0001.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/psm/0018.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/TB5_Aug_20/0002.dcm""")
    // val file = new File("""D:/tmp/wl/nonorth/BR1_Phase2/0014.dcm""")

    val al = new DicomFile(file).attributeList.get

    val colAngle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    val dicomImage = {
      // Invert the pixels if necessary.
      WLPreprocessImage(al, None).preprocessedImage
    }

    Trace.trace()
    val nonCardinal = new WLNonCardEdgeAnalysis(dicomImage, al)
    Trace.trace()
    Trace.trace(nonCardinal)

    Trace.trace()

    val bufImg = {
      val sortedPixels = dicomImage.pixelData.flatten.sorted
      val minPixelValue = sortedPixels(10)
      val maxPixelValue = sortedPixels.dropRight(10).last
      dicomImage.toBufferedImage(ImageUtil.rgbColorMap(Color.blue), minPixelValue, maxPixelValue)
    }

    val origImage = ImageUtil.magnify(bufImg, 1)

    val trans = new IsoImagePlaneTranslator(al)

    val gc = ImageUtil.getGraphics(bufImg)

    gc.setColor(Color.white)

    if (true) {
      val text = "Collimator Angle: " + Util.fmtDbl(colAngle)
      ImageText.drawTextCenteredAt(gc, dicomImage.width / 2, 40, text)
    }

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      // Trace.trace(Util.d2i(x1) + " : " + Util.d2i(y1) + " : " + Util.d2i(x2) + " : " + Util.d2i(y2))
      gc.drawLine(Util.d2i(x1), Util.d2i(y1), Util.d2i(x2), Util.d2i(y2))
    }

    // ------------------------------------------------------------------------------------

    if (true) {
      val start = System.currentTimeMillis()
      val imgScale = 3

      def listCoordinates(edge: WLNonCardEdge): Seq[Point2d] = {
        Seq(
          edge.loLoAoi, //
          edge.loHiAoi, //
          edge.hiLoAoi, //
          edge.hiHiAoi
        )
      }

      val coordinateList = nonCardinal.edgeSet.edgeList.flatMap(listCoordinates)

      val border_pix = 3
      val minX = (coordinateList.map(_.getX).min - border_pix).round.toInt
      val maxX = (coordinateList.map(_.getX).max + border_pix).round.toInt
      val minY = (coordinateList.map(_.getY).min - border_pix).round.toInt
      val maxY = (coordinateList.map(_.getY).max + border_pix).round.toInt

      val width = maxX - minX
      val height = maxY - minY

      val boundingRectangle = new Rectangle(minX, minY, width, height)

      val si = ScaledImage(imgScale, minX, minY)

      val aoi: BufferedImage = ImageUtil.magnify(ImageUtil.subImage(origImage, boundingRectangle), imgScale)

      // val buf = si.magnify(origImage)
      Trace.trace("making big image")
      def drawAoi(edgeSet: WLNonCardEdgeSet, aoi: BufferedImage): Unit = {

        val gc = ImageUtil.getGraphics(aoi)

        gc.setColor(Color.white)

        def drawEdge(edge: WLNonCardEdge): Unit = {
          gc.setColor(Color.white)

          si.drawTextCenteredAt(gc, (edge.loLoAoi.getX + edge.hiHiAoi.getX) / 2, (edge.loLoAoi.getY + edge.hiHiAoi.getY) / 2, edge.name)

          si.drawLine(gc, edge.loLoAoi, edge.loHiAoi)
          si.drawLine(gc, edge.hiLoAoi, edge.hiHiAoi)
          si.drawLine(gc, edge.loLoAoi, edge.hiLoAoi)
          si.drawLine(gc, edge.loHiAoi, edge.hiHiAoi)
          gc.setColor(Color.red)
          si.drawLine(gc, edge.edgeLo, edge.edgeHi)
        }

        drawEdge(edgeSet.X1)
        drawEdge(edgeSet.X2)
        drawEdge(edgeSet.Y1)
        drawEdge(edgeSet.Y2)
      }

      drawAoi(nonCardinal.edgeSet, aoi)

      val file = new File("""D:/tmp/foy.png""")

      ImageUtil.writePngFile(aoi, file)

      Trace.trace()
      val nonCardBall = WLNonCardBall(nonCardinal.edgeSet, dicomImage)
      Trace.trace()
      Trace.trace("Ball center: " + nonCardBall.ballCenterFirstTry)
      Trace.trace()

      val elapsed = System.currentTimeMillis() - start
      println(s"Elapsed ms: $elapsed    Wrote file $file")
    }

    Trace.trace("Center of four edges as pixels: " + nonCardinal.edgeSet.center)
    Trace.trace("Center of four edges as iso: " + trans.pix2IsoCoordX(nonCardinal.edgeSet.center.getX) + ", " + trans.pix2IsoCoordY(nonCardinal.edgeSet.center.getY))

    // ------------------------------------------------------------------------------------

    /**
      * Label the collimator edge
      * @param name Edge name.
      * @param point1 One end.
      * @param point2 The other end.
      */
    def labelEdge(name: String, point1: Point2D.Double, point2: Point2D.Double): Unit = {
      val centerX = (point1.getX + point2.getX) / 2
      val centerY = (point1.getY + point2.getY) / 2
      ImageText.drawTextCenteredAt(gc, centerX, centerY, name)
    }

    // ------------------------------------------------------------------------------------

    val rot = WLRotator(al)

    val X1Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYTop)))
    val X2Y1 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYTop)))

    val X1Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYBottom)))
    val X2Y2 = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYBottom)))

    Trace.trace(s"jawsYTop   : ${rot.jawsYTop.round}")
    Trace.trace(s"jawsYBottom: ${rot.jawsYBottom.round}")
    Trace.trace(s"jawsXLeft  : ${rot.jawsXLeft.round}")
    Trace.trace(s"jawsXRight : ${rot.jawsXRight.round}")

    Trace.trace(s"\n    topLeft: $X1Y2\n    topRight: $X2Y2\n    bottomLeft: $X1Y1\n    bottomRight: $X2Y1")

    drawLine(X1Y2.getX, X1Y2.getY, X2Y2.getX, X2Y2.getY)
    drawLine(X1Y2.getX, X1Y2.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y1.getX, X2Y1.getY, X1Y1.getX, X1Y1.getY)
    drawLine(X2Y2.getX, X2Y2.getY, X2Y1.getX, X2Y1.getY)

    labelEdge("X1", X1Y2, X1Y1)
    labelEdge("X2", X2Y2, X2Y1)
    labelEdge("Y1", X1Y1, X2Y1)
    labelEdge("Y2", X1Y2, X2Y2)

    val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
    Util.writePng(bufImg, pngFile)

    val txtFile = new File(file.getParent, file.getName.replace(".dcm", "_.txt"))
    Util.writeFile(txtFile, dicomImage.pixelsToText)

    Trace.trace(s"wrote $pngFile")

    System.exit(0)
  }
}
