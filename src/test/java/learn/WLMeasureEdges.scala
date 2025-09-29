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

package learn

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ImageUtil.LocateEdge
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import org.aqa.webrun.wl.WLCoarseBox
import org.aqa.webrun.wl.WLMessage
import org.aqa.BiCubicImage
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Point2D
import java.io.File

case class WLMeasureEdges(rtimage: AttributeList, wlMessage: Option[WLMessage]) extends Logging {}

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object WLMeasureEdges extends Logging {

  var biCub2: Option[BiCubicImage] = None

  case class PointSet(line: WLLine, offsetPos: Double, offsetNeg: Double, aoiSizePos: Double, aoiSizeNeg: Double) extends Logging {
    val middle: Double = (offsetPos + offsetNeg) / 2

    val size: Double = offsetPos.abs + offsetNeg.abs

    override def toString: String = s"line: $line   offsetPos: $offsetPos   offsetNeg: $offsetNeg   middle: $middle"
  }

  def getOffsets(line: WLLine, color: Color, biCubicImage: BiCubicImage, pixBandWidth: Double, resolution: Double, profileResolution: Option[Double] = None, offsetLo: Double, offsetHi: Double)
      : PointSet = {

    case class Measurement(edge_pix: Double, aoiSize_pix: Double) {}

    def minPoint(start: Double, finish: Double): Measurement = {
      val gradient = line.makeGradient(start, finish, biCubicImage, pixBandWidth, resolution)
      val min = gradient.min
      if (profileResolution.isDefined) try { // TODO
        val s = {
          val x = gradient.indexOf(min) * resolution
          if (finish < 0) -x else x
        }
        val profile = line.makeProfile(s, finish, color, biCubicImage, pixBandWidth, profileResolution.get)
        // Trace.trace("profile:\n" + profile.mkString("\n") + "\n\n")

      } catch {
        case _: Throwable =>
          Trace.trace("no profile for you")
      }
      val index = gradient.indexOf(min)
      val partialProfile = gradient.drop(index)
      val halfway = (min + gradient.max) / 2
      val edgeUnscaled = LocateEdge.locateEdge(partialProfile.map(_.toFloat).toIndexedSeq, halfway)
      val edge = (edgeUnscaled + index) * resolution
      val edgeSigned = if (start < finish) edge else -edge
      Measurement(edgeSigned, (finish.abs - (index * resolution)))
    }

    // val pointSet = PointSet(line, minPoint(offsetLo, offsetHi), minPoint(-offsetLo, -offsetHi))
    val pos = minPoint(offsetLo, offsetHi)
    val neg = minPoint(-offsetLo, -offsetHi)

    val pointSet = PointSet(line, pos.edge_pix, neg.edge_pix, pos.aoiSize_pix, neg.aoiSize_pix)
    pointSet
  }

  def measure(rtimage: AttributeList): Unit = {

    val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""")

    val colAngle = rtimage.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    val dicomImage = {
      // Invert the pixels if necessary.
      WLPreprocessImage(rtimage, None).preprocessedImage
    }

    // val bufImg = dicomImage.toDeepColorBufferedImage(0.01)
    val bufImg = dicomImage.toBufferedImage(Color.white)
    val bufImg2 = dicomImage.toDeepColorBufferedImage(0.01)

    val trans = new IsoImagePlaneTranslator(rtimage)

    val pixPerMm = (trans.iso2PixDistX(1) + trans.iso2PixDistY(1)) / 2

    val pixBandWidth = 4 * pixPerMm

    val coarseAoi = WLCoarseBox(dicomImage, trans, wlMsg = None).locate()

    if (false) {
      Trace.trace("Adding rectangle.")
      // val gc = ImageUtil.getGraphics(bufImg)
      val gc = bufImg.getGraphics.asInstanceOf[Graphics2D]

      gc.setColor(Color.black)

      if (true) {
        val text = "Collimator Angle: " + Util.fmtDbl(colAngle)
        ImageText.drawTextCenteredAt(gc, dicomImage.width / 2, 40, text)
        Trace.trace(text)
      }

      def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
        // Trace.trace(Util.d2i(x1) + " : " + Util.d2i(y1) + " : " + Util.d2i(x2) + " : " + Util.d2i(y2))
        gc.drawLine(Util.d2i(x1), Util.d2i(y1), Util.d2i(x2), Util.d2i(y2))
      }

      if (true) {
        gc.drawRect(coarseAoi.x, coarseAoi.y, coarseAoi.width, coarseAoi.height)
      }
    }

    biCub2 = Some(BiCubicImage(dicomImage, Some(bufImg2)))
    val biCubicImage = BiCubicImage(dicomImage, None)
    // val biCubicImage = BiCubicImage(dicomImage, Some(bufImg))

    /** Coarse center point found by using center of mass of the rectangle containing ball. */
    val centerPointA = new Point2D.Double(coarseAoi.getCenterX, coarseAoi.getCenterY)

    val resolutionInitial = 0.25 // TODO make configurable

    // ------------------------------------------------------------------------------------

    case class Edges(pointSet0: PointSet, pointSet90: PointSet) {

      private val point0 = pointSet0.line.pointOn(pointSet0.middle)
      val line90 = new WLLine(point0, pointSet0.line.perpendicular.angle)

      private val point90 = pointSet90.line.pointOn(pointSet90.middle)
      val line0 = new WLLine(point90, pointSet90.line.perpendicular.angle)

      val center: Point2D.Double = line90.intersection(line0)

      val size0: Double = pointSet0.size

      val size90: Double = pointSet90.size
    }

    /**
      * Make a set of edges centered at the two orthogonal point sets.
      * @param pointSet0 Parallel to collimator.
      * @param pointSet90 Perpendicular to collimator.
      * @return Edges with point centered between the two point sets.
      */
    def XmakeEdges(pointSet0: PointSet, pointSet90: PointSet): Edges = {
      ??? // Edges( pointSet0.size, pointSet90.size)
    }

    /**
      * Find a second approximation of the center point using two thin bands of pixels that cross the coarse
      * center point, one strip at the collimator angle, and the other perpendicular to the collimator angle.
      *
      * The point of using a thin band, is to ensure that there is no interference with the other two edges.
      * The point of using a band with multiple pixels is to increase the number of pixels being used to
      * limit the effects of each individual pixel.
      */

    val edgesB: Edges = {

      val lineB = new WLLine(centerPointA, colAngle)

      // specify a number of pixels that will eventually take go off the edge of the imager
      val offTheEdge = dicomImage.width + dicomImage.height

      // Make a band of pixels parallel to the collimator angle.  This profile of this band can be used to find the edges.
      val pointSetB0: PointSet = getOffsets(lineB, Color.white, biCubicImage, pixBandWidth, resolutionInitial, profileResolution = None, 0, offTheEdge)

      // Same as for pointSetB0, but perpendicular to the collimator angle.
      val pointSetB90 = getOffsets(lineB.perpendicular, Color.black, biCubicImage, pixBandWidth, resolutionInitial, profileResolution = None, 0, offTheEdge)

      val newEdges = Edges(pointSetB0, pointSetB90)
      Trace.trace("newEdges: " + newEdges)

      newEdges
    }

    // ------------------------------------------------------------------------------------

    // find a third (and final) approximation of the center point using four areas of interest around the
    // expected positions of the four edges.

    val tol = pixPerMm * Config.WLBoxEdgeTolerance_mm
    val tol2 = tol * 2
    val tol3 = tol * 3

    val resolutionFinal = 0.05

    val edgesC: Edges = {
      val pointSet0 = {
        val pixBandWidth = edgesB.size90 - tol2
        val line0 = new WLLine(edgesB.center, colAngle)
        getOffsets(line0, Color.white, biCubicImage, pixBandWidth, resolutionFinal, profileResolution = Some(0.25), offsetLo = 0, offsetHi = (edgesB.size0 + tol3) / 2)
      }

      val pointSet90 = {
        val pixBandWidth = edgesB.size0 - tol2
        val line90 = new WLLine(edgesB.center, Util.modulo360(colAngle + 90))
        getOffsets(line90, Color.black, biCubicImage, pixBandWidth, resolutionFinal, profileResolution = Some(0.25), offsetLo = 0, offsetHi = (edgesB.size90 + tol3) / 2)
      }

      val e = Edges(pointSet0, pointSet90)
      Trace.trace(s"edgesC: $e")
      e
    }

    /*
    val lineC = WLLine(centerPointB.getX, centerPointB.getY, colAngle)

    // this is the center point that was found by using a thin band of pixels along either line.
    val centerPointC = {
      val lineC0 = {
        val p = lineB.pointOn(getMidPoint(lineB, biCubicImage, pixBandWidth, resolution, offTheEdge).middle)
        WLLine(p.getX, p.getY, lineB.perpendicular.angle)
      }

      // def getMidPoint(line: WLLine, biCubicImage: BiCubicImage, pixBandWidth: Double, resolution: Double, offTheEdge: Double): PointSet = {

      val lineC90 = {
        val perpendicular = lineB.perpendicular
        val p = perpendicular.pointOn(getMidPoint(perpendicular, biCubicImage, pixBandWidth, resolution, offTheEdge).middle)
        WLLine(p.getX, p.getY, perpendicular.perpendicular.angle)
      }

      lineC0.intersection(lineC90)
     */

    // ------------------------------------------------------------------------------------

    /*
    if (true) { // Show info for debugging. TODO rm.
      Trace.trace(s" center coarse:  ${centerPointA.getX}  $centerPointA.getY")
      Trace.trace(s" center fine: $centerPointB")
      Trace.trace(s" dist: " + centerPointB.distance(centerPointA.getX, centerPointA.getY))

      val line0A = WLLine(centerPointB.getX, centerPointB.getY, lineB.angle)
     */

    /*
    val pair0 = getMidPoint(line0)

    bufImg.setRGB(pair0._1.getX.round.toInt, pair0._1.getY.round.toInt, 0)
    bufImg.setRGB(pair0._2.getX.round.toInt, pair0._2.getY.round.toInt, 0)

    val pair90 = getMidPoint(line0.perpendicular)

    bufImg.setRGB(pair90._1.getX.round.toInt, pair90._1.getY.round.toInt, 0)
    bufImg.setRGB(pair90._2.getX.round.toInt, pair90._2.getY.round.toInt, 0)
     */

    val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
    Util.writePng(bufImg2, pngFile)

    val txtFile = new File(file.getParent, file.getName.replace(".dcm", "_.txt"))
    Util.writeFile(txtFile, dicomImage.pixelsToText)

    Trace.trace(s"wrote $pngFile")
  }

  def main(args: Array[String]): Unit = {

    Trace.trace
    val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""")
    val rtimage = new DicomFile(file).attributeList.get
    val start = System.currentTimeMillis()
    measure(rtimage)
    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Elapsed: ${Util.elapsedTimeHumanFriendly(elapsed)}")
  }
}
