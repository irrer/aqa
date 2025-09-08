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
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.webrun.wl.WLCoarseBox
import org.aqa.webrun.wl.WLMessage
import org.aqa.BiCubicImage
import org.aqa.Logging
import org.aqa.webrun.wl.WLPreprocessImage

import java.awt.Color
import java.awt.geom.Point2D
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import scala.annotation.tailrec

case class WLNonCardinal(rtimage: AttributeList, wlMessage: Option[WLMessage]) {}

case class Line(centerX: Double, centerY: Double, angle: Double) extends Logging {

  private val perpendicularAngle = Util.modulo360(angle + 90)

  /** The line perpendicular to this line, with the same center. */
  def perpendicular: Line = Line(centerX: Double, centerY: Double, perpendicularAngle)

  private val radians: Double = Math.toRadians(Util.modulo360(angle))

  private val cos: Double = Math.cos(radians)
  private val sin: Double = Math.sin(radians)
  private val tan: Double = Math.tan(radians)

  private val m: Double = -tan

  private val b: Double = centerY - (m * centerX)

  private def x2y(x: Double): Double = (m * x) + b
  private def y2x(y: Double): Double = if (m == 0) b else (y - b) / m

  /**
    * Find a point on the line offset from the center.
    * @param offset Distance along line from center.
    * @return New point on the line.
    */
  def pointOn(offset: Double): Point2D.Double = {

    val point = angle match {
      case _ if (angle.abs > 350) || (angle.abs < 1) => new Point2D.Double(centerX + offset, centerY)
      case _ if (angle > 179) && (angle < 181)       => new Point2D.Double(centerX + offset, centerY)
      case _ if (angle > 89) && (angle < 91)         => new Point2D.Double(centerX, centerY + offset)
      case _ if (angle > 269) && (angle < 271)       => new Point2D.Double(centerX, centerY + offset)

      case _ =>
        val useX = //
          ((angle > 45) && (angle < 135)) ||
            ((angle > 225) && (angle < 315))

        if (useX) {
          val x = centerX + (offset * cos)
          val y = x2y(x)
          new Point2D.Double(x, y)
        } else {
          val y = centerY + (offset * sin)
          val x = y2x(y)

          new Point2D.Double(x, y)
        }
    }
    point
  }

  def makeProfile(offsetLo: Double, offsetHi: Double, biCubicImage: BiCubicImage, width: Double, resolution: Double, bufImg: BufferedImage): Seq[Double] = {

    @tailrec
    def add(offset: Double, profile: Seq[Double]): Seq[Double] = {
      if (offset <= offsetHi) {
        val point = pointOn(offset)
        val line = Line(point.getX, point.getY, perpendicularAngle)

        val count = (width / resolution).round.toInt

        val pointList = (0 until count).map(i => line.pointOn((i * resolution) - (width / 2)))
        val sum =
          try {
            val mean = pointList.map(biCubicImage.get).sum / pointList.size
            Some(mean)
          } catch {
            case _: org.apache.commons.math3.exception.OutOfRangeException =>
              None // out of bounds - just use what we've got so far
          }
        if (sum.isDefined)
          add(offset + resolution, profile :+ sum.get)
        else
          profile
      } else
        profile
    }

    val profile = add(offsetLo, Seq())
    profile
  }
}

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object WLNonCardinal {

  def main(args: Array[String]): Unit = {

    Trace.trace

    // val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\0010.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\1\0002.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""") // rotated 315
    // val file = new File("""D:\tmp\wl\nonorth\1\0006.dcm""") // rotated 45
    // val file = new File("""D:\tmp\wl\nonorth\1\0001.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\psm\0018.dcm""")
    val file = new File("""D:\tmp\wl\nonorth\TB5_Aug_20\0002.dcm""")
    // val file = new File("""D:\tmp\wl\nonorth\BR1_Phase2\0014.dcm""")

    val al = new DicomFile(file).attributeList.get

    val colAngle = al.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    val dicomImage = {
      // Invert the pixels if necessary.
      WLPreprocessImage(al, "NonCardinal", None).preprocessedImage
    }

    val biCubicImage = BiCubicImage(dicomImage)

    val trans = new IsoImagePlaneTranslator(al)

    val pixPerMm = trans.iso2PixDistX(1)

    val pixBandWidth = 2 * pixPerMm

    val bufImg = dicomImage.toDeepColorBufferedImage(0.01)

    // val gc = ImageUtil.getGraphics(bufImg)
    val gc = bufImg.getGraphics.asInstanceOf[Graphics2D]

    gc.setColor(Color.black)

    if (true) {
      val text = "Collimator Angle: " + Util.fmtDbl(colAngle)
      ImageText.drawTextCenteredAt(gc, dicomImage.width / 2, 40, text)
    }

    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      // Trace.trace(Util.d2i(x1) + " : " + Util.d2i(y1) + " : " + Util.d2i(x2) + " : " + Util.d2i(y2))
      gc.drawLine(Util.d2i(x1), Util.d2i(y1), Util.d2i(x2), Util.d2i(y2))
    }

    def drawLineP(point1: Point2D.Double, point2: Point2D.Double): Unit =
      drawLine(point1.getX, point1.getY, point2.getX, point2.getY)

    val coarseAoi = WLCoarseBox(dicomImage, trans, wlMsg = None).locate()

    if (true) {
      gc.drawRect(coarseAoi.x, coarseAoi.y, coarseAoi.width, coarseAoi.height)
    }

    val xCenter = coarseAoi.getCenterX
    val yCenter = coarseAoi.getCenterY

    if (true) {
      val line = Line(xCenter, yCenter, colAngle)
      drawLineP(line.pointOn(100), line.pointOn(-100))

      val pLine = line.perpendicular
      gc.setColor(Color.white)
      drawLineP(pLine.pointOn(100), pLine.pointOn(-100))

      (0 until 20).foreach(i => {
        val p = line.pointOn(i * 4)
        val l = Line(p.getX, p.getY, pLine.angle)

        val lo = l.pointOn(-pixBandWidth)
        val hi = l.pointOn(pixBandWidth)

        drawLineP(lo, hi)
      })

      var resolution = 1.0
      while (resolution > 0.1) {
        val start = System.currentTimeMillis()
        val profile = line.makeProfile(0, 2000, biCubicImage, pixBandWidth, resolution, bufImg)
        val elapsed = "%8d".format(System.currentTimeMillis() - start)

        val min = profile.min

        val index = profile.indexOf(min)

        val edge = profile.dropRight(index)
        val position = LocateEdge.locateEdge(edge.map(_.toFloat).toIndexedSeq, (min + edge.max) / 2) * resolution

        Trace.trace(s"Elapsed: $elapsed   resolution: ${"%8.6f".format(resolution)}   Profile edge: $position")
        resolution = resolution * .9
      }
      // profile.foreach(println)

    }

    Trace.trace(s"colAngle: $colAngle")

    // ------------------------------------------------------------------------------------

    val cos0 = Math.cos(Math.toRadians(colAngle))

    val m0 = Math.tan(Math.toRadians(colAngle))

    val b0 = yCenter - (m0 * xCenter)

    def line0(xx: Double) = ((m0 * xx) + b0).toInt

    // ------------------------------------------------------------------------------------

    val m1 = Math.tan(Math.toRadians(colAngle + 90))

    val b1 = yCenter - (m1 * xCenter)

    def line90(xx: Double) = (m1 * xx) + b1

    // ------------------------------------------------------------------------------------

    if (false) {
      val x1 = 400
      val x2 = 900

      drawLine(x1, line0(x1), x2, line0(x2))

      val xHi = xCenter + (11 * cos0)
      bufImg.setRGB(xHi.toInt, line0(xHi), 0xfffff)

      val xLo = xCenter - (4 * cos0)
      bufImg.setRGB(xLo.toInt, line0(xLo), 0xfffff)

    }

    if (false) {
      gc.setColor(Color.white)

      val x1 = xCenter - pixBandWidth * 10
      val x2 = xCenter + pixBandWidth * 10

      drawLine(x1, line90(x1), x2, line90(x2))

    }

    if (false) {
      (0 until 500).foreach(i => {

        val x = cos0 * 5 * i + xCenter
        val bb = line0(x) - (m1 * x)

        def lineX(xx: Double) = (m1 * xx) + bb

        val xLo = x - pixBandWidth
        val xHi = x + pixBandWidth
        drawLine(xLo, lineX(xLo), xHi, lineX(xHi))

      })
    }

    if (false) {

      val rot = Rotator(al)

      val topLeft = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYTop)))
      val topRight = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYTop)))

      val bottomLeft = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXLeft, rot.jawsYBottom)))
      val bottomRight = rot.trans.iso2Pix(rot.rot(new Point2D.Double(rot.jawsXRight, rot.jawsYBottom)))

      Trace.trace(s"jawsYTop   : ${rot.jawsYTop.round}")
      Trace.trace(s"jawsYBottom: ${rot.jawsYBottom.round}")
      Trace.trace(s"jawsXLeft  : ${rot.jawsXLeft.round}")
      Trace.trace(s"jawsXRight : ${rot.jawsXRight.round}")

      Trace.trace(s"\n    topLeft: $topLeft\n    topRight: $topRight\n    bottomLeft: $bottomLeft\n    bottomRight: $bottomRight")

      drawLine(topLeft.getX, topLeft.getY, topRight.getX, topRight.getY)
      drawLine(topLeft.getX, topLeft.getY, bottomLeft.getX, bottomLeft.getY)
      drawLine(bottomRight.getX, bottomRight.getY, bottomLeft.getX, bottomLeft.getY)
      drawLine(topRight.getX, topRight.getY, bottomRight.getX, bottomRight.getY)
    }

    val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
    Util.writePng(bufImg, pngFile)

    val txtFile = new File(file.getParent, file.getName.replace(".dcm", "_.txt"))
    Util.writeFile(txtFile, dicomImage.pixelsToText)

    Trace.trace(s"wrote $pngFile")

    System.exit(0)
  }

}
