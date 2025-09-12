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
import org.aqa.DicomFile
import org.aqa.Logging

import java.awt.Color
import java.awt.Graphics2D
import java.io.File

case class WLMeasureEdges(rtimage: AttributeList, wlMessage: Option[WLMessage]) extends Logging {}

/**
  * Quick program to show the center raw pixel values from DICOM files.
  */
object WLMeasureEdges extends Logging {

  case class PointSet(line: WLLine, offsetPos: Double, offsetNeg: Double) extends Logging {
    val middle: Double = (offsetPos + offsetNeg) / 2

    override def toString: String = s"line: $line   offsetPos: $offsetPos   offsetNeg: $offsetNeg   middle: $middle"
  }

  def getMidPoint(line: WLLine, biCubicImage: BiCubicImage, pixBandWidth: Double, resolution: Double, offTheEdge: Double): PointSet = {

    def minPoint(start: Double, finish: Double): Double = {
      val profile = line.makeProfile(start, finish, biCubicImage, pixBandWidth, resolution)
      Trace.trace("profile:\n" + profile.take(4).mkString("\n"))
      val min = profile.min
      val Index = profile.indexOf(min)
      val partialProfile = profile.drop(Index)
      val halfway = (min + profile.max) / 2
      val edgeUnscaled = LocateEdge.locateEdge(partialProfile.map(_.toFloat).toIndexedSeq, halfway)
      val edge = (edgeUnscaled + Index) * resolution
      val edgeSigned = if (start < finish) edge else -edge
      edgeSigned
    }

    val pointSet = PointSet(line, minPoint(0, offTheEdge), minPoint(0, -offTheEdge))
    Trace.trace(pointSet)
    pointSet
  }

  def measure(rtimage: AttributeList): Unit = {

    Trace.trace

    val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""")

    val colAngle = rtimage.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head

    val dicomImage = {
      // Invert the pixels if necessary.
      WLPreprocessImage(rtimage, None).preprocessedImage
    }

    val bufImg = dicomImage.toDeepColorBufferedImage(0.01)

    val biCubicImage = BiCubicImage(dicomImage, Some(bufImg))

    val trans = new IsoImagePlaneTranslator(rtimage)

    val pixPerMm = (trans.iso2PixDistX(1) + trans.iso2PixDistY(1)) / 2

    val pixBandWidth = 4 * pixPerMm

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

    val coarseAoi = WLCoarseBox(dicomImage, trans, wlMsg = None).locate()

    if (true) {
      gc.drawRect(coarseAoi.x, coarseAoi.y, coarseAoi.width, coarseAoi.height)
    }

    val xCenter = coarseAoi.getCenterX
    val yCenter = coarseAoi.getCenterY

    Trace.trace(s"colAngle: $colAngle")

    // ------------------------------------------------------------------------------------

    val line0 = WLLine(xCenter, yCenter, colAngle)

    val resolution = 0.2 // TODO make configurable

    val offTheEdge = dicomImage.width + dicomImage.height

    // this is the center point that was found by using a thin band of pixels along either line.
    val centerPointA = {
      val line0A = {
        val p = line0.pointOn(getMidPoint(line0, biCubicImage, pixBandWidth, resolution, offTheEdge).middle)
        WLLine(p.getX, p.getY, line0.perpendicular.angle)
      }

      // def getMidPoint(line: WLLine, biCubicImage: BiCubicImage, pixBandWidth: Double, resolution: Double, offTheEdge: Double): PointSet = {

      val line90A = {
        val perpendicular = line0.perpendicular
        val p = perpendicular.pointOn(getMidPoint(perpendicular, biCubicImage, pixBandWidth, resolution, offTheEdge).middle)
        WLLine(p.getX, p.getY, perpendicular.perpendicular.angle)
      }

      line0A.intersection(line90A)
    }

    Trace.trace(s" center coarse:  $xCenter  $yCenter")
    Trace.trace(s" center fine: $centerPointA")
    Trace.trace(s" dist: " + centerPointA.distance(xCenter, yCenter))

    val line0A = WLLine(centerPointA.getX, centerPointA.getY, line0.angle)

    /*
    val pair0 = getMidPoint(line0)

    bufImg.setRGB(pair0._1.getX.round.toInt, pair0._1.getY.round.toInt, 0)
    bufImg.setRGB(pair0._2.getX.round.toInt, pair0._2.getY.round.toInt, 0)

    val pair90 = getMidPoint(line0.perpendicular)

    bufImg.setRGB(pair90._1.getX.round.toInt, pair90._1.getY.round.toInt, 0)
    bufImg.setRGB(pair90._2.getX.round.toInt, pair90._2.getY.round.toInt, 0)
     */

    val pngFile = new File(file.getParent, file.getName.replace("dcm", "png"))
    Util.writePng(bufImg, pngFile)

    val txtFile = new File(file.getParent, file.getName.replace(".dcm", "_.txt"))
    Util.writeFile(txtFile, dicomImage.pixelsToText)

    Trace.trace(s"wrote $pngFile")

    System.exit(0)
  }

  def main(args: Array[String]): Unit = {

    Trace.trace
    val file = new File("""D:\tmp\wl\nonorth\1\0005.dcm""")
    val rtimage = new DicomFile(file).attributeList.get
    measure(rtimage)
  }
}
