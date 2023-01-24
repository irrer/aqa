package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import org.aqa.Util
import org.aqa.webrun.wl

import java.io.File
import java.util.Date

class Point(val x: Double, val y: Double) {
  override def toString: String = {
    "X: " + x.formatted("%8.5f") + "    " +
      "Y: " + y.formatted("%8.5f")
  }
}

/** Describe the edges of a box. */
class Edges(val top: Double, val bottom: Double, val left: Double, val right: Double) {
  override def toString: String = {
    def prnt(name: String, value: Double) = "    " + name + value.formatted("%7.4f\n")

    prnt("Edge top     : ", top) +
      prnt("Edge bottom  : ", bottom) +
      prnt("Edge left    : ", left) +
      prnt("Edge right   : ", right) +
      prnt("Width        : ", right - left) +
      prnt("Height       : ", bottom - top) +
      prnt("Aspect Ratio : ", (right - left) / (bottom - top))
  }
}

object ImageStatus extends Enumeration {
  type ImageStatus = Value

  val Passed: wl.ImageStatus.Value = Value("Passed")
  val BoxNotFound: wl.ImageStatus.Value = Value("Failed: Box Not Found")
  val OffsetLimitExceeded: wl.ImageStatus.Value = Value("Failed: Offset Limit Exceeded")
  val BallMissing: wl.ImageStatus.Value = Value("Failed: No ball in box")
  val BallAreaNoisy: wl.ImageStatus.Value = Value("Failed: Ball image area is noisy")
  val EdgeExtentsNotFound: wl.ImageStatus.Value = Value("Failed: Extents of edges not found")
  val UnknownTreatmentMachine: wl.ImageStatus.Value = Value("Failed: Unknown treatment machine")
  val UnexpectedError: wl.ImageStatus.Value = Value("Failed: Unexpected Error")
}

class WLImageResult(
    val imageStatus: ImageStatus.ImageStatus,
    boxP: Point,
    ballP: Point,
    boxEdgesP: Edges,
    val directory: File,
    // val imageMetaData: ImageMetaData,
    val attributeList: AttributeList,
    val pixels: Array[Array[Float]],
    val badPixelList: List[WLBadPixel],
    val marginalPixelList: List[WLBadPixel]
) {
  val ok: Boolean = !((boxP == null) || (ballP == null))
  val offX: Double = if (ok) boxP.x - ballP.x else -1
  val offY: Double = if (ok) boxP.y - ballP.y else -1
  val offXY: Double = if (ok) Math.sqrt((offX * offX) + (offY * offY)) else -1
  val date = new Date

  val box: Point = if (boxP == null) new Point(-1, -1) else boxP
  val ball: Point = if (ballP == null) new Point(-1, -1) else ballP
  val boxEdges: Edges = if (boxEdgesP == null) new Edges(-1, -1, -1, -1) else boxEdgesP

  def attr(tag: AttributeTag): String = attributeList.get(tag).getSingleStringValueOrNull

  val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(attributeList)) //attrFloat(TagByName.GantryAngle)

  override def toString: String = {

    def badPixelListToString(list: List[WLBadPixel], name: String): String = {
      "" +
        "    " + name + " pixels: " + list.size + "\n" +
        list.foldLeft("")((t, bad) => { t + "    " + bad + "\n" })
    }

    "" +
      "    Directory: " + directory.getAbsolutePath + "\n" +
      "    Status: " + imageStatus + "\n" +
      "    Box Center: " + box + "\n" +
      "    Ball Center: " + ball + "\n" +
      "    Box Edges: " + boxEdges + "\n" +
      "    Offset: " + (if (ok) new Point(offX, offY).toString else "not available") + "\n" +
      "    sqrt(x*x + y*y): " + (if (ok) offXY.formatted("%8.5f") else "not available") + "\n" +
      badPixelListToString(badPixelList, "bad") +
      badPixelListToString(marginalPixelList, "marginal")
  }
}
