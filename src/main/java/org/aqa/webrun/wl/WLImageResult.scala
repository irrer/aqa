package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.webrun.wl
import org.aqa.webrun.ExtendedData

import java.io.File
import java.text.SimpleDateFormat
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
    val rtimage: AttributeList,
    val pixels: Array[Array[Float]],
    val badPixelList: List[WLBadPixel],
    val marginalPixelList: List[WLBadPixel],
    val extendedData: ExtendedData
) {
  val ok: Boolean = !((boxP == null) || (ballP == null))
  val offX: Double = if (ok) boxP.x - ballP.x else -1
  val offY: Double = if (ok) boxP.y - ballP.y else -1
  val offXY: Double = if (ok) Math.sqrt((offX * offX) + (offY * offY)) else -1
  val date = new Date

  val box: Point = if (boxP == null) new Point(-1, -1) else boxP
  val ball: Point = if (ballP == null) new Point(-1, -1) else ballP
  val boxEdges: Edges = if (boxEdgesP == null) new Edges(-1, -1, -1, -1) else boxEdgesP

  val elapsedTime_ms = {
    val ms = DicomUtil.getTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime).get.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  val gantryRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(rtimage))
  val collimatorRounded_deg = Util.angleRoundedTo90(Util.collimatorAngle(rtimage))

  val gantryRounded_txt = "G" + gantryRounded_deg.formatted(("%03d"))
  val collimatorRounded_txt = "C" + collimatorRounded_deg.formatted(("%03d"))
  val elapsedTime_txt = new SimpleDateFormat("MM:ss").format(new Date(elapsedTime_ms))

  val imageName: String = gantryRounded_txt + collimatorRounded_txt + elapsedTime_txt

  def attr(tag: AttributeTag): String = rtimage.get(tag).getSingleStringValueOrNull

  val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(rtimage)) //attrFloat(TagByName.GantryAngle)

  override def toString: String = {

    def badPixelListToString(list: List[WLBadPixel], name: String): String = {
      if (list == null)
        "NA"
      else
        "" +
          "    " + name + " pixels: " + list.size + "\n" +
          list.foldLeft("")((t, bad) => { t + "    " + bad + "\n" })
    }

    // WLImageResult(status, boxP = null, ballP = null,boxEdgesP =  null, directory = subDir, rtimage = rtimage,  pixels = null, badPixelList =  null, marginalPixelList, extendedData)

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
