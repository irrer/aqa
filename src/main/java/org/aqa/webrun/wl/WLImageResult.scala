package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.webrun.wl
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.PlannedRectangle

import java.io.File
import java.sql.Timestamp
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
    def prt(name: String, value: Double) = "    " + name + value.formatted("%7.4f\n")

    prt("Edge top     : ", top) +
      prt("Edge bottom  : ", bottom) +
      prt("Edge left    : ", left) +
      prt("Edge right   : ", right) +
      prt("Width        : ", right - left) +
      prt("Height       : ", bottom - top) +
      prt("Aspect Ratio : ", (right - left) / (bottom - top))
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
    edgesUnscaled: Edges,
    boxEdgesP: Edges,
    val directory: File,
    val rtimage: AttributeList,
    val pixels: Array[Array[Float]],
    coarseX: (Int, Int),
    coarseY: (Int, Int),
    brcX: Double,
    brcY: Double,
    val badPixelList: Seq[WLBadPixel],
    val marginalPixelList: Seq[WLBadPixel],
    val extendedData: ExtendedData,
    val runReq: WLRunReq
) {
  val ok: Boolean = !((boxP == null) || (ballP == null))
  val offX: Double = if (ok) boxP.x - ballP.x else -1
  val offY: Double = if (ok) boxP.y - ballP.y else -1
  val offXY: Double = if (ok) Math.sqrt((offX * offX) + (offY * offY)) else -1
  val date = new Date
  private val trans = new IsoImagePlaneTranslator(rtimage)

  val box: Point = if (boxP == null) new Point(-1, -1) else boxP
  val ball: Point = if (ballP == null) new Point(-1, -1) else ballP
  val boxEdges: Edges = if (boxEdgesP == null) new Edges(-1, -1, -1, -1) else boxEdgesP

  val contentTime: Date = {
    val content = Util.dicomGetTimeAndDate(rtimage, TagByName.ContentDate, TagByName.ContentTime)
    val acquisition = Util.dicomGetTimeAndDate(rtimage, TagByName.AcquisitionDate, TagByName.AcquisitionTime)
    val list = Seq(content, acquisition).flatten
    list.head
  }

  val elapsedTime_ms: Long = {
    val ms = contentTime.getTime
    val elapsed_ms = ms - extendedData.output.dataDate.get.getTime
    elapsed_ms
  }

  private val gantry_deg: Double = Util.gantryAngle(rtimage)
  private val collimator_deg: Double = Util.collimatorAngle(rtimage)
  private def angleRoundedTo22_5(angle: Double): Double = (((angle + 3600) / 22.5).round.toInt % 16) * 22.5 // convert to nearest multiple of 22.5 degrees

  val gantryRounded_deg: Int = Util.angleRoundedTo90(gantry_deg)
  val collimatorRounded_deg: Double = angleRoundedTo22_5(collimator_deg)

  val gantryRounded_txt: String = "G" + gantryRounded_deg.formatted("%03d")
  val collimatorRounded_txt: String = "C" + {
    if (collimatorRounded_deg.toInt == collimatorRounded_deg)
      collimatorRounded_deg.toInt.formatted("%03d")
    else
      collimatorRounded_deg.formatted("%5.1f")
  }

  val elapsedTime_txt: String = {
    val totalSeconds = elapsedTime_ms / 1000
    (totalSeconds / 60) + ":" + (totalSeconds % 60).formatted("%02d")
  }

  val imageName: String = gantryRounded_txt + " " + collimatorRounded_txt + " " + elapsedTime_txt

  def subDirName: String = {
    val min = elapsedTime_ms / (60 * 1000)
    val sec = (elapsedTime_ms / 1000) % 60
    val name = min.formatted("%d") + "_" + sec.formatted("%02d") + "__" + gantryRounded_txt + "__" + collimatorRounded_txt
    name
  }

  def subDir: File = {
    val dir = new File(extendedData.output.dir, subDirName)
    dir.mkdirs()
    dir
  }

  def attr(tag: AttributeTag): String = {
    DicomUtil.findAllSingle(rtimage, tag).map(_.getSingleStringValueOrEmptyString()).head
  }

  val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(rtimage)) //attrFloat(TagByName.GantryAngle)

  private val left_pix = edgesUnscaled.left + coarseX._1
  private val right_pix = edgesUnscaled.right + coarseX._1
  private val top_pix = edgesUnscaled.top + coarseY._1
  private val bottom_pix = edgesUnscaled.bottom + coarseY._1

  private val left_mm = trans.pix2IsoCoordX(left_pix)
  private val right_mm = trans.pix2IsoCoordX(right_pix)
  private val top_mm = trans.pix2IsoCoordY(top_pix)
  private val bottom_mm = trans.pix2IsoCoordY(bottom_pix)

  private val ballX_pix = brcX + coarseX._1
  private val ballY_pix = brcY + coarseY._1
  private val ballCenter_mm = trans.pix2Iso(ballX_pix, ballY_pix)
  private val boxCenterX_pix = (right_pix + left_pix) / 2.0
  private val boxCenterY_pix = (bottom_pix + top_pix) / 2.0
  private val boxCenter_mm = trans.pix2Iso(boxCenterX_pix, boxCenterY_pix)
  private val offsetX_mm = boxCenter_mm.getX - ballCenter_mm.getX
  private val offsetY_mm = boxCenter_mm.getY - ballCenter_mm.getY
  private val offset_mm = Math.sqrt((offsetX_mm * offsetX_mm) + (offsetY_mm * offsetY_mm))

  override def toString: String = {

    def badPixelListToString(list: Seq[WLBadPixel], name: String): String = {
      if (list == null)
        "NA"
      else
        "" +
          "    " + name + " pixels: " + list.size + "\n" +
          list.foldLeft("")((t, bad) => { t + "    " + bad + "\n" })
    }

    "" +
      "    Directory: " + directory.getAbsolutePath + "\n" +
      s"    Status: $imageStatus\n" +
      "    Offset: " + (if (ok) new Point(offX, offY).toString else "not available") + "\n" +
      "    sqrt(x*x + y*y): " + (if (ok) offXY.formatted("%8.5f") else "not available") + "\n" +
      s"    Box  left      pix: $left_pix\n" +
      s"    Box  right     pix: $right_pix\n" +
      s"    Box  top       pix: $top_pix\n" +
      s"    Box  bottom    pix: $bottom_pix\n" +
      s"    Box center X,Y pix: $boxCenterX_pix, $boxCenterY_pix\n" +
      s"    Ball X,Y       pix: $ballX_pix, $ballY_pix\n" +
      s"    Box  left      iso mm: $left_mm\n" +
      s"    Box  right     iso mm: $right_mm\n" +
      s"    Box  top       iso mm: $top_mm\n" +
      s"    Box  bottom    iso mm: $bottom_mm\n" +
      s"    Ball X,Y       iso mm: ${ballCenter_mm.getX},${ballCenter_mm.getY}\n" +
      s"    Box X,Y        iso mm: ${boxCenter_mm.getX}, ${boxCenter_mm.getY}\n" +
      s"    offset X,Y     iso mm: $offsetX_mm, $offsetY_mm\n" +
      s"    offset         iso mm: $offset_mm\n" +
      badPixelListToString(badPixelList, "bad") +
      badPixelListToString(marginalPixelList, "marginal")
  }

  /**
    * Construct a database object from these results.
    * @return database row content
    */
  def toWinstonLutz: WinstonLutz = {

    val beamName: Option[String] = {
      if (runReq.rtplan.isDefined)
        Util.getBeamNameOfRtimage(runReq.rtplan.get, rtimage)
      else
        None
    }

    val planned = if (runReq.rtplan.isDefined) Some(PlannedRectangle(rtplan = runReq.rtplan.get, rtimage)) else None

    val wl = WinstonLutz(
      winstonLutzPK = None,
      outputPK = extendedData.output.outputPK.get,
      rtimageUID = Util.sopOfAl(rtimage),
      rtplanUID = Phase2Util.referencedPlanUID(rtimage),
      beamName = beamName,
      gantryAngle_deg = gantry_deg,
      collimatorAngle_deg = collimator_deg,
      dataDate = new Timestamp(contentTime.getTime),
      topEdge_mm = top_mm,
      bottomEdge_mm = bottom_mm,
      leftEdge_mm = left_mm,
      rightEdge_mm = right_mm,
      ballX_mm = ballCenter_mm.getX,
      ballY_mm = ballCenter_mm.getY,
      topEdgePlanned_mm = planned.map(_.top),
      bottomEdgePlanned_mm = planned.map(_.bottom),
      leftEdgePlanned_mm = planned.map(_.left),
      rightEdgePlanned_mm = planned.map(_.right)
    )
    wl
  }
}
