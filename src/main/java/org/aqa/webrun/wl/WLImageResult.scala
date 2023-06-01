package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.WinstonLutz
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

class WLImageResult(
    val imageStatus: WLImageStatus.ImageStatus,
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

  /** Elapsed time in ms of this slice since the first slice in the series was captured. */
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

  private def left_pix = edgesUnscaled.left + coarseX._1
  private def right_pix = edgesUnscaled.right + coarseX._1
  private def top_pix = edgesUnscaled.top + coarseY._1
  private def bottom_pix = edgesUnscaled.bottom + coarseY._1

  private def left_mm = trans.pix2IsoCoordX(left_pix)
  private def right_mm = trans.pix2IsoCoordX(right_pix)
  private def top_mm = trans.pix2IsoCoordY(top_pix)
  private def bottom_mm = trans.pix2IsoCoordY(bottom_pix)

  private def ballX_pix = brcX + coarseX._1
  private def ballY_pix = brcY + coarseY._1
  private def ballCenter_mm = trans.pix2Iso(ballX_pix, ballY_pix)
  private def boxCenterX_pix = (right_pix + left_pix) / 2.0
  private def boxCenterY_pix = (bottom_pix + top_pix) / 2.0
  private def boxCenter_mm = trans.pix2Iso(boxCenterX_pix, boxCenterY_pix)
  private def offsetX_mm = boxCenter_mm.getX - ballCenter_mm.getX
  private def offsetY_mm = boxCenter_mm.getY - ballCenter_mm.getY
  private def offset_mm = Math.sqrt((offsetX_mm * offsetX_mm) + (offsetY_mm * offsetY_mm))

  override def toString: String = {

    def badPixelListToString(list: Seq[WLBadPixel], name: String): String = {
      if (list == null)
        "NA"
      else
        "" +
          "    " + name + " pixels: " + list.size + "\n" +
          list.foldLeft("")((t, bad) => { t + "    " + bad + "\n" })
    }

    def opt(dFun: () => Double): String = {
      try {
        dFun().toString
      } catch {
        case _ : Throwable => "NA"
      }
    }

    "" +
      "    Directory: " + directory.getAbsolutePath + "\n" +
      s"    Status: $imageStatus\n" +
      "    Offset: " + (if (ok) new Point(offX, offY).toString else "not available") + "\n" +
      "    sqrt(x*x + y*y): " + (if (ok) offXY.formatted("%8.5f") else "not available") + "\n" +
      s"    Box  left      pix: ${opt(left_pix _)}\n" +
      s"    Box  right     pix: ${opt(right_pix _)}\n" +
      s"    Box  top       pix: ${opt(top_pix _)}\n" +
      s"    Box  bottom    pix: ${opt(bottom_pix _)}\n" +
      s"    Box center X,Y pix: ${opt(boxCenterX_pix _)}, ${opt(boxCenterY_pix _)}\n" +
      s"    Ball X,Y       pix: ${opt(ballX_pix _)}, ${opt(ballY_pix _)}\n" +
      s"    Box  left      iso mm: ${opt(left_mm _)}\n" +
      s"    Box  right     iso mm: ${opt(right_mm _)}\n" +
      s"    Box  top       iso mm: ${opt(top_mm _)}\n" +
      s"    Box  bottom    iso mm: ${opt(bottom_mm _)}\n" +
      s"    Ball X,Y       iso mm: ${opt(ballCenter_mm.getX _)},${opt(ballCenter_mm.getY _)}\n" +
      s"    Box X,Y        iso mm: ${opt(boxCenter_mm.getX _)}, ${opt(boxCenter_mm.getY _)}\n" +
      s"    offset X,Y     iso mm: ${opt(offsetX_mm _)}, ${opt(offsetY_mm _)}\n" +
      s"    offset         iso mm: ${opt(offset_mm _)}\n" +
      badPixelListToString(badPixelList, "bad") +
      badPixelListToString(marginalPixelList, "marginal")
  }

  val beamName: Option[String] = {
    if (runReq.rtplan.isDefined)
      Util.getBeamNameOfRtimage(runReq.rtplan.get, rtimage)
    else
      None
  }

  /**
    * Construct a database object from these results.
    * @return database row content
    */
  def toWinstonLutz: WinstonLutz = {

    val planned = if (runReq.rtplan.isDefined) Some(PlannedRectangle(rtplan = runReq.rtplan.get, rtimage)) else None

    val rtplanUID = {
      Phase2Util.referencedPlanUIDOpt(rtimage) match {
        case Some(uid) => uid
        case _         => ""
      }
    }

    val wl = WinstonLutz(
      winstonLutzPK = None,
      outputPK = extendedData.output.outputPK.get,
      rtimageUID = Util.sopOfAl(rtimage),
      rtplanUID = rtplanUID,
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
