package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.PlannedRectangle
import org.aqa.db.WinLutz360

import java.awt.geom.Point2D
import java.awt.Rectangle
import java.io.File
import java.sql.Timestamp
import java.util.Date
import javax.vecmath.Point2d

/** Describe the edges of a box. */
class Edges(val top: Double, val bottom: Double, val left: Double, val right: Double) {
  override def toString: String = {
    def prt(name: String, value: Double) = "    " + name + "%7.4f\n".format(value)

    prt("Edge top     : ", top) +
      prt("Edge bottom  : ", bottom) +
      prt("Edge left    : ", left) +
      prt("Edge right   : ", right) +
      prt("Width        : ", right - left) +
      prt("Height       : ", bottom - top) +
      prt("Aspect Ratio : ", (right - left) / (bottom - top))
  }
}

/**
  * Encapsulate the results of the measurements for one image.
  *
  * @param imageStatus Pass, fail, etc.
  * @param boxRelativeToBounds_mm Center of box relative to coarse bounds.
  * @param ballRelativeToBounds_mm Center of ball relative to coarse bounds.
  * @param edgesUnscaled unused
  * @param boxEdgesP unknown
  * @param edgeSet all edge in information, including AOI for each edge.  Measurements are relative to each edge's bounding box.
  * @param directory HTML directory for this image
  * @param rtimage DICOM image
  * @param pixels unknown
  * @param coarseAoiBounds_pix Bounding box in pixels around entire box.
  * @param brcX X offset from coarseAoiBounds_pix to center of ball in pixels
  * @param brcY Y offset from coarseAoiBounds_pix to center of ball in pixels
  * @param badPixelList List of bad pixels.
  * @param marginalPixelList List of marginal pixels.
  * @param extendedData Metadata
  * @param runReq all input data
  */
case class WLImageResult(
    imageStatus: WLImageStatus.ImageStatus,
    boxRelativeToBounds_mm: Option[Point2d] = None,
    ballRelativeToBounds_mm: Option[Point2d] = None,
    edgesUnscaled: Option[Edges] = None,
    boxEdgesP: Option[Edges] = None,
    edgeSet: Option[WLEdgeSet] = None,
    directory: File,
    rtimage: AttributeList,
    pixels: Option[Array[Array[Float]]] = None,
    coarseAoiBounds_pix: Option[Rectangle] = None,
    brcX: Option[Double] = None,
    brcY: Option[Double] = None,
    badPixelList: Seq[WLBadPixel],
    marginalPixelList: Seq[WLBadPixel],
    extendedData: ExtendedData,
    runReq: WLRunReq
) extends WLResult(extendedData, runReq) {
  val ok: Boolean = boxRelativeToBounds_mm.isDefined && ballRelativeToBounds_mm.isDefined
  val offX_mm: Double = if (ok) boxRelativeToBounds_mm.get.x - ballRelativeToBounds_mm.get.x else -1
  val offY_mm: Double = if (ok) boxRelativeToBounds_mm.get.y - ballRelativeToBounds_mm.get.y else -1
  val offXY_mm: Double = if (ok) Math.sqrt((offX_mm * offX_mm) + (offY_mm * offY_mm)) else -1
  val date = new Date
  private val trans = new IsoImagePlaneTranslator(rtimage)

  val box: Point2d = if (boxRelativeToBounds_mm.isEmpty) new Point2d(-1, -1) else boxRelativeToBounds_mm.get
  val ball: Point2d = if (ballRelativeToBounds_mm.isEmpty) new Point2d(-1, -1) else ballRelativeToBounds_mm.get
  val boxEdges: Edges = if (boxEdgesP.isEmpty) new Edges(-1, -1, -1, -1) else boxEdgesP.get

  // private val gantry_deg: Double = Util.gantryAngle(rtimage)
  // private val collimator_deg: Double = Util.collimatorAngle(rtimage)

  def attr(tag: AttributeTag): String = {
    DicomUtil.findAllSingle(rtimage, tag).map(_.getSingleStringValueOrEmptyString()).head
  }

  val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(rtimage)) //attrFloat(TagByName.GantryAngle)

  // @formatter:off
  private def left_pix  : Double = edgeSet.get.  left.pos_pix + edgeSet.get.  left.bounds.x
  private def right_pix : Double = edgeSet.get. right.pos_pix + edgeSet.get. right.bounds.x
  private def top_pix   : Double = edgeSet.get.   top.pos_pix + edgeSet.get.   top.bounds.y
  private def bottom_pix: Double = edgeSet.get.bottom.pos_pix + edgeSet.get.bottom.bounds.y
  // @formatter:on
  private def left_mm: Double = trans.pix2IsoCoordX(left_pix)

  private def right_mm: Double = trans.pix2IsoCoordX(right_pix)

  private def top_mm: Double = trans.pix2IsoCoordY(top_pix)

  private def bottom_mm: Double = trans.pix2IsoCoordY(bottom_pix)

  private def ballX_pix: Double = brcX.get + coarseAoiBounds_pix.get.x

  private def ballY_pix: Double = brcY.get + coarseAoiBounds_pix.get.y

  private def ballCenter_mm: Point2D.Double = trans.pix2Iso(ballX_pix, ballY_pix)

  private def boxCenterX_pix: Double = (right_pix + left_pix) / 2.0

  private def boxCenterY_pix: Double = (bottom_pix + top_pix) / 2.0

  private def boxCenter_mm: Point2D.Double = trans.pix2Iso(boxCenterX_pix, boxCenterY_pix)

  private def offsetX_mm_def: Double = boxCenter_mm.getX - ballCenter_mm.getX

  private def offsetY_mm_def: Double = boxCenter_mm.getY - ballCenter_mm.getY

  private def offset_mm: Double = Math.sqrt((offsetX_mm_def * offsetX_mm_def) + (offsetY_mm_def * offsetY_mm_def))

  override def toString: String = {

    def opt(dFun: () => Double): String = {
      try {
        dFun().toString
      } catch {
        case _: Throwable => "NA"
      }
    }

    /*
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
     */
    ""
  }

  /**
   * Construct a database object from these results.
   *
   * @return database row content
   */
  def toWinstonLutz: WinstonLutz = {

    val planned = if (runReq.rtplan.isDefined) Some(PlannedRectangle(rtplan = runReq.rtplan.get, rtimage)) else None

    val rtplanUID = {
      Phase2Util.referencedPlanUIDOpt(rtimage) match {
        case Some(uid) => uid
        case _ => ""
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
      tableAngle_deg = Some(tableAngle_deg),
      dataDate = new Timestamp(contentTime.getTime),
      topEdge_mm = top_mm,
      bottomEdge_mm = bottom_mm,
      leftEdge_mm = left_mm,
      rightEdge_mm = right_mm,
      ballCenterX_mm = ballCenter_mm.getX,
      ballCenterY_mm = ballCenter_mm.getY,
      topEdgePlanned_mm = planned.map(_.top),
      bottomEdgePlanned_mm = planned.map(_.bottom),
      leftEdgePlanned_mm = planned.map(_.left),
      rightEdgePlanned_mm = planned.map(_.right)
    )
    wl
  }

  // ----------------------------------------------------------------------------------------

  // support for WLResult

  override def offsetX_mm: Double = offX_mm

  override def offsetY_mm: Double = offY_mm

  override def getImageStatus: WLImageStatus.Value = imageStatus

  override def convertToDB: Either[WinstonLutz, WinLutz360] = Left(toWinstonLutz)

  override def attrList: AttributeList = rtimage

}
