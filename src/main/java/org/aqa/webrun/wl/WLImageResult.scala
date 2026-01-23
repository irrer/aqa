package org.aqa.webrun.wl

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Util
import org.aqa.db.WinstonLutz
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.PlannedRectangle
import org.aqa.db.WinLutz360

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
 * @param imageStatus             Pass, fail, etc.
 * @param boxRelativeToBounds_mm  Center of box relative to coarse bounds.
 * @param ballRelativeToBounds_mm Center of ball relative to coarse bounds.
 * @param edgesUnscaled           unused
 * @param boxEdgesP               unknown
 * @param edgeSet                 all edge in information, including AOI for each edge.  Measurements are relative to each edge's bounding box.
 * @param directory               HTML directory for this image
 * @param rtimage                 DICOM image
 * @param pixels                  unknown
 * @param coarseAoiBounds_pix     Bounding box in pixels around entire box.
 * @param brcX                    X offset from coarseAoiBounds_pix to center of ball in pixels
 * @param brcY                    Y offset from coarseAoiBounds_pix to center of ball in pixels
 * @param badPixelList            List of bad pixels.
 * @param marginalPixelList       List of marginal pixels.
 * @param extendedData            Metadata
 * @param runReq                  all input data
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

  val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(rtimage))

  // @formatter:off
  private def left_pix  : Double = edgeSet.get.  left.absoluteEdge_pix
  private def right_pix : Double = edgeSet.get. right.absoluteEdge_pix
  private def top_pix   : Double = edgeSet.get.   top.absoluteEdge_pix
  private def bottom_pix: Double = edgeSet.get.bottom.absoluteEdge_pix
  // @formatter:on
  private def left_mm: Double = trans.pix2IsoCoordX(left_pix)

  private def right_mm: Double = trans.pix2IsoCoordX(right_pix)

  private def top_mm: Double = trans.pix2IsoCoordY(top_pix)

  private def bottom_mm: Double = trans.pix2IsoCoordY(bottom_pix)

  private def ballX_pix: Double = brcX.get + coarseAoiBounds_pix.get.x

  private def ballY_pix: Double = brcY.get + coarseAoiBounds_pix.get.y

  override def ballCenter_mm: Option[Point2d] = Some(trans.pix2Iso(new Point2d(ballX_pix, ballY_pix)))

  private def boxCenterX_pix: Double = (right_pix + left_pix) / 2.0

  private def boxCenterY_pix: Double = (bottom_pix + top_pix) / 2.0

  override def boxCenter_mm: Point2d = trans.pix2Iso(new Point2d(boxCenterX_pix, boxCenterY_pix))

  // private def offsetX_mm_def: Double = boxCenter_mm.getX - ballCenter_mm.getX

  // private def offsetY_mm_def: Double = boxCenter_mm.getY - ballCenter_mm.getY

  // private def offset_mm: Double = Math.sqrt((offsetX_mm_def * offsetX_mm_def) + (offsetY_mm_def * offsetY_mm_def))

  override def OffsetTop_mm: Option[Double] = Some(top_mm)

  override def OffsetBottom_mm: Option[Double] = Some(bottom_mm)

  override def OffsetLeft_mm: Option[Double] = Some(left_mm)

  override def OffsetRight_mm: Option[Double] = Some(right_mm)

  override def OffsetX1_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0 => OffsetLeft_mm
        case 90 => OffsetBottom_mm
        case 180 => OffsetRight_mm
        case 270 => OffsetTop_mm
      }
    }
    else
      None
  }

  override def OffsetX2_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0 => OffsetRight_mm
        case 90 => OffsetTop_mm
        case 180 => OffsetLeft_mm
        case 270 => OffsetBottom_mm
      }
    }
    else
      None
  }

  override def OffsetY1_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0 => OffsetBottom_mm
        case 90 => OffsetRight_mm
        case 180 => OffsetTop_mm
        case 270 => OffsetLeft_mm
      }
    }
    else
      None
  }

  override def OffsetY2_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0 => OffsetTop_mm
        case 90 => OffsetLeft_mm
        case 180 => OffsetBottom_mm
        case 270 => OffsetRight_mm
      }
    }
    else
      None
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
      ballCenterX_mm = if (ballCenter_mm.isDefined) ballCenter_mm.get.getX else Double.NaN,
      ballCenterY_mm = if (ballCenter_mm.isDefined) ballCenter_mm.get.getY else Double.NaN,
      topEdgePlanned_mm = planned.map(_.top),
      bottomEdgePlanned_mm = planned.map(_.bottom),
      leftEdgePlanned_mm = planned.map(_.left),
      rightEdgePlanned_mm = planned.map(_.right)
    )
    val newWl = wl.copy(winstonLutzPK = None)
    newWl
  }

  // ----------------------------------------------------------------------------------------

  // support for WLResult

  override def offsetX_mm: Double = offX_mm

  override def offsetY_mm: Double = offY_mm

  override def getImageStatus: WLImageStatus.Value = imageStatus

  override def convertToDB: Either[WinstonLutz, WinLutz360] = Left(toWinstonLutz)

  override def attrList: AttributeList = rtimage

}
