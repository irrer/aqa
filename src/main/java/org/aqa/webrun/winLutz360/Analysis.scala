package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.BiCubicImage
import org.aqa.db.WinLutz360
import org.aqa.db.WinstonLutz
import org.aqa.webrun.wl.WLImageStatus
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLResult
import org.aqa.PlannedRectangle
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.WLImageUtil

import java.awt.image.BufferedImage
import java.sql.Timestamp
import javax.vecmath.Point2d

case class Analysis(extendedData: ExtendedData, al: AttributeList, wlRunReq: WLRunReq, machineWL: MachineWL, wlMessage: Option[WLMessage]) extends WLResult(extendedData, wlRunReq) {

  // Invert the pixels if necessary.
  val preprocessedImage: DicomImage = WLPreprocessImage(al, None).preprocessedImage
  val trans = new IsoImagePlaneTranslator(al)

  /** The beam center.  Usually 0,0, but might be offset.  If it is offset, then the plan must
    * be available to know what it is.  If not available, then it defaults to 0,0
    */
  val beamCenter_mm: Point2d = {
    try {
      val planBeam = Util.getBeamOfRtimage(wlRunReq.rtplan.get, al)
      val RTImagePosition = DicomUtil.findAllSingle(planBeam.get, TagByName.RTImagePosition).head.getDoubleValues

      val x = RTImagePosition.head / trans.beamExpansionRatio
      val y = RTImagePosition(1) / trans.beamExpansionRatio
      new Point2d(-x, y)
    } catch {
      case _: Throwable =>
        new Point2d(0, 0)
    }
  }

  private val biCubicImage = BiCubicImage(preprocessedImage)
  val edge = EdgeAnalysis(preprocessedImage, al, biCubicImage, wlRunReq, wlMessage, trans, beamCenter_mm)
  val ball = Ball(edge.edgeSet: EdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage, trans, al, wlMessage, beamCenter_mm)

  /** Take the mean of the pixels that are in the center of the ball and use those to establish the point in the brightest color level. */
  val maxPixelValue: Float = {
    val offsetList = -2 until 3
    val center_pix = edge.edgeSet.center_pix
    val valueList = for (x <- offsetList; y <- offsetList) yield preprocessedImage.get(center_pix.x.toInt + x, center_pix.y.toInt + y)
    val mean = valueList.sum / valueList.size
    mean
  }

  /** Take the mean of some of the lowest pixel values to establish the dimmest extreme of the color map. */
  val minPixelValue: Float = {
    val drop = 5 // number of values to drop, in case there are some bad ones.
    val take = 10 // number of values to use to find the mean.
    val sortedPixels = preprocessedImage.pixelData.flatten.sorted
    sortedPixels.slice(drop, drop + take).sum / 10
  }

  val approxImg: BufferedImage = EdgeSetImage.makeImage(edge.approximateEdgeSet, preprocessedImage, scale = 1, al)

  /** Scale (magnification factor) that images should be drawn at. */
  val scale: Int = WLImageUtil.calculateCloseupScale(al)
  val img: BufferedImage = EdgeSetImage.makeImage(edge.edgeSet, preprocessedImage, scale = scale, al)

  private val validator: Validate = Validate(edge, ball, machineWL, wlMessage)

  val statusList: Seq[Validate.ValidationStatus] = validator.statusList

  val status: WLImageStatus.Value = if (statusList.isEmpty) WLImageStatus.UnexpectedError else validator.statusList.head.status

  private val statusMessage: String = if (statusList.isEmpty) "No status returned by validation." else validator.statusList.head.msg

  wlMessage.foreach(_.info(s"Status: $status : $statusMessage"))

  override def OffsetX1_mm: Option[Double] = if (originIsDefined) Some(edge.x1DistanceToOrigin) else None
  override def OffsetX2_mm: Option[Double] = if (originIsDefined) Some(edge.x2DistanceToOrigin) else None
  override def OffsetY1_mm: Option[Double] = if (originIsDefined) Some(edge.y1DistanceToOrigin) else None
  override def OffsetY2_mm: Option[Double] = if (originIsDefined) Some(edge.y2DistanceToOrigin) else None

  override def OffsetTop_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0   => OffsetY2_mm
        case 90  => OffsetX2_mm
        case 180 => OffsetY1_mm
        case 270 => OffsetX1_mm
      }
    } else
      None
  }

  override def OffsetBottom_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0   => OffsetY1_mm
        case 90  => OffsetX1_mm
        case 180 => OffsetY2_mm
        case 270 => OffsetX2_mm
      }
    } else
      None
  }

  override def OffsetLeft_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0   => OffsetX1_mm
        case 90  => OffsetY2_mm
        case 180 => OffsetX2_mm
        case 270 => OffsetY1_mm
      }
    } else
      None
  }

  override def OffsetRight_mm: Option[Double] = {
    if (isCardinal) {
      collimatorRoundedTo90 match {
        case 0   => OffsetX2_mm
        case 90  => OffsetY1_mm
        case 180 => OffsetX1_mm
        case 270 => OffsetY2_mm
      }
    } else
      None
  }

  override def boxCenter_mm: Point2d = trans.pix2Iso(edge.edgeSet.center_pix)
  override def ballCenter_mm: Option[Point2d] = ball.center_mm

  /**
    * Determine if the origin (center of beam) is defined. It is defined if either:
    *
    * - The RTPLAN is available.
    *
    * - It is assumed to be 0,0, and both pairs of opposing edges are nearly equidistant from 0,0.
    */
  private val originIsDefined: Boolean = {
    val maxDistanceError_mm = 5.0
    def xDiff = (edge.x1DistanceToOrigin - edge.x2DistanceToOrigin).abs
    def yDiff = (edge.y1DistanceToOrigin - edge.y2DistanceToOrigin).abs
    def isClose = (xDiff < maxDistanceError_mm) || (yDiff < maxDistanceError_mm)

    wlRunReq.rtplan.isDefined || isClose
  }

  private def makeWinLutz360: WinLutz360 = {
    val beamName: Option[String] = {
      if (wlRunReq.rtplan.isDefined)
        Util.getBeamNameOfRtimage(wlRunReq.rtplan.get, al)
      else
        None
    }

    val tableAngle_deg: Double = DicomUtil.findAllSingle(al, TagByName.PatientSupportAngle).head.getDoubleValues.head

    val plannedRectangle: Option[PlannedRectangle] = {
      if (wlRunReq.rtplan.isDefined)
        Some(PlannedRectangle(wlRunReq.rtplan.get, al))
      else
        None
    }

    val dataDate = new Timestamp(WLImageUtil.timeOf(al).getTime)

    val plannedEdgeSet = {
      if (wlRunReq.rtplan.isDefined) {
        Some(new PlannedEdgeSet(wlRunReq.rtplan.get, al))
      } else
        None
    }

    val winLutz360: WinLutz360 = WinLutz360(
      winLutz360PK = None,
      outputPK = extendedData.outputPK,
      rtimageUID = Util.sopOfAl(al),
      beamName = beamName,
      gantryAngle_deg = Util.gantryAngle(al),
      collimatorAngle_deg = Util.collimatorAngle(al),
      tableAngle_deg = Some(tableAngle_deg),
      dataDate = dataDate,
      //
      boxCenterX_mm = trans.pix2IsoCoordX(edge.edgeSet.center_pix.getX),
      boxCenterY_mm = trans.pix2IsoCoordY(edge.edgeSet.center_pix.getY),
      //
      ballCenterX_mm = if (ball.center_mm.isDefined) ball.center_mm.get.getX else Double.NaN,
      ballCenterY_mm = if (ball.center_mm.isDefined) ball.center_mm.get.getY else Double.NaN,
      //
      X1Offset_mm = OffsetX1_mm,
      X2Offset_mm = OffsetX2_mm,
      Y1Offset_mm = OffsetY1_mm,
      Y2Offset_mm = OffsetY2_mm,
      //
      X1Type = plannedEdgeSet.map(p => p.x1.edgeType.toString),
      X2Type = plannedEdgeSet.map(p => p.x2.edgeType.toString),
      Y1Type = plannedEdgeSet.map(p => p.y1.edgeType.toString),
      Y2Type = plannedEdgeSet.map(p => p.y2.edgeType.toString),
      //
      X1PlannedOffset_mm = plannedEdgeSet.map(p => p.x1.position),
      X2PlannedOffset_mm = plannedEdgeSet.map(p => p.x2.position),
      Y1PlannedOffset_mm = plannedEdgeSet.map(p => p.y1.position),
      Y2PlannedOffset_mm = plannedEdgeSet.map(p => p.y2.position)
    )

    winLutz360
  }

  override def offsetX_mm: Double = if (ball.center_pix.isDefined) trans.pix2IsoDistX(edge.edgeSet.center_pix.getX - ball.center_pix.get.getX) else Double.NaN

  override def offsetY_mm: Double = if (ball.center_pix.isDefined) trans.pix2IsoDistY(edge.edgeSet.center_pix.getY - ball.center_pix.get.getY) else Double.NaN

  override def getImageStatus: WLImageStatus.Value = status

  override def convertToDB: Either[WinstonLutz, WinLutz360] = Right(makeWinLutz360)

  override def attrList: AttributeList = al

  private val htmlMaker = HTML(this, wlMessage)
  htmlMaker.generate()
}
