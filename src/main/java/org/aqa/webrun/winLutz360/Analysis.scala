package org.aqa.webrun.winLutz360

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.BiCubicImage
import org.aqa.db.Output
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.db.WinstonLutz
import org.aqa.db.WinLutz360
import org.aqa.webrun.wl.WLImageStatus
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLResult
import org.aqa.PlannedRectangle
import org.aqa.Util
import org.aqa.db.MachineWL
import org.aqa.webrun.wl.WLImageUtil

import java.awt.image.BufferedImage
import java.io.File
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

  // ImageDisplay.showInMSPaint(approxImg)
  // ImageDisplay.showInMSPaint(img)

  val validator: Validate = Validate(edge, ball, machineWL, wlMessage)

  val status: WLImageStatus.Value = validator.getStatus.get

  private val statusMessage: String = validator.getErrorMessage.get

  wlMessage.foreach(_.info(s"Status: $status : $statusMessage"))

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

    /**
      * Determine if the origin (center of beam) is defined. It is defined if either:
      *
      * - The RTPLAN is available.
      *
      * - It is assumed to be 0,0, and both pairs of opposing edges are nearly equidistant from 0,0.
      */
    val originIsDefined: Boolean = {
      val maxDistanceError_mm = 5.0
      def xDiff = (edge.x1DistanceToOrigin - edge.x2DistanceToOrigin).abs
      def yDiff = (edge.y1DistanceToOrigin - edge.y2DistanceToOrigin).abs
      def isClose = (xDiff < maxDistanceError_mm) || (yDiff < maxDistanceError_mm)

      wlRunReq.rtplan.isDefined || isClose
    }

    val winLutz360: WinLutz360 = WinLutz360(
      winLutz360PK = None,
      outputPK = extendedData.outputPK,
      rtimageUID = Util.sopOfAl(al),
      beamName = beamName,
      gantryAngle_deg = Util.gantryAngle(al),
      collimatorAngle_deg = Util.collimatorAngle(al),
      tableAngle_deg = Some(tableAngle_deg),
      //
      boxCenterX_mm = edge.edgeSet.center_pix.getX,
      boxCenterY_mm = edge.edgeSet.center_pix.getY,
      //
      ballCenterX_mm = ball.center_pix.getX,
      ballCenterY_mm = ball.center_pix.getY,
      //
      X1Offset_mm = if (originIsDefined) Some(edge.x1DistanceToOrigin) else None,
      X2Offset_mm = if (originIsDefined) Some(edge.x2DistanceToOrigin) else None,
      Y1Offset_mm = if (originIsDefined) Some(edge.y1DistanceToOrigin) else None,
      Y2Offset_mm = if (originIsDefined) Some(edge.y2DistanceToOrigin) else None,
      //
      X1Type = None, // TODO
      X2Type = None, // TODO
      Y1Type = None, // TODO
      Y2Type = None, // TODO
      //
      X1PlannedOffset_mm = None, // TODO
      X2PlannedOffset_mm = None, // TODO
      Y1PlannedOffset_mm = None, // TODO
      Y2PlannedOffset_mm = None // TODO
    )

    winLutz360
  }

  override def offsetX_mm: Double = trans.pix2IsoDistX(edge.edgeSet.center_pix.getX - ball.center_pix.getX)

  override def offsetY_mm: Double = trans.pix2IsoDistY(edge.edgeSet.center_pix.getY - ball.center_pix.getY)

  override def getImageStatus: WLImageStatus.Value = {
    val ok = validator.getStatus.isDefined && validator.getStatus.head.toString.equals(WLImageStatus.Passed.toString)
    if (ok)
      WLImageStatus.Passed
    else {
      if (validator.getStatus.isDefined)
        validator.getStatus.get
      else
        WLImageStatus.UnexpectedError
    }
  } // TODO be more specific

  override def convertToDB: Either[WinstonLutz, WinLutz360] = Right(makeWinLutz360)

  override def attrList: AttributeList = al

  private val htmlMaker = HTML(this, wlMessage)
  htmlMaker.generate()
}

//noinspection SpellCheckingInspection
object Analysis {

  // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""")
  val file = new File("""D:/tmp/wl/nonorth/WLNonCardNon45_20250625_Peyton/20250625_G180C30T0.dcm""")
  // val file = new File("""D:/tmp/wl/nonorth/ClinicalWinstonLutz_0.1_TB5_2025-12-12T06_34_56/RTIMAGE1.dcm""") // UM Production

  def main(args: Array[String]): Unit = {
    Config.validate
    val output = Output.get(9711).get
    Trace.trace("Starting test ----------------------------------------------------------------------------------")
    val ext = ExtendedData.get(output)
    val al = new DicomFile(file).attributeList.get
    val runReq = WLRunReq(Seq(al).toList, None)
    val wlMessage: WLMessage = WLMessage(runReq, al)
    val machineWL = MachineWL.getMachineWLOrDefault(ext.machine.machinePK.get)
    val wlNonCardAnalysis = Analysis(ext, al, runReq, machineWL, Some(wlMessage))
    Trace.trace("status: " + wlNonCardAnalysis.validator.getStatus)
    Trace.trace("error message: " + wlNonCardAnalysis.validator.getErrorMessage)

    CompositeImage.makeCompositeImage(wlNonCardAnalysis)

    Thread.sleep(5 * 1000)
    System.exit(99)
  }
}
