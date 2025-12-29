package org.aqa.webrun.wl.nonCardinal

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
import org.aqa.db.WinstonLutzNonCardinal
import org.aqa.webrun.wl.WLImageStatus
import org.aqa.webrun.wl.WLMessage
import org.aqa.webrun.wl.WLResult
import org.aqa.PlannedRectangle
import org.aqa.Util
import org.aqa.db.MachineWL

import java.awt.image.BufferedImage
import java.io.File

case class WLNonCardAnalysis(extendedData: ExtendedData, al: AttributeList, wlRunReq: WLRunReq, machineWL: MachineWL, wlMessage: Option[WLMessage]) extends WLResult(extendedData, wlRunReq) {

  // Invert the pixels if necessary.
  private val preprocessedImage = WLPreprocessImage(al, None).preprocessedImage

  private val biCubicImage = BiCubicImage(preprocessedImage)
  val nonCardEdge = new WLNonCardEdgeAnalysis(preprocessedImage, al, biCubicImage, wlMessage)
  val nonCardBall = WLNonCardBall(nonCardEdge.edgeSet: WLNonCardEdgeSet, preprocessedImage: DicomImage, biCubicImage: BiCubicImage, al)

  val trans = new IsoImagePlaneTranslator(al)

  /** Take the mean of the pixels that are in the center of the ball and use those to establish the point in the brightest color level. */
  val maxPixelValue: Float = {
    val offsetList = -2 until 3
    val center_pix = nonCardEdge.edgeSet.center_pix
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

  val approxImg: BufferedImage = WLNonCardEdgeSetImage.makeImage(nonCardEdge.approximateEdgeSet, 3, al, border = 3, minPixelValue, maxPixelValue)
  val img: BufferedImage = WLNonCardEdgeSetImage.makeImage(nonCardEdge.edgeSet, 3, al, border = 3, minPixelValue, maxPixelValue)

  // ImageDisplay.showInMSPaint(approxImg)
  // ImageDisplay.showInMSPaint(img)

  val validator: WLNonCardValidate = WLNonCardValidate(nonCardEdge, nonCardBall, machineWL, wlMessage)

  val status: WLImageStatus.Value = validator.getStatus().get

  private val statusMessage: String = validator.getErrorMessage().get

  wlMessage.foreach(_.info(s"Status: $status : $statusMessage"))

  private def makeWinstonLutzNonCardinal: WinstonLutzNonCardinal = {
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

    val XOffset_mm: Double = nonCardEdge.edgeSet.X1.edgeLine.centerPoint.distance(nonCardEdge.edgeSet.X2.edgeLine.centerPoint) / 2

    val YOffset_mm: Double = nonCardEdge.edgeSet.Y1.edgeLine.centerPoint.distance(nonCardEdge.edgeSet.Y2.edgeLine.centerPoint) / 2

    val wlNonCard = WinstonLutzNonCardinal(
      // @formatter:off
      winstonLutzNonCardinalPK = None                                          ,
      outputPK                 = extendedData.outputPK                         ,
      rtimageUID               = Util.sopOfAl(al)                              ,
      beamName                 = beamName                                      ,
      gantryAngle_deg          = Util.gantryAngle(al)                          ,
      collimatorAngle_deg      = Util.collimatorAngle(al)                      ,
      tableAngle_deg           = tableAngle_deg                                ,
      //
      boxX_mm                  = nonCardEdge.edgeSet.center_pix.getX           ,
      boxY_mm                  = nonCardEdge.edgeSet.center_pix.getY           ,
      ballX_mm                 = nonCardBall.center_pix.getX                   ,
      ballY_mm                 = nonCardBall.center_pix.getY                   ,
      XOffset_mm               = XOffset_mm                                      ,
      YOffset_mm               = YOffset_mm                                      ,
      //
      plannedOffsetX1_mm       = plannedRectangle.map(_.x1)                    ,
      plannedOffsetX2_mm       = plannedRectangle.map(_.x2)                    ,
      plannedOffsetY1_mm       = plannedRectangle.map(_.y1)                    ,
      plannedOffsetY2_mm       = plannedRectangle.map(_.y2)                    ,
      //

      // @formatter:on
    )

    wlNonCard
  }

  override def offsetX_pix: Double = nonCardEdge.edgeSet.center_pix.getX - nonCardBall.center_pix.getX

  override def offsetY_pix: Double = nonCardEdge.edgeSet.center_pix.getY - nonCardBall.center_pix.getY

  override def getImageStatus: WLImageStatus.Value = {
    val ok = (validator.getStatus().size == 1) && validator.getStatus().head.toString.equals(WLImageStatus.Passed.toString)
    if (ok) WLImageStatus.Passed else WLImageStatus.OffsetLimitExceeded
  } // TODO be more specific

  override def convertToDB: Either[WinstonLutz, WinstonLutzNonCardinal] = Right(makeWinstonLutzNonCardinal)

  override def attrList: AttributeList = al

  private val htmlMaker = WLNonCardinalHTML(this, wlMessage)
  htmlMaker.generate()
}

//noinspection SpellCheckingInspection
object WLNonCardAnalysis {

  // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""")
  val file = new File("""D:/tmp/wl/nonorth/WLNonCardNon45_20250625_Peyton/20250625_G180C30T0.dcm""")
  // val file = new File("""D:/tmp/wl/nonorth/ClinicalWinstonLutz_0.1_TB5_2025-12-12T06_34_56/RTIMAGE1.dcm""") // UM Production

  def main(args: Array[String]): Unit = {
    Config.validate
    val output = Output.get(9711).get
    Trace.trace("Starting test ----------------------------------------------------------------------------------")
    val ext = ExtendedData.get(output)
    val al = new DicomFile(file).attributeList.get
    val runReq = WLRunReq(Seq(al), None)
    val wlMessage: WLMessage = WLMessage(runReq, al)
    val machineWL = MachineWL.getMachineWLOrDefault(ext.machine.machinePK.get)
    val wlNonCardAnalysis = WLNonCardAnalysis(ext, al, runReq, machineWL, Some(wlMessage))
    Trace.trace("status: " + wlNonCardAnalysis.validator.getStatus())
    Trace.trace("error message: " + wlNonCardAnalysis.validator.getErrorMessage())

    WLNonCardCompositeImage.makeCompositeImage(wlNonCardAnalysis)

    Thread.sleep(5 * 1000)
    System.exit(99)
  }
}
