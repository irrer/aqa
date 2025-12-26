package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
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
import org.aqa.Util
import org.checkerframework.checker.units.qual.s

import java.awt.image.BufferedImage
import java.io.File

case class WLNonCardAnalysis(extendedData: ExtendedData, al: AttributeList, wlRunReq: WLRunReq, wlMessage: Option[WLMessage]) extends WLResult {

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

  // ImageDisplay.showInMSPaint(approxImg) // TODO rm
  // ImageDisplay.showInMSPaint(img) // TODO rm

  val validator: WLNonCardValidate = WLNonCardValidate(nonCardEdge, nonCardBall, wlMessage)

  Trace.trace(s"""validator.errorList:size: ${validator.errorList.size}\n ${validator.errorList.mkString("\n")}""")

  private def makeWinstonLutzNonCardinal: WinstonLutzNonCardinal = {
    val beamName: Option[String] = {
      if (wlRunReq.rtplan.isDefined)
        Util.getBeamNameOfRtimage(wlRunReq.rtplan.get, al)
      else
        None
    }

    case class MLC(mlcType: String, leafList: Seq[Double], boundaries: Seq[Double]) {

      private val leafCount = leafList.size

      private val pairCount = leafCount / 2

      private case class LeafPair(l1: Double, l2: Double) {
        val gap: Double = l2 - l1
      }

      private val pairList: Seq[LeafPair] = leafList.take(pairCount).zip(leafList.takeRight(pairCount)).map(pair => LeafPair(pair._1, pair._2))

      private val gapList = pairList.map(_.gap)


      val gap: Double = gapList.max

      private val gappingPair = pairList.find(_.gap == gap).get

      private val first: Int = gapList.indexWhere(_ == gap)
      private val last: Int = gapList.lastIndexWhere(_ == gap)

      val top: Double = boundaries(first)
      val bottom: Double = boundaries(last + 1)
      val left: Double = gappingPair.l1
      val right: Double = gappingPair.l1

    }

    val tableAngle_deg: Double = DicomUtil.findAllSingle(al, TagByName.PatientSupportAngle).head.getDoubleValues.head

    val mlcList = if (wlRunReq.rtplan.isDefined) {

      val beam = Util.getBeamOfRtimage(wlRunReq.rtplan.get, al).get


      def makeMLC(attr: Attribute): Option[MLC] = {
        try {
          val seqAttr = attr.asInstanceOf[SequenceAttribute]
          val attrList = DicomUtil.alOfSeq(seqAttr).head
          val leafPairCount = attrList.get(TagByName.NumberOfLeafJawPairs).getIntegerValues.head
          val mlcType = DicomUtil.findAllSingle(attrList, TagByName.RTBeamLimitingDeviceType).head.getSingleStringValueOrEmptyString
          val leafBoundaryList = DicomUtil.findAllSingle(attrList, TagByName.RTBeamLimitingDeviceType).head.getDoubleValues

          if ((leafPairCount > 1) && (mlcType.startsWith("MLC")))
            Some(MLC(mlcType, leafPairCount, leafBoundaryList))
          else
            None
        }
        catch {
          case _: Throwable => None
        }
      }

      val mlcList = DicomUtil.findAllSingle(beam, TagByName.RTBeamLimitingDeviceType).flatMap(makeMLC)

      mlcList
    }
    else
      None

    val wlNonCard = WinstonLutzNonCardinal(
      // @formatter:off
      winstonLutz2PK       = Option[Long]              ,
      outputPK             = extendedData.outputPK               ,
      rtimageUID           = Util.sopOfAl(al)             ,
      beamName             = beamName                 ,
      gantryAngle_deg      = Util.gantryAngle(al)            ,
      collimatorAngle_deg  = Util.collimatorAngle(al)            ,
      tableAngle_deg       = tableAngle_deg              ,
      //
      X1x_mm               = Some(nonCardEdge.edgeSet.X1.edgeLine.centerX)    ,
      X1y_mm               = Some(nonCardEdge.edgeSet.X1.edgeLine.centerY)    ,
      X2x_mm               = Some(nonCardEdge.edgeSet.X2.edgeLine.centerX)    ,
      X2y_mm               = Some(nonCardEdge.edgeSet.X2.edgeLine.centerY)    ,
      Y1x_mm               = Some(nonCardEdge.edgeSet.Y1.edgeLine.centerX)    ,
      Y1y_mm               = Some(nonCardEdge.edgeSet.Y1.edgeLine.centerY)    ,
      Y2x_mm               = Some(nonCardEdge.edgeSet.Y2.edgeLine.centerX)   ,
      Y2y_mm               = Some(nonCardEdge.edgeSet.Y2.edgeLine.centerY)    ,
      //
      plannedOffsetX1_mm   = Option[Double]     ,
      plannedOffsetX2_mm   = Option[Double]     ,
      plannedOffsetY1_mm   = Option[Double]     ,
      plannedOffsetY2_mm   = Option[Double]     ,
      //
      ballX_mm             = Double             ,
      ballY_mm             = Double               // Y coordinate of center of ball in mm

      // @formatter:on
    )
    ???
  }

  override def offsetX_pix: Double = nonCardEdge.edgeSet.center_pix.getX - nonCardBall.center_pix.getX

  override def offsetY_pix: Double = nonCardEdge.edgeSet.center_pix.getY - nonCardBall.center_pix.getY

  override def getImageStatus: WLImageStatus.Value = if (validator.errorList.isEmpty) WLImageStatus.Passed else WLImageStatus.OffsetLimitExceeded // TODO be more specific

  override def convertToDB: Either[WinstonLutz, WinstonLutzNonCardinal] = Right(makeWinstonLutzNonCardinal)

  override def attrList: AttributeList = al
}

object WLNonCardAnalysis {

  // val file = new File("""D:/tmp/wl/nonorth/1/0005.dcm""")
  val file = new File("""D:/tmp/wl/nonorth/WLNonCardNon45_20250625_Peyton/20250625_G180C30T0.dcm""")
  // val file = new File("""D:/tmp/wl/nonorth/ClinicalWinstonLutz_0.1_TB5_2025-12-12T06_34_56/RTIMAGE1.dcm""") // UM Production

  def main(args: Array[String]): Unit = {
    Config.validate
    val output = Output.get(9685).get
    Trace.trace("Starting test ----------------------------------------------------------------------------------")
    val ext = ExtendedData.get(output)
    val al = new DicomFile(file).attributeList.get
    val runReq = WLRunReq(Seq(al), None)

    val wlMessage: WLMessage = WLMessage(runReq, al)

    val wlNonCardAnalysis = WLNonCardAnalysis(ext, al, runReq, Some(wlMessage))
    Trace.trace("List of errors: " + wlNonCardAnalysis.validator.errorList.mkString("\n"))

    WLNonCardCompositeImage.makeCompositeImage(wlNonCardAnalysis)

    Thread.sleep(5 * 1000)
    System.exit(99)
  }
}
