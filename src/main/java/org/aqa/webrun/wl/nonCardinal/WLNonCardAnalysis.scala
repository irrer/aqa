package org.aqa.webrun.wl.nonCardinal

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.wl.WLRunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.wl.WLPreprocessImage
import org.aqa.BiCubicImage
import org.aqa.db.Output
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.webrun.wl.WLMessage

import java.awt.image.BufferedImage
import java.io.File

case class WLNonCardAnalysis(extendedData: ExtendedData, al: AttributeList, wlRunReq: WLRunReq, wlMessage: Option[WLMessage]) {

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
