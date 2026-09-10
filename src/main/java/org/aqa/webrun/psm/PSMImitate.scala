package org.aqa.webrun.psm

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.DicomImageDouble
import org.aqa.Util

import java.awt.Color
import javax.vecmath.Point2d
import javax.vecmath.Point2i

case class PSMImitate(ff: AttributeList, response: Seq[AttributeList], grid: PSMGrid) {

  private val prefix = "Imitate"

  private val radius = 3

  private def fmt(d: Double): String = "%24.16f".format(d)

  private val ffImgRaw = new DicomImageDouble(ff)

  private val makeFFImage: DicomImageDouble = {
    val mean: Double = {
      val sum = ffImgRaw.pixelData.flatten.map(_.toDouble).sum
      val size = ffImgRaw.Rows * ffImgRaw.Columns
      val m = sum / size
      m
    }
    println(s"$prefix FF mean: ${fmt(mean)}")

    def norm(v: Double) = v / mean

    ffImgRaw.fun1(norm)
  }

  private val ffImg: DicomImageDouble = makeFFImage

  private val responseImgRaw = response.map(al => new DicomImageDouble(al))

  println(s"$prefix FF Raw  first 10: \n    " + ffImgRaw.pixelData.head.take(10).map(p => fmt(p)).mkString("\n    "))
  println(s"$prefix FF Norm first 10: \n    " + ffImg.pixelData.head.take(10).map(p => fmt(p)).mkString("\n    "))

  def scaleImg(al: AttributeList): DicomImageDouble = {
    val imgRaw = new DicomImageDouble(al)

    val slope = al.get(TagByName.RescaleSlope).getDoubleValues.head.toFloat
    val intercept = al.get(TagByName.RescaleIntercept).getDoubleValues.head.toFloat

    def fun1(v: Double): Double = (v * slope) + intercept

    imgRaw.fun1(fun1)
  }

  private val responseImg = response.map(al => new DicomImageDouble(al))

  private def meanPix(point: Point2i, img: DicomImageDouble): Double = {

    val range = -radius to radius
    val pixList = range.flatMap(yy => range.map(xx => img.get(xx + point.getX - 1, yy + point.y - 1)))
    val mean = pixList.sum / pixList.size
    mean
  }

  if (true) {
    val j = new java.math.BigDecimal("0.7")
    j.divide(j)
  }

  // @formatter:off
  val    topPoint = new Point2i(596, 298)
  val bottomPoint = new Point2i(596, 894)
  val   leftPoint = new Point2i(298, 596)
  val  rightPoint = new Point2i(894, 596)
  val centerPoint = new Point2i(596, 596)
  // @formatter:on

  def showFF(name: String, point: Point2i): Double = {
    val v = meanPix(point, ffImg)
    val vText = fmt(v)
    val nameText = "%-7s".format(name)
    println(s"$prefix FF $nameText FFcorB: $vText")
    v
  }

  // @formatter:off
  showFF("top"   ,    topPoint)
  showFF("bottom", bottomPoint)
  showFF("left"  ,   leftPoint)
  showFF("right" ,  rightPoint)
  showFF("center", centerPoint)
  // @formatter:on

  private def showResponse(index: Int): Unit = {
    val rsp = response(index)
    val irt = rsp.get(TagByName.XRayImageReceptorTranslation).getDoubleValues
    val x = irt(0)
    val y = irt(1)

    println(s"$prefix  index: $index    x: $x    y: $y")
  }

  response.indices.foreach(showResponse)

  def findResponse(name: String, point: Point2i): AttributeList = {
    val r = response.minBy(rsp => {
      val trans = new IsoImagePlaneTranslator(rsp)
      val irt = rsp.get(TagByName.XRayImageReceptorTranslation).getDoubleValues
      val tx = trans.iso2PixCoordX(-irt.head)
      val ty = trans.iso2PixCoordY(-irt(1))
      val pd = new Point2d(point.getX, point.getY)
      val p = new Point2d(tx, ty)
      pd.distance(p)
    })

    val di = {
      val slope = r.get(TagByName.RescaleSlope).getDoubleValues.head
      val intercept = r.get(TagByName.RescaleIntercept).getDoubleValues.head

      def fun1(pixelValue: Double): Double = (pixelValue * slope) + intercept

      val d1 = new DicomImageDouble(r)
      val d2 = d1.fun1(fun1)
      d2
    }


    val trans = new IsoImagePlaneTranslator(r)
    val irt = r.get(TagByName.XRayImageReceptorTranslation).getDoubleValues
    val x_iso = irt.head
    val y_iso = irt(1)
    val tx = trans.iso2PixCoordX(-x_iso)
    val ty = trans.iso2PixCoordY(y_iso)

    val meanResponse = meanPix(point, di)
    val meanFF = meanPix(point, ffImg)

    val n = "%-8s".format(name)
    println(s"$prefix findResponse $n  : ${Util.sopOfAl(r)}    x_iso: ${fmt(x_iso)}    y_iso: ${fmt(y_iso)}    tx: ${fmt(tx)}    ty: ${fmt(ty)}    meanResponse: ${fmt(meanResponse)}    meanFF: ${fmt(meanFF)}    meanFF*meanResponse: ${fmt(meanFF * meanResponse)}")
    r
  }


  // @formatter:off
  val    topBeam: PSMBeamAnalysisResult = grid.resultList.minBy(r => r.psmBeam.center.getY)
  val bottomBeam: PSMBeamAnalysisResult = grid.resultList.maxBy(r => r.psmBeam.center.getY)
  val   leftBeam: PSMBeamAnalysisResult = grid.resultList.minBy(r => r.psmBeam.center.getX)
  val  rightBeam: PSMBeamAnalysisResult = grid.resultList.maxBy(r => r.psmBeam.center.getX)
  val centerBeam: PSMBeamAnalysisResult = grid.resultList.find(r => r.psmBeam.SOPInstanceUID.equals(grid.centerBeam.SOPInstanceUID)).get

  val centerCROI = meanPix(centerPoint, scaleImg(centerBeam.rtimage))
  val    topCROI = meanPix(   topPoint, scaleImg(   topBeam.rtimage)) / centerCROI
  val bottomCROI = meanPix(bottomPoint, scaleImg(bottomBeam.rtimage)) / centerCROI
  val   leftCROI = meanPix ( leftPoint, scaleImg(  leftBeam.rtimage)) / centerCROI
  val  rightCROI = meanPix( rightPoint, scaleImg( rightBeam.rtimage)) / centerCROI

  println(s"$prefix    centerCROI: ${fmt(centerCROI)}")
  println(s"$prefix       topCROI: ${fmt(   topCROI)}")
  println(s"$prefix    bottomCROI: ${fmt(bottomCROI)}")
  println(s"$prefix      leftCROI: ${fmt(  leftCROI)}")
  println(s"$prefix     rightCROI: ${fmt( rightCROI)}")

  // @formatter:on

  val AQACor: Double = {
    val di = scaleImg(centerBeam.rtimage)
    val pixelValueList = centerBeam.pixelList.keys.map(pix => di.get(pix.getX, pix.getY))
    val corA = pixelValueList.sum / pixelValueList.size

    val ffValueList = centerBeam.pixelList.keys.map(pix => ffImg.get(pix.getX, pix.getY))
    val corB = ffValueList.sum / pixelValueList.size
    val cor = corA * corB
    println(s"$prefix AQACor: ${fmt(cor)}")
    cor
  }
  //val centerFFCor = centerBeamCenterCU__FFcorA * meanPix(centerPoint, ffImg)


  def showImage(name: String, beam: PSMBeamAnalysisResult, point: Point2i, centerBeamCenterCU__FFcorA: Option[Double], centerFFCor: Option[Double]): Double = {

    val nam = "%-6s".format(name)
    val rtimage = beam.rtimage
    val di = scaleImg(beam.rtimage)
    val bufImg = di.toDeepColorBufferedImage(0.001)
    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.black)


    val slope = rtimage.get(TagByName.RescaleSlope).getDoubleValues.head.toDouble
    val intercept = rtimage.get(TagByName.RescaleIntercept).getDoubleValues.head.toDouble

    val matlab_cu = (meanPix(point, new DicomImageDouble(rtimage)) * slope) + intercept

    val AQACorA: Double = {
      val pixelValueList = beam.pixelList.keys.map(pix => di.get(pix.getX, pix.getY))
      pixelValueList.sum / pixelValueList.size
    }

    def doText(y: Int, text: String): Unit = {
      println(s"$prefix doText $nam: $text")
      ImageText.drawTextCenteredAt(gc, bufImg.getWidth / 2, y, text)
    }

    val FFcorAText: String = {
      if (centerBeamCenterCU__FFcorA.isDefined) {
        val centerBeamCenterCU__FFcorAa = centerBeamCenterCU__FFcorA.get / matlab_cu
        " centerBeamCenterCU__FFcorAa: " + fmt(centerBeamCenterCU__FFcorAa)
      }
      else " NA"
    }
    val FFcor_ = matlab_cu * meanPix(point, ffImg)
    val PSM = if (centerFFCor.isDefined) FFcor_ / centerFFCor.get else 0

    val AQAFFCorB: Double = {
      val pixelValueList = beam.pixelList.keys.map(pix => ffImg.get(pix.getX, pix.getY))
      pixelValueList.sum / pixelValueList.size
    }

    val AQAFFcor = AQACorA * AQAFFCorB

    doText(20, s"$nam x: ${beam.psmBeam.center.getX}    y: ${beam.psmBeam.center.getY}")
    doText(40, s"pointX: ${point.getX}    pointY: ${point.getY}")
    doText(60, s"Matlab FFcorA: ${fmt(matlab_cu)} $FFcorAText")
    doText(80, s"   AQA FFcorA: ${fmt(beam.psmBeam.mean_cu)}")
    doText(100, s"Matlab FFcorB: ${fmt(meanPix(point, ffImg))}")
    doText(120, s"   AQA FFcorB: ${fmt(AQAFFCorB)}")
    doText(140, s"Matlab FFcor_: ${fmt(FFcor_)}")
    doText(160, s"   AQA FFcor : ${fmt(AQAFFcor)}")
    doText(180, s"Matlab PSM: ${fmt(PSM)}")

    val s = (radius * 2) + 1
    gc.drawRect(point.getX - radius, point.getY - radius, s, s)

    // Trace.showInMSPaint(bufImg)

    matlab_cu
  }

  // @formatter:off
  val centerBeamCenterCU__FFcorA = showImage( "center", centerBeam, centerPoint, None, None)
  val centerFFCor = centerBeamCenterCU__FFcorA * meanPix(centerPoint, ffImg)
  showImage                                 (    "top",    topBeam,    topPoint, Some(centerBeamCenterCU__FFcorA), Some(centerFFCor))
  showImage                                 ( "bottom", bottomBeam, bottomPoint, Some(centerBeamCenterCU__FFcorA), Some(centerFFCor))
  showImage                                 (   "left",   leftBeam,   leftPoint, Some(centerBeamCenterCU__FFcorA), Some(centerFFCor))
  showImage                                 (  "right",  rightBeam,  rightPoint, Some(centerBeamCenterCU__FFcorA), Some(centerFFCor))

  // @formatter:on

  println(s"$prefix Done")

}


