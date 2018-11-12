package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.Point
import java.awt.Rectangle
import org.aqa.db.Machine
import java.awt.geom.Point2D
import edu.umro.ScalaUtil.Trace
import org.aqa.Util
import com.pixelmed.dicom.AttributeList
import org.aqa.Logging

/**
 * @param rtplan: RTPLAN file
 *
 * @param rtimageMap: List of RTIMAGE files except for flood field
 *
 * @param flood: Flood field file
 */
case class RunReq(rtplan: DicomFile, machine: Machine, rtimageMap: Map[String, DicomFile], flood: DicomFile) extends Logging {

  def reDir(dir: File): RunReq = {
    val rtiMap = rtimageMap.toSeq.map(ni => (ni._1, ni._2.reDir(dir))).toMap
    val rtplanRedirred = if (rtplan.file.getParentFile.getAbsolutePath.equals(Config.sharedDir.getAbsolutePath)) rtplan else rtplan.reDir(dir)
    new RunReq(rtplanRedirred, machine, rtiMap, flood.reDir(dir))
  }

  val floodOriginalImage = new DicomImage(flood.attributeList.get)

  logger.info("Bad pixel radius (in pixels) for flood image of: " + flood.badPixelRadius)

  val floodBadPixelList = Phase2Util.identifyBadPixels(floodOriginalImage, flood.badPixelRadius)

  val floodCorrectedImage = floodOriginalImage.correctBadPixels(floodBadPixelList, flood.badPixelRadius)

  if (false) { // TODO rm
    println("=================================")
    val lo = 50
    val hi = 4000
    val worst = for (
      x <- (0 until floodOriginalImage.width);
      y <- (0 until floodOriginalImage.height);
      if ((floodOriginalImage.get(x, y) < lo) || (floodOriginalImage.get(x, y) > hi))
    ) yield {
      (x, y)
    }

    worst.map(xy => println("bad " + xy._1.formatted("%4d") + ", " + xy._2.formatted("%4d") + " : " + floodOriginalImage.get(xy._1, xy._2)))
    println("=================================")
    def showHist(name: String, di: DicomImage) = {
      val hist = di.histogram
      def toStr(h: Seq[DicomImage.HistPoint]) = {
        h.map(vc => { vc.value + ", " + vc.count }).mkString("\n")
      }
      println(name + ": ==========\n" + toStr(hist.take(10)) + "\n--------\n" + toStr(hist.takeRight(10)) + "\n=======")
    }

    def badPixToString(di: DicomImage, list: Seq[DicomImage.PixelRating]) = {
      "size: " + list.size + " :: " + list.map(p => di.get(p.x, p.y).toInt).sorted.mkString("   ")
    }

    val fc2bad = Phase2Util.identifyBadPixels(floodCorrectedImage, flood.badPixelRadius)
    val fc2 = floodCorrectedImage.correctBadPixels(fc2bad, flood.badPixelRadius)
    showHist("floodOriginalImage", floodOriginalImage)
    showHist("floodCorrectedImage", floodCorrectedImage)
    showHist("fc2", fc2)

    println("floodBadPixelList before: " + badPixToString(floodOriginalImage, floodBadPixelList))
    println("floodBadPixelList after: " + badPixToString(floodCorrectedImage, floodBadPixelList))
    println("fc2bad before: " + badPixToString(floodCorrectedImage, fc2bad))
    println("fc2bad after: " + badPixToString(fc2, fc2bad))
    println("hey")
  }
  val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(flood.attributeList.get)

  val imageSize = new Point(floodOriginalImage.width, floodOriginalImage.height)

  private val floodMeasurementAndImage = MeasureTBLREdges.measure(floodCorrectedImage, ImagePlanePixelSpacing, Util.collimatorAngle(flood.attributeList.get), floodCorrectedImage, new Point(0, 0))

  val floodMeasurement = floodMeasurementAndImage.measurementSet

  /**
   * Rectangle in pixels (not mm) within an image that flood field floods.  Image analysis for other images should
   * be done within the confines of this rectangle.
   */
  val floodRectangle = {
    val pnX = (Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getX) / 2
    val pnY = (Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getY) / 2

    val fPix = floodMeasurement.translate(new Point(0, 0), new Point2D.Double(1 / ImagePlanePixelSpacing.getX, 1 / ImagePlanePixelSpacing.getY))

    val x = Math.round(fPix.left + pnX).toInt
    val y = Math.round(fPix.top + pnY).toInt
    val width = Math.round((fPix.right - fPix.left) - (pnX * 2)).toInt
    val height = Math.round((fPix.bottom - fPix.top) - (pnY * 2)).toInt

    new Rectangle(x, y, width, height)
  }

  val floodOffset = floodRectangle.getLocation

  val floodPixelCorrectedAndCroppedImage = floodCorrectedImage.getSubimage(floodRectangle)

  lazy val floodImage = floodMeasurementAndImage.bufferedImage

  case class Derived(dicomFile: DicomFile) {
    lazy val originalImage = new DicomImage(dicomFile.attributeList.get)
    lazy val badPixels = Phase2Util.identifyBadPixels(originalImage, dicomFile.badPixelRadius)
    lazy val pixelCorrectedImage = originalImage.correctBadPixels(badPixels, dicomFile.badPixelRadius)
    lazy val pixelCorrectedCroppedImage = pixelCorrectedImage.getSubimage(floodRectangle)
    lazy val biasAndPixelCorrectedCroppedImage = pixelCorrectedCroppedImage.biasCorrect(floodPixelCorrectedAndCroppedImage)
  }

  val derivedMap = rtimageMap.keys.par.map(beamName => (beamName, new Derived(rtimageMap(beamName)))).toList.toMap

  def beamNameOfAl(al: AttributeList): String = {
    val sop = Util.sopOfAl(al)

    val beamName = rtimageMap.keys.find(k => Util.sopOfAl(rtimageMap(k).attributeList.get).equals(sop))

    if (beamName.isDefined) beamName.get
    else {
      if (sop.equals(Util.sopOfAl(rtplan.attributeList.get))) "RTPLAN"
      else if (sop.equals(Util.sopOfAl(flood.attributeList.get))) Config.FloodFieldBeamName
      else "unknown"
    }

  }
}
