package org.aqa.webrun.phase2

import org.aqa.DicomFile
import java.io.File
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import java.awt.Point
import java.awt.Rectangle
import org.aqa.db.Machine

/**
 * @param rtplan: RTPLAN file
 *
 * @param rtimageMap: List of RTIMAGE files except for flood field
 *
 * @param flood: Flood field file
 */
case class RunReq(rtplan: DicomFile, machine: Machine, rtimageMap: Map[String, DicomFile], flood: DicomFile) {

  def reDir(dir: File): RunReq = {
    val rtiMap = rtimageMap.toSeq.map(ni => (ni._1, ni._2.reDir(dir))).toMap
    val rtplanRedirred = if (rtplan.file.getParentFile.getAbsolutePath.equals(Config.sharedDir.getAbsolutePath)) rtplan else rtplan.reDir(dir)
    new RunReq(rtplanRedirred, machine, rtiMap, flood.reDir(dir))
  }

  val floodOriginalImage = new DicomImage(flood.attributeList.get)

  val floodCorrectedImage = Phase2Util.correctBadPixels(floodOriginalImage)

  val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(flood.attributeList.get)

  private val floodMeasurementAndImage = MeasureNSEWEdges.measure(floodCorrectedImage, ImagePlanePixelSpacing, floodCorrectedImage, new Point(0, 0))

  val floodMeasurement = floodMeasurementAndImage.measurementSet

  /**
   * Rectangle within an image that flood field floods.  Image analysis for other images should
   * be done within the confines of this rectangle.
   */
  val floodRectangle = {
    val ImagePlanePixelSpacing = Phase2Util.getImagePlanePixelSpacing(flood.attributeList.get)
    val pnX = (Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getX) / 2
    val pnY = (Config.PenumbraThickness_mm / ImagePlanePixelSpacing.getY) / 2

    val x = Math.round(floodMeasurement.west + pnX).toInt
    val y = Math.round(floodMeasurement.north + pnY).toInt
    val width = Math.round((floodMeasurement.east - floodMeasurement.west) - (pnX * 2)).toInt
    val height = Math.round((floodMeasurement.south - floodMeasurement.north) - (pnY * 2)).toInt

    new Rectangle(x, y, width, height)
  }

  val floodOffset = floodRectangle.getLocation

  val floodPixelCorrectedAndCroppedImage = floodCorrectedImage.getSubimage(floodRectangle)

  lazy val floodImage = floodMeasurementAndImage.bufferedImage

  class Derived(dicomFile: DicomFile) {
    lazy val originalImage = new DicomImage(dicomFile.attributeList.get)
    lazy val pixelCorrectedImage = Phase2Util.correctBadPixels(originalImage)
    lazy val pixelCorrectedCroppedImage = pixelCorrectedImage.getSubimage(floodRectangle)
    lazy val biasAndPixelCorrectedCroppedImage = pixelCorrectedCroppedImage.biasCorrect(floodPixelCorrectedAndCroppedImage)
  }

  val derivedMap = rtimageMap.keys.map(beamName => (beamName, new Derived(rtimageMap(beamName)))).toMap
  

}
