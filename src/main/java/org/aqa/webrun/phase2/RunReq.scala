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
import org.aqa.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil

/**
 * @param rtplan: RTPLAN file
 *
 * @param rtimageMap: List of RTIMAGE files except for flood field
 *
 * @param flood: Flood field file
 */
case class RunReq(rtplan: DicomFile, rtplanCBCT: Option[DicomFile], machine: Machine, rtimageMap: Map[String, DicomFile], flood: DicomFile) extends Logging {

  def reDir(dir: File): RunReq = {
    val rtiMap = rtimageMap.toSeq.map(ni => (ni._1, ni._2.reDir(dir))).toMap
    val rtplanRedirred = if (rtplan.file.getParentFile.getAbsolutePath.equals(Config.sharedDir.getAbsolutePath)) rtplan else rtplan.reDir(dir)
    val rtplanCBCTRedirred = {
      if (rtplanCBCT.isDefined)
        if (rtplanCBCT.get.file.getParentFile.getAbsolutePath.equals(Config.sharedDir.getAbsolutePath)) rtplanCBCT else Some(rtplanCBCT.get.reDir(dir))
      else None
    }
    new RunReq(rtplanRedirred, rtplanCBCTRedirred, machine, rtiMap, flood.reDir(dir))
  }

  private val floodAttributeList = flood.attributeList.get
  val floodOriginalImage = new DicomImage(floodAttributeList)

  logger.info("Bad pixel radius (in pixels) for flood image of: " + flood.badPixelRadius)

  val floodBadPixelList = Phase2Util.identifyBadPixels(floodOriginalImage, flood.badPixelRadius)

  val floodCorrectedImage = floodOriginalImage.correctBadPixels(floodBadPixelList, flood.badPixelRadius)

  val imageSize = new Point(floodOriginalImage.width, floodOriginalImage.height)

  val floodTranslator = new IsoImagePlaneTranslator(floodAttributeList)
  
  val treatmentMachineType = DicomUtil.TreatmentMachineType.attrListToTreatmentMachineType(rtplan.attributeList.get)

  private val floodExpected_mm = MeasureTBLREdges.imageCollimatorPositions(floodAttributeList, rtplan.attributeList.get).toTBLR(Util.collimatorAngle(flood.attributeList.get))

  private val floodMeasurementAndImage = MeasureTBLREdges.measure(floodCorrectedImage, floodTranslator, Some(floodExpected_mm), Util.collimatorAngle(floodAttributeList), floodCorrectedImage, new Point(0, 0), Config.PenumbraThresholdPercent / 100)

  val floodMeasurement = floodMeasurementAndImage.measurementSet

  /**
   * Rectangle in pixels (not mm) within an image that flood field floods.  Image analysis for other images should
   * be done within the confines of this rectangle.
   */
  val floodRectangle = {

    val pnX = floodTranslator.iso2PixDistX(Config.PenumbraThickness_mm / 2)
    val pnY = floodTranslator.iso2PixDistY(Config.PenumbraThickness_mm / 2)

    val x = Math.round(floodMeasurement.left + pnX).toInt
    val y = Math.round(floodMeasurement.top + pnY).toInt
    val width = Math.round((floodMeasurement.right - floodMeasurement.left) - (pnX * 2)).toInt
    val height = Math.round((floodMeasurement.bottom - floodMeasurement.top) - (pnY * 2)).toInt

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
    val attributeList = dicomFile.attributeList.get // convenience
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

  /** List of all attribute lists. */
  val attributeListSeq = derivedMap.values.map(_.attributeList) ++ Seq(flood.attributeList.get, rtplan.attributeList.get)

  val sopToPatientIdMap = attributeListSeq.map(al => (Util.sopOfAl(al), Util.patientIdOfAl(al))).toMap

  /** Given the SOPInstanceUID, return the patient ID. */
  def patientIdOfSop(sopUid: String) = sopToPatientIdMap(sopUid)
}
