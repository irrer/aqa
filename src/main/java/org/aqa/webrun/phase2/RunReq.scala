/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Util
import org.aqa.run.RunReqClass

import java.awt.Point
import java.awt.Rectangle
import java.awt.image.BufferedImage

/**
  * @param rtplan                                  RTPLAN file
  * @param rtimageMap                              List of RTIMAGE files except for flood field
  * @param flood                                   Flood field file
  * @param symmetryAndFlatnessBaselineRedoBeamList List of beams that were explicitly marked as baselines when this data was processed previously
  * @param wedgeBaselineRedoBeamList               List of beams that were explicitly marked as baselines when this data was processed previously
  */
case class RunReq(
    rtplan: AttributeList,
    rtimageMap: Map[String, AttributeList],
    flood: Option[AttributeList],
    symmetryAndFlatnessBaselineRedoBeamList: Seq[String],
    wedgeBaselineRedoBeamList: Seq[String]
) extends RunReqClass {

  val floodOriginalImage: Option[DicomImage] = if (flood.isDefined) Some(new DicomImage(flood.get)) else None

  private val badPixelRadius: Int = Util.badPixelRadius(rtimageMap.values.head)

  logger.info("Bad pixel radius (in pixels) for flood image of: " + badPixelRadius)

  val floodBadPixelList: Seq[DicomImage.PixelRating] = if (floodOriginalImage.isDefined) Phase2Util.identifyBadPixels(floodOriginalImage.get, badPixelRadius) else Seq()

  private val floodCorrectedImage: Option[DicomImage] = if (floodOriginalImage.isDefined) Some(floodOriginalImage.get.correctBadPixels(floodBadPixelList, badPixelRadius)) else None

  /*
  val imageSize = {
    val img = new DicomImage(rtimageMap.values.head)
    new Point(img.width, floodOriginalImage.img)
  }
   */

  private val floodTranslator: Option[IsoImagePlaneTranslator] = if (flood.isDefined) Some(new IsoImagePlaneTranslator(flood.get)) else None

  val treatmentMachineType: Option[DicomUtil.TreatmentMachineType.Value] = DicomUtil.TreatmentMachineType.attrListToTreatmentMachineType(rtplan)

  private val floodExpected_mm = if (flood.isDefined) Some(MeasureTBLREdges.imageCollimatorPositions(flood.get, rtplan).toTBLR(Util.collimatorAngle(flood.get))) else None

  private val floodMeasurementAndImage = {
    // The beam name check is a hack to let Focal Spot use Phase3 RunReq or focal spot RunReq
    if (flood.isDefined) {
      MeasureTBLREdges.measure(
        floodCorrectedImage.get,
        floodTranslator.get,
        floodExpected_mm,
        Util.collimatorAngle(flood.get),
        floodCorrectedImage.get,
        new Point(0, 0),
        Config.PenumbraThresholdPercent / 100
      )
    } else {
      // Put in fake values.  These will never be used.
      val tblr = MeasureTBLREdges.TBLR(0, 0, 0, 0)
      val bufImg = new BufferedImage(100, 100, BufferedImage.TYPE_INT_RGB)
      MeasureTBLREdges.AnalysisResult(tblr, bufImg)
    }

  }

  private val floodMeasurement: MeasureTBLREdges.TBLR = floodMeasurementAndImage.measurementSet

  /**
    * Rectangle in pixels (not mm) within an image that flood field floods.  Image analysis for other images should
    * be done within the confines of this rectangle.
    */
  val floodRectangle: Option[Rectangle] = {

    if (flood.isDefined) {

      val pnX = floodTranslator.get.iso2PixDistX(Config.PenumbraThickness_mm / 2)
      val pnY = floodTranslator.get.iso2PixDistY(Config.PenumbraThickness_mm / 2)

      val x = Math.round(floodMeasurement.left + pnX).toInt
      val y = Math.round(floodMeasurement.top + pnY).toInt
      val width = Math.round((floodMeasurement.right - floodMeasurement.left) - (pnX * 2)).toInt
      val height = Math.round((floodMeasurement.bottom - floodMeasurement.top) - (pnY * 2)).toInt

      Some(new Rectangle(x, y, width, height))
    } else
      None
  }

  private val floodPixelCorrectedAndCroppedImage: Option[DicomImage] =
    if (floodCorrectedImage.isDefined && floodRectangle.isDefined) Some(floodCorrectedImage.get.getSubimage(floodRectangle.get)) else None

  case class Derived(al: AttributeList) {
    lazy val originalImage = new DicomImage(al)
    lazy val badPixels: Seq[DicomImage.PixelRating] = Phase2Util.identifyBadPixels(originalImage, Util.badPixelRadius(al))
    lazy val pixelCorrectedImage: DicomImage = originalImage.correctBadPixels(badPixels, Util.badPixelRadius(al))
    private lazy val pixelCorrectedCroppedImage: Option[DicomImage] = if (floodRectangle.isDefined) Some(pixelCorrectedImage.getSubimage(floodRectangle.get)) else None
    lazy val biasAndPixelCorrectedCroppedImage: Option[DicomImage] =
      if (floodPixelCorrectedAndCroppedImage.isDefined && floodPixelCorrectedAndCroppedImage.isDefined)
        Some(pixelCorrectedCroppedImage.get.biasCorrect(floodPixelCorrectedAndCroppedImage.get))
      else
        None
    val attributeList: AttributeList = al // convenience
  }

  val derivedMap: Map[String, Derived] = rtimageMap.keys.par.map(beamName => (beamName, Derived(rtimageMap(beamName)))).toList.toMap

  def beamNameOfAl(al: AttributeList): String = {
    val sop = Util.sopOfAl(al)

    val beamName = rtimageMap.keys.find(k => Util.sopOfAl(rtimageMap(k)).equals(sop))

    if (beamName.isDefined) beamName.get
    else {
      if (sop.equals(Util.sopOfAl(rtplan))) "RTPLAN"
      else if (flood.isDefined && sop.equals(Util.sopOfAl(flood.get))) Config.FloodFieldBeamName
      else "unknown"
    }

  }

  /** List of all attribute lists. */
  private val attributeListSeq: Iterable[AttributeList] = {
    val list = derivedMap.values.map(_.attributeList) ++ Seq(rtplan)
    if (flood.isDefined)
      list ++ Seq(flood.get)
    else
      list
  }

  private val sopToPatientIdMap: Map[String, String] = attributeListSeq.map(al => (Util.sopOfAl(al), Util.patientIdOfAl(al))).toMap

  /** Given the SOPInstanceUID, return the patient ID. */
  def patientIdOfSop(sopUid: String): String = sopToPatientIdMap(sopUid)
}
