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
import org.aqa.Logging
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
case class RunReq(rtplan: AttributeList, rtimageMap: Map[String, AttributeList], flood: AttributeList,
                  symmetryAndFlatnessBaselineRedoBeamList: Seq[String], wedgeBaselineRedoBeamList: Seq[String]) extends RunReqClass with Logging {

  private val floodAttributeList = flood
  val floodOriginalImage = new DicomImage(floodAttributeList)

  private val floodBadPixelRadius = Util.badPixelRadius(flood)

  logger.info("Bad pixel radius (in pixels) for flood image of: " + floodBadPixelRadius)

  val floodBadPixelList: Seq[DicomImage.PixelRating] = Phase2Util.identifyBadPixels(floodOriginalImage, floodBadPixelRadius)

  val floodCorrectedImage: DicomImage = floodOriginalImage.correctBadPixels(floodBadPixelList, floodBadPixelRadius)

  val imageSize = new Point(floodOriginalImage.width, floodOriginalImage.height)

  val floodTranslator = new IsoImagePlaneTranslator(floodAttributeList)

  val treatmentMachineType: Option[DicomUtil.TreatmentMachineType.Value] = DicomUtil.TreatmentMachineType.attrListToTreatmentMachineType(rtplan)

  private val floodExpected_mm = MeasureTBLREdges.imageCollimatorPositions(floodAttributeList, rtplan).toTBLR(Util.collimatorAngle(flood))

  private val floodMeasurementAndImage = MeasureTBLREdges.measure(floodCorrectedImage, floodTranslator, Some(floodExpected_mm), Util.collimatorAngle(floodAttributeList), floodCorrectedImage, new Point(0, 0), Config.PenumbraThresholdPercent / 100)

  val floodMeasurement: MeasureTBLREdges.TBLR = floodMeasurementAndImage.measurementSet

  /**
   * Rectangle in pixels (not mm) within an image that flood field floods.  Image analysis for other images should
   * be done within the confines of this rectangle.
   */
  val floodRectangle: Rectangle = {

    val pnX = floodTranslator.iso2PixDistX(Config.PenumbraThickness_mm / 2)
    val pnY = floodTranslator.iso2PixDistY(Config.PenumbraThickness_mm / 2)

    val x = Math.round(floodMeasurement.left + pnX).toInt
    val y = Math.round(floodMeasurement.top + pnY).toInt
    val width = Math.round((floodMeasurement.right - floodMeasurement.left) - (pnX * 2)).toInt
    val height = Math.round((floodMeasurement.bottom - floodMeasurement.top) - (pnY * 2)).toInt

    new Rectangle(x, y, width, height)
  }

  val floodOffset: Point = floodRectangle.getLocation

  val floodPixelCorrectedAndCroppedImage: DicomImage = floodCorrectedImage.getSubimage(floodRectangle)

  lazy val floodImage: BufferedImage = floodMeasurementAndImage.bufferedImage

  case class Derived(al: AttributeList) {
    lazy val originalImage = new DicomImage(al)
    lazy val badPixels: Seq[DicomImage.PixelRating] = Phase2Util.identifyBadPixels(originalImage, Util.badPixelRadius(al))
    lazy val pixelCorrectedImage: DicomImage = originalImage.correctBadPixels(badPixels, Util.badPixelRadius(al))
    lazy val pixelCorrectedCroppedImage: DicomImage = pixelCorrectedImage.getSubimage(floodRectangle)
    lazy val biasAndPixelCorrectedCroppedImage: DicomImage = pixelCorrectedCroppedImage.biasCorrect(floodPixelCorrectedAndCroppedImage)
    val attributeList: AttributeList = al // convenience
  }

  val derivedMap: Map[String, Derived] = rtimageMap.keys.par.map(beamName => (beamName, Derived(rtimageMap(beamName)))).toList.toMap

  def beamNameOfAl(al: AttributeList): String = {
    val sop = Util.sopOfAl(al)

    val beamName = rtimageMap.keys.find(k => Util.sopOfAl(rtimageMap(k)).equals(sop))

    if (beamName.isDefined) beamName.get
    else {
      if (sop.equals(Util.sopOfAl(rtplan))) "RTPLAN"
      else if (sop.equals(Util.sopOfAl(flood))) Config.FloodFieldBeamName
      else "unknown"
    }

  }

  /** List of all attribute lists. */
  val attributeListSeq: Iterable[AttributeList] = derivedMap.values.map(_.attributeList) ++ Seq(flood, rtplan)

  val sopToPatientIdMap: Map[String, String] = attributeListSeq.map(al => (Util.sopOfAl(al), Util.patientIdOfAl(al))).toMap

  /** Given the SOPInstanceUID, return the patient ID. */
  def patientIdOfSop(sopUid: String): String = sopToPatientIdMap(sopUid)
}
