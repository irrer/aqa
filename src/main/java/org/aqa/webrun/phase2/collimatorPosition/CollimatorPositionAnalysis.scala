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

package org.aqa.webrun.phase2.collimatorPosition

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.Point
import scala.xml.Elem

/**
  * Analyze DICOM files for ImageAnalysis.
  */
object CollimatorPositionAnalysis extends Logging {

  /**
    * Measure the four collimator edges.  Function is public to make it testable.
    */
  private def measureImage(
      beamName: String,
      FloodCompensation: Boolean,
      biasAndPixelCorrectedCroppedImage: DicomImage,
      pixelCorrectedImage: DicomImage,
      rtimage: AttributeList,
      originalImage: DicomImage,
      outputPK: Long,
      floodOffset: Point,
      collimatorCenter: Point2D.Double,
      rtplan: AttributeList
  ): Either[String, (CollimatorPosition, BufferedImage)] = {
    try {
      val collimatorAngle = Util.collimatorAngle(rtimage)
      val gantryAngle = Util.gantryAngle(rtimage)

      def dbl(tag: AttributeTag): Double = rtimage.get(tag).getDoubleValues.head

      val expected_mm = MeasureTBLREdges.imageCollimatorPositions(rtimage, rtplan).toTBLR(collimatorAngle)

      val translator = new IsoImagePlaneTranslator(rtimage)
      val edges = {
        if (FloodCompensation) {
          val exp_mm = new MeasureTBLREdges.TBLR(
            expected_mm.top - translator.pix2IsoDistY(floodOffset.getY),
            expected_mm.bottom - translator.pix2IsoDistY(floodOffset.getY),
            expected_mm.left - translator.pix2IsoDistX(floodOffset.getX),
            expected_mm.right - translator.pix2IsoDistX(floodOffset.getX)
          )
          MeasureTBLREdges.measure(biasAndPixelCorrectedCroppedImage, translator, Some(exp_mm), collimatorAngle, originalImage, floodOffset, Config.PenumbraThresholdPercent / 100)
        } else
          MeasureTBLREdges.measure(pixelCorrectedImage, translator, Some(expected_mm), collimatorAngle, originalImage, new Point(0, 0), Config.PenumbraThresholdPercent / 100)
      }

      // expected edge values compensated with collimator centering.
      val expectedEdgesTBLR = MeasureTBLREdges.imageCollimatorPositions(rtimage, rtplan).toTBLR(collimatorAngle).addOffset(collimatorCenter)
      val floodOff = if (FloodCompensation) floodOffset else new Point(0, 0)
      val measuredTBLR = edges.measurementSet.floodRelative(floodOff).pix2iso(translator)
      val measuredX1X2Y1Y2 = measuredTBLR.toX1X2Y1Y2(collimatorAngle)

      val expMinusMeasured = expectedEdgesTBLR.minus(measuredTBLR).toX1X2Y1Y2(collimatorAngle)
      logger.info(
        "Beam " + beamName + " flood Comp: " + FloodCompensation +
          "\n    expected edges: " + expectedEdgesTBLR.toX1X2Y1Y2(collimatorAngle) +
          "\n    measured edges: " + measuredX1X2Y1Y2 +
          "\n    expected - measured: " + expMinusMeasured
      )

      val worst = expMinusMeasured.toSeq.map(m => m.abs).max

      val status = if (worst > Config.CollimatorCenteringTolerence_mm) ProcedureStatus.fail else ProcedureStatus.pass

      val uid = rtimage.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString

      val colPosn = new CollimatorPosition(
        None,
        outputPK,
        status.toString,
        uid,
        beamName,
        FloodCompensation,
        measuredX1X2Y1Y2.X1,
        measuredX1X2Y1Y2.X2,
        measuredX1X2Y1Y2.Y1,
        measuredX1X2Y1Y2.Y2,
        expMinusMeasured.X1,
        expMinusMeasured.X2,
        expMinusMeasured.Y1,
        expMinusMeasured.Y2,
        collimatorCenter.getX,
        collimatorCenter.getY,
        gantryAngle,
        collimatorAngle
      )

      logger.info("CollimatorPosition\n" + colPosn)

      Right(colPosn, edges.bufferedImage)
    } catch {
      case t: Throwable => {
        val msg = "Unexpected error while analyzing " + beamName + " for collimator position."
        logger.warn(msg + " : " + t + "\n" + fmtEx(t))
        Left(msg)
      }
    }

  }

  /**
    *  For testing only.
    */
  def testMeasureImage(
      beamName: String,
      FloodCompensation: Boolean,
      biasAndPixelCorrectedCroppedImage: DicomImage,
      pixelCorrectedImage: DicomImage,
      al: AttributeList,
      originalImage: DicomImage,
      outputPK: Long,
      floodOffset: Point,
      rtplan: AttributeList
  ): Either[String, (CollimatorPosition, BufferedImage)] = {
    measureImage(beamName, FloodCompensation, biasAndPixelCorrectedCroppedImage, pixelCorrectedImage, al, originalImage, outputPK, floodOffset, new Point2D.Double(0, 0), rtplan)
  }

  val subProcedureName = "Collimator Position"

  case class CollimatorPositionResult(sum: Elem, sts: ProcedureStatus.Value, resultList: Seq[CollimatorPosition], crashList: Seq[String]) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
    * Determine if the given image qualifies for collimator position analysis.  It must be on
    * the configured list of beams and have edges inside the image by 1/2 the penumbra thickness.
    */
  private def imageQualifies(rtimage: AttributeList, rtplan: AttributeList): Boolean = {

    val colPosnBeamNameList = Config.CollimatorPositionBeamList.map(cp => cp.beamName).toSet

    val beamNameOpt = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage)
    val nameOnList = {
      beamNameOpt match {
        case Some(beamName) => {
          val onList = colPosnBeamNameList.contains(beamName)
          logger.info("collimator beam " + beamName + " on configured list: " + onList)
          onList
        }
        case _ => false
      }
    }

    def isInside = {
      val collimatorAngle = rtimage.get(TagByName.BeamLimitingDeviceAngle).getDoubleValues.head
      val tblr = MeasureTBLREdges.imageCollimatorPositions(rtimage, rtplan).toTBLR(collimatorAngle)
      val trans = new IsoImagePlaneTranslator(rtimage)
      val imageMax = trans.pix2Iso(new Point2D.Double(trans.width, trans.height))
      val err = Config.PenumbraThickness_mm / 2
      val max = new Point2D.Double(imageMax.getX - err, imageMax.getY - err)
      val min = new Point2D.Double(-max.getX, -max.getY)

      val inside = (tblr.top >= min.getY) &&
        (tblr.bottom <= max.getY) &&
        (tblr.left >= min.getX) &&
        (tblr.right <= max.getX)
      logger.info("beam " + beamNameOpt + " collimator edges inside image: " + inside)
      inside
    }

    nameOnList && isInside
  }

  /**
    * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, CollimatorPositionResult] = {
    try {
      logger.info("Starting analysis of CollimatorPosition for machine " + extendedData.machine.id)
      val qualifiedImageList = runReq.derivedMap.toSeq.filter(ndf => imageQualifies(ndf._2.attributeList, runReq.rtplan)).toMap.keys.toSet

      val posnBeams = Config.CollimatorPositionBeamList.filter(cp => qualifiedImageList.contains(cp.beamName))
      val resultList = posnBeams.par
        .map(cp =>
          measureImage(
            cp.beamName,
            cp.FloodCompensation,
            runReq.derivedMap(cp.beamName).biasAndPixelCorrectedCroppedImage,
            runReq.derivedMap(cp.beamName).pixelCorrectedImage,
            runReq.derivedMap(cp.beamName).attributeList,
            runReq.derivedMap(cp.beamName).originalImage,
            extendedData.output.outputPK.get,
            runReq.floodOffset,
            collimatorCenteringResource.centerOfBeam(cp.beamName),
            runReq.rtplan
          )
        )
        .toList

      val doneList = resultList.filter(r => r.isRight).map(r => r.right.get)
      val crashList = resultList.filter(l => l.isLeft).map(l => l.left.get)

      // To pass (succeed), there must be no crashes and all successfully processed beams must pass.
      val pass = crashList.isEmpty && doneList.map(d => d._1.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      val doneDataList = doneList.map(r => r._1)
      CollimatorPosition.insert(doneDataList) // save to database
      logger.info("Inserted  " + doneList.size + " + CollimatorPosition rows.")

      // TODO Should make nice HTML for each buffered images.
      val elem = CollimatorPositionHTML.makeDisplay(extendedData, runReq, doneList, crashList, procedureStatus)
      val result = Right(new CollimatorPositionResult(elem, procedureStatus, doneDataList, crashList))
      logger.info("Finished analysis of CollimatorPosition.  Status: " + procedureStatus + " for machine " + extendedData.machine.id)
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
