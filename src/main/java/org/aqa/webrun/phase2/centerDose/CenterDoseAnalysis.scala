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

package org.aqa.webrun.phase2.centerDose

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ImageUtil.DicomImage
import org.aqa.db.CenterDose
import org.aqa.DicomFile
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.geom.Point2D
import java.awt.Point
import scala.xml.Elem

object CenterDoseAnalysis extends Logging {

  /**
    * Construct a CenterDose
    */
  private def constructCenterDose(beamName: String, pointList: Seq[Point], outputPK: Long, dicomImage: DicomImage, attributeList: AttributeList): CenterDose = {
    val dose = Phase2Util.measureDose(pointList, dicomImage, attributeList)
    val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
    val units = attributeList.get(TagFromName.RescaleType).getSingleStringValueOrEmptyString
    new CenterDose(None, outputPK, SOPInstanceUID, beamName, dose, units)
  }

  private def analyse(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Seq[CenterDose] = {
    val outputPK = extendedData.output.outputPK.get

    val availableBeamList = Config.CenterDoseBeamNameList.filter(beamName => runReq.derivedMap.contains(beamName))
    val resultList = availableBeamList.map(beamName =>
      constructCenterDose(beamName, collimatorCenteringResource.centerPointListOfBeam(beamName), outputPK, runReq.derivedMap(beamName).originalImage, runReq.rtimageMap(beamName))
    )
    logger.info("Number of CenterDose results " + resultList.size)
    logger.info("CenterDose results:\n" + resultList.mkString("\n"))
    resultList
  }

  /**
    * For testing only.
    */
  def testConstructCenterDose(beamName: String, dicomFile: DicomFile): CenterDose = {
    val attributeList = dicomFile.attributeList.get
    val pointList = Phase2Util.makeCenterDosePointList(attributeList, new Point2D.Double(0, 0))
    val outputPK = -1
    constructCenterDose(beamName, pointList, outputPK, new DicomImage(attributeList), attributeList)
  }

  private val subProcedureName = "Center Dose"

  case class CenterDoseResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[CenterDose]) extends SubProcedureResult(summry, stats, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, CenterDoseResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CenterDose for machine " + extendedData.machine.id)
      val status = ProcedureStatus.done
      val resultList = analyse(extendedData, runReq, collimatorCenteringResource)
      CenterDose.insert(resultList)
      val summary = CenterDoseHTML.makeDisplay(extendedData, runReq, resultList, status)
      val result = Right(new CenterDoseResult(summary, status, resultList))
      logger.info("Finished analysis of CenterDose for machine " + extendedData.machine.id)
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }

}
