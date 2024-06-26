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

package org.aqa.webrun.phase2.metadataCheck

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.MetadataCheck
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import scala.xml.Elem

/**
  * Analyze DICOM files for ImageAnalysis.
  */
object MetadataCheckAnalysis extends Logging {

  private val JAW_NAME_X = "ASYMX"
  private val JAW_NAME_Y = "ASYMY"

  private def getJawPosns(jawSeq: Seq[AttributeList]) = {
    Seq("X", "Y").map(name =>
      jawSeq.filter(jp => jp.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.toUpperCase.endsWith(name)).head.get(TagByName.LeafJawPositions).getDoubleValues
    )
  }

  private def getPlanJawPositions(plan: AttributeList, beamNumber: Int): Seq[Array[Double]] = {
    // beam in BeamSequence that matches beamNumber
    val bs = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).filter(b => b.get(TagByName.BeamNumber).getIntegerValues.head == beamNumber).head
    val controlPoint = DicomUtil.seqToAttr(bs, TagByName.ControlPointSequence).head
    val bldps = DicomUtil.seqToAttr(controlPoint, TagByName.BeamLimitingDevicePositionSequence)
    getJawPosns(bldps)
  }

  private def getImageJawPositions(image: AttributeList): Seq[Array[Double]] = {
    // beam in BeamSequence that matches beamNumber
    val expSeq = DicomUtil.seqToAttr(image, TagByName.ExposureSequence).head
    val blds = DicomUtil.seqToAttr(expSeq, TagByName.BeamLimitingDeviceSequence)
    getJawPosns(blds)
  }

  def makeMetadata(outputPK: Long, plan: AttributeList, image: AttributeList): Option[MetadataCheck] = {

    def findBldpt(all: Seq[AttributeList], name: String) = all.filter(al => al.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.equalsIgnoreCase(name)).head

    def angleDiff(a: Double, b: Double): Double = {
      val diff = Util.modulo360(a - b)
      if (diff <= 180) diff else diff - 360
    }

    try {
      val imageBeamNumber = image.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
      val planBeamSeq = Phase2Util.getBeamSequence(plan, imageBeamNumber)
      val planCtrlPointSeqList = DicomUtil.seqToAttr(planBeamSeq, TagByName.ControlPointSequence)
      val imageExposureSeq = DicomUtil.seqToAttr(image, TagByName.ExposureSequence).head

      def allPlanCtrlPtDbl(dblTag: AttributeTag): Seq[Double] = planCtrlPointSeqList.filter(al => al.get(dblTag) != null).map(al => al.get(dblTag).getDoubleValues.head)
      def aDblSeq(al: AttributeList, dblTag: AttributeTag): Seq[Double] = al.get(dblTag).getDoubleValues.toSeq
      def aDblHead(al: AttributeList, dblTag: AttributeTag): Double = aDblSeq(al, dblTag).head

      val planJawPosns = getPlanJawPositions(plan, imageBeamNumber)
      val imageJawPosns = getImageJawPositions(image)

      val beamName = Util.normalizedBeamName(planBeamSeq)

      val SOPInstanceUID = image.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString()

      val gantryAnglePlan_deg = allPlanCtrlPtDbl(TagByName.GantryAngle).last
      val collimatorAnglePlan_deg = aDblHead(planCtrlPointSeqList.head, TagByName.BeamLimitingDeviceAngle)
      val energyPlan_kev = aDblHead(planCtrlPointSeqList.head, TagByName.NominalBeamEnergy) * 1000.0 //   Convert from MeV to KeV

      val gantryAngleImage_deg = aDblHead(image, TagByName.GantryAngle)
      val collimatorAngleImage_deg = aDblHead(image, TagByName.BeamLimitingDeviceAngle)
      val energyImage_kev = aDblHead(imageExposureSeq, TagByName.KVP)

      val gantryAnglePlanMinusImage_deg = angleDiff(gantryAnglePlan_deg, gantryAngleImage_deg)
      val collimatorAnglePlanMinusImage_deg = angleDiff(collimatorAnglePlan_deg, collimatorAngleImage_deg)

      val toleranceTable = DicomUtil.seqToAttr(plan, TagByName.ToleranceTableSequence).head

      val tolGantryAngle = aDblHead(toleranceTable, TagByName.GantryAngleTolerance)
      val tolCollimatorAngle = aDblHead(toleranceTable, TagByName.BeamLimitingDeviceAngleTolerance)
      val tolJaw = DicomUtil.seqToAttr(toleranceTable, TagByName.BeamLimitingDeviceToleranceSequence)
      val tolJawX = aDblHead(findBldpt(tolJaw, JAW_NAME_X), TagByName.BeamLimitingDevicePositionTolerance)
      val tolJawY = aDblHead(findBldpt(tolJaw, JAW_NAME_Y), TagByName.BeamLimitingDevicePositionTolerance)

      // If this is a wedge, then do not consider jaw errors as errors.
      val isWedge = (planBeamSeq.get(TagByName.NumberOfWedges) != null) && (planBeamSeq.get(TagByName.NumberOfWedges).getIntegerValues.head > 0)

      val x1JawPlan_mm = planJawPosns.head(0)
      val x1JawPlanMinusImage_mm = planJawPosns.head(0) - imageJawPosns.head(0)
      val x2JawPlan_mm = planJawPosns.head(1)
      val x2JawPlanMinusImage_mm = planJawPosns.head(1) - imageJawPosns.head(1)
      val y1JawPlan_mm = planJawPosns(1)(0)
      val y1JawPlanMinusImage_mm = planJawPosns(1)(0) - imageJawPosns(1)(0)
      val y2JawPlan_mm = planJawPosns(1)(1)
      val y2JawPlanMinusImage_mm = planJawPosns(1)(1) - imageJawPosns(1)(1)

      val pass = {
        (gantryAnglePlanMinusImage_deg.abs < tolGantryAngle) &&
        (collimatorAnglePlanMinusImage_deg.abs < tolCollimatorAngle) &&
        (isWedge ||
        (x1JawPlanMinusImage_mm.abs < tolJawX) &&
        (x2JawPlanMinusImage_mm.abs < tolJawX) &&
        (y1JawPlanMinusImage_mm.abs < tolJawY) &&
        (y2JawPlanMinusImage_mm.abs < tolJawY))
      }

      // image (independent of plan) uses flattening filter free
      val flatteningFilter = image.get(TagByName.RTImageDescription).getSingleStringValueOrEmptyString.toLowerCase.contains("fff")

      val metadataCheck = new MetadataCheck(
        None, // metadataCheckPK
        outputPK, // outputPK
        beamName,
        Some(SOPInstanceUID),
        gantryAnglePlan_deg,
        gantryAnglePlanMinusImage_deg,
        collimatorAnglePlan_deg,
        collimatorAnglePlanMinusImage_deg,
        x1JawPlan_mm,
        x1JawPlanMinusImage_mm,
        x2JawPlan_mm,
        x2JawPlanMinusImage_mm,
        y1JawPlan_mm,
        y1JawPlanMinusImage_mm,
        y2JawPlan_mm,
        y2JawPlanMinusImage_mm,
        energyPlan_kev,
        energyPlan_kev - energyImage_kev,
        flatteningFilter,
        pass
      )

      logger.info("MetadataCheckAnalysis.makeMetadata:\n" + metadataCheck)
      Some(metadataCheck)
    } catch {
      case t: Throwable =>
        logger.info("Unable to make Metadata: " + t + "\n" + fmtEx(t))
        None
    }
  }

  private val subProcedureName = "Metadata Check"

  class MetadataResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
    * Run the Metadata sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, MetadataResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName + "  for machine " + extendedData.machine.id)
      val planAttrList = runReq.rtplan

      val rtimageList = runReq.rtimageMap.values.toList
      val resultList = rtimageList.flatMap(rtimage => makeMetadata(extendedData.output.outputPK.get, planAttrList, rtimage))

      // make sure all were processed and that they all passed
      val pass = (resultList.size == rtimageList.size) && resultList.map(pc => pc.pass).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      MetadataCheck.insert(resultList)
      val elem = MetadataCheckHTML.makeDisplay(extendedData, runReq, resultList, procedureStatus)
      val pcr = Right(new MetadataResult(elem, procedureStatus))
      logger.info("Finished analysis of Metadata for machine " + extendedData.machine.id)
      pcr
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of Metadata: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }
}
