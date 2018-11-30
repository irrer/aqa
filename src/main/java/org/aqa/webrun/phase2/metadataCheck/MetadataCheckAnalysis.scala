package org.aqa.webrun.phase2.metadataCheck

import org.aqa.Logging
import org.aqa.db.MetadataCheck
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object MetadataCheckAnalysis extends Logging {

  /**
   * Determine if the given attribute list references the given beam number.
   */
  private def matchesBeam(beamNumber: Int, al: AttributeList): Boolean = {
    (al.get(TagFromName.BeamNumber).getIntegerValues.head == beamNumber)
  }

  /**
   * Look through the BeamSequence and find the ReferencedBeamSequence that matches the given beam.
   */
  private def getBeamSequence(plan: AttributeList, beamNumber: Int): AttributeList = {
    Util.seq2Attr(plan, TagFromName.BeamSequence).find(bs => matchesBeam(beamNumber, bs)).get
  }

  private val JAW_NAME_X = "ASYMX"
  private val JAW_NAME_Y = "ASYMY"
  private val jawNameSeq = Seq(JAW_NAME_X, JAW_NAME_Y)

  private def getJawPosns(jawSeq: Seq[AttributeList]) = {
    Seq("X", "Y").map(name => jawSeq.filter(jp => jp.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.toUpperCase.endsWith(name)).
      head.get(TagFromName.LeafJawPositions).getDoubleValues)
  }

  private def getPlanJawPositions(plan: AttributeList, beamNumber: Int): Seq[Array[Double]] = {
    // beam in BeamSequence that matches beamNumber
    val bs = Util.seq2Attr(plan, TagFromName.BeamSequence).filter(b => b.get(TagFromName.BeamNumber).getIntegerValues.head == beamNumber).head
    val controlPoint = Util.seq2Attr(bs, TagFromName.ControlPointSequence).head
    val bldps = Util.seq2Attr(controlPoint, TagFromName.BeamLimitingDevicePositionSequence)
    getJawPosns(bldps)
  }

  private def getImageJawPositions(image: AttributeList): Seq[Array[Double]] = {
    // beam in BeamSequence that matches beamNumber
    val expSeq = Util.seq2Attr(image, TagFromName.ExposureSequence).head
    val blds = Util.seq2Attr(expSeq, TagFromName.BeamLimitingDeviceSequence)
    getJawPosns(blds)
  }

  def makeMetadata(outputPK: Long, plan: AttributeList, image: AttributeList): Option[MetadataCheck] = {

    def findBldpt(all: Seq[AttributeList], name: String) = all.filter(al => al.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.equalsIgnoreCase(name)).head

    def angleDiff(a: Double, b: Double): Double = {
      val diff = Util.modulo360(a - b)
      if (diff <= 180) diff else diff - 360
    }

    try {
      val imageBeamNumber = image.get(TagFromName.ReferencedBeamNumber).getIntegerValues.head
      val planBeamSeq = getBeamSequence(plan, imageBeamNumber)
      val planCtrlPointSeq = Util.seq2Attr(planBeamSeq, TagFromName.ControlPointSequence).head
      val imageExposureSeq = Util.seq2Attr(image, TagFromName.ExposureSequence).head

      def aDbl(al: AttributeList, dblTag: AttributeTag): Double = doubleArrayOps(al.get(dblTag).getDoubleValues).head

      val planJawPosns = getPlanJawPositions(plan, imageBeamNumber)
      val imageJawPosns = getImageJawPositions(image)

      val beamName = planBeamSeq.get(TagFromName.BeamName).getSingleStringValueOrNull

      val gantryAnglePlan_deg = aDbl(planCtrlPointSeq, TagFromName.GantryAngle)
      val collimatorAnglePlan_deg = aDbl(planCtrlPointSeq, TagFromName.BeamLimitingDeviceAngle)
      val energyPlan_kev = aDbl(planCtrlPointSeq, TagFromName.NominalBeamEnergy) * 1000.0 //   Convert from MeV to KeV

      val gantryAngleImage_deg = aDbl(image, TagFromName.GantryAngle)
      val collimatorAngleImage_deg = aDbl(image, TagFromName.BeamLimitingDeviceAngle)
      val energyImage_kev = aDbl(imageExposureSeq, TagFromName.KVP)

      val gantryAnglePlanMinusImage_deg = angleDiff(gantryAnglePlan_deg, gantryAngleImage_deg)
      val collimatorAnglePlanMinusImage_deg = angleDiff(collimatorAnglePlan_deg, collimatorAngleImage_deg)

      val toleranceTable = Util.seq2Attr(plan, TagFromName.ToleranceTableSequence).head

      val tolGantryAngle = aDbl(toleranceTable, TagFromName.GantryAngleTolerance)
      val tolCollimatorAngle = aDbl(toleranceTable, TagFromName.BeamLimitingDeviceAngleTolerance)
      val tolJaw = Util.seq2Attr(toleranceTable, TagFromName.BeamLimitingDeviceToleranceSequence)
      val tolJawX = aDbl(findBldpt(tolJaw, JAW_NAME_X), TagFromName.BeamLimitingDevicePositionTolerance)
      val tolJawY = aDbl(findBldpt(tolJaw, JAW_NAME_Y), TagFromName.BeamLimitingDevicePositionTolerance)

      val x1JawPlan_mm = planJawPosns(0)(0)
      val x1JawPlanMinusImage_mm = planJawPosns(0)(0) - imageJawPosns(0)(0)
      val x2JawPlan_mm = planJawPosns(0)(1)
      val x2JawPlanMinusImage_mm = planJawPosns(0)(1) - imageJawPosns(0)(1)
      val y1JawPlan_mm = planJawPosns(1)(0)
      val y1JawPlanMinusImage_mm = planJawPosns(1)(0) - imageJawPosns(1)(0)
      val y2JawPlan_mm = planJawPosns(1)(1)
      val y2JawPlanMinusImage_mm = planJawPosns(1)(1) - imageJawPosns(1)(1)

      val pass = {
        (gantryAnglePlanMinusImage_deg.abs < tolGantryAngle) &&
          (collimatorAnglePlanMinusImage_deg.abs < tolCollimatorAngle) &&
          (x1JawPlanMinusImage_mm.abs < tolJawX) &&
          (x2JawPlanMinusImage_mm.abs < tolJawX) &&
          (y1JawPlanMinusImage_mm.abs < tolJawY) &&
          (y2JawPlanMinusImage_mm.abs < tolJawY)
      }

      // image (independent of plan) uses flattening filter free
      val flatteningFilter = image.get(TagFromName.RTImageDescription).getSingleStringValueOrEmptyString.toLowerCase.contains("fff")

      val metadataCheck = new MetadataCheck(
        None, // metadataCheckPK
        outputPK, // outputPK
        beamName,
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
        pass)

      logger.info("MetadataCheckAnalysis.makeMetadata:\n" + metadataCheck)
      Some(metadataCheck)
    } catch {
      case t: Throwable => {
        logger.info("Unable to make Metadata: " + t + "\n" + fmtEx(t))
        None
      }
    }
  }

  private val subProcedureName = "Metadata Check"

  class MetadataResult(summary: Elem, status: ProcedureStatus.Value, resultList: Seq[MetadataCheck]) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the Metadata sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, MetadataResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName)
      val planAttrList = runReq.rtplan.attributeList.get

      val rtimageList = runReq.rtimageMap.filter(img => Config.MetadataCheckBeamNameList.contains(img._1)).map(img => img._2).toList
      val resultList = rtimageList.map(rtimage => makeMetadata(extendedData.output.outputPK.get, planAttrList, rtimage.attributeList.get)).flatten

      // make sure all were processed and that they all passed
      val pass = (resultList.size == Config.MetadataCheckBeamNameList.size) && resultList.map(pc => pc.pass).reduce(_ && _)
      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      MetadataCheck.insert(resultList)
      val elem = MetadataCheckHTML.makeDisplay(extendedData, runReq, resultList, procedureStatus)
      val pcr = Right(new MetadataResult(elem, procedureStatus, resultList))
      logger.info("Finished analysis of Metadata")
      pcr
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of Metadata: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}