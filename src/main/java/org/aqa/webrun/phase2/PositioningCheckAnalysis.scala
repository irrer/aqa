package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.PositioningCheck
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus

/**
 * Analyze DICOM files for ImageAnalysis.
 */
object PositioningCheckAnalysis extends Logging {

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

  private def modulo360(degrees: Double) = {
    val pos = (degrees + 3600.0) % 360.0
    if (pos <= 180) pos else pos - 360
  }

  def makePositioningCheck(plan: AttributeList, image: AttributeList): Option[PositioningCheck] = {

    def findBldpt(all: Seq[AttributeList], name: String) = all.filter(al => al.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.equalsIgnoreCase(name)).head

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

      val gantryAngleImage_deg = aDbl(imageExposureSeq, TagFromName.GantryAngle)
      val collimatorAngleImage_deg = aDbl(imageExposureSeq, TagFromName.BeamLimitingDeviceAngle)
      val energyImage_kev = aDbl(imageExposureSeq, TagFromName.KVP)

      val gantryAnglePlanMinusImage_deg = modulo360(gantryAnglePlan_deg - gantryAngleImage_deg)
      val collimatorAnglePlanMinusImage_deg = modulo360(collimatorAnglePlan_deg - collimatorAngleImage_deg)

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

      val positioningCheck = new PositioningCheck(
        None, // positioningCheckPK
        -1, // outputPK
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

      logger.info("PositioningCheckAnalysis.makePositioningCheck:\n" + positioningCheck)
      Some(positioningCheck)
    } catch {
      case t: Throwable => {
        logger.info("Unable to make PositioningCheck: " + t)
        None
      }
    }
  }

  /**
   * Run the PositioningCheck sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(output: Output, positioningCheckRunRequirements: PositioningCheckRunRequirements): (ProcedureStatus.Value, Elem) = {
    val outPK = output.outputPK.get
    val list = positioningCheckRunRequirements.imageIdFileList.map(imgId => imgId.positioningCheck.copy(outputPK = outPK))
    PositioningCheck.insert(list)
    val pass: Boolean = list.map(ii => ii.pass).reduce(_ && _)
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    val elem = PositioningCheckHTML.makeDisplay(output, positioningCheckRunRequirements, procedureStatus)
    (procedureStatus, elem)
  }
}
