package org.aqa.webrun.phase2

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
import org.aqa.db.Wedge
import java.awt.Rectangle
import org.aqa.db.CollimatorCentering
import java.awt.Point
import java.awt.geom.Point2D
import org.aqa.db.CenterDose
import org.aqa.db.WedgePoint

/**
 * Analyze DICOM files for Wedge Analysis.
 */
object WedgeAnalysis extends Logging {

  def wedgeOrientationTransverse(beamName: String, plan: AttributeList): Boolean = {
    val bs = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val WedgeOrientation = Util.seq2Attr(bs, TagFromName.WedgeSequence).head.get(TagFromName.WedgeOrientation).getDoubleValues.head
    val horiz = Util.angleRoundedTo90(WedgeOrientation) match {
      case 0 => true
      case 90 => false
      case 180 => true
      case 270 => false
    }
    horiz
  }

  val subProcedureName = "Wedge"

  /**
   * Use the center dose and flood CenterDose points to calculate the WedgePoints.
   */
  private def analyze(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering, centerDoseList: Seq[CenterDose]): Seq[WedgePoint] = {
    val flood = centerDoseList.find(b => b.beamName.equals(Config.FloodFieldBeamName)).get
    val outputPK = extendedData.output.outputPK.get

    def centerDoseToWedgePoint(beamName: String): WedgePoint = {
      val centerDose = centerDoseList.find(c => c.beamName.equals(beamName)).get
      new WedgePoint(None, outputPK, centerDose.SOPInstanceUID, centerDose.beamName, centerDose.dose, flood.dose, (centerDose.dose * 100) / flood.dose)
    }

    val validBeamNameList = Config.WedgeBeamList.filter(b => centerDoseList.find(c => c.beamName.equals(b)).isDefined)
    validBeamNameList.map(b => centerDoseToWedgePoint(b))
  }

  class WedgeResult(summary: Elem, status: ProcedureStatus.Value, wedgePointList: Seq[WedgePoint]) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the WedgeAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering, centerDoseList: Seq[CenterDose]): Either[Elem, WedgeResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName)

      val wedgePointList = analyze(extendedData, runReq, collimatorCentering, centerDoseList)
      WedgePoint.insert(wedgePointList)
      val status = ProcedureStatus.done
      val summary = WedgeHTML.makeDisplay(extendedData, status, runReq, wedgePointList)
      val result = new WedgeResult(summary, status, wedgePointList)
      logger.info("Finished analysis of " + subProcedureName)
      Right(result)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of MetadataCheck: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
