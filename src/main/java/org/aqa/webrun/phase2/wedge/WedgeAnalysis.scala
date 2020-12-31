package org.aqa.webrun.phase2.wedge

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.CenterDose
import org.aqa.db.CollimatorCentering
import org.aqa.db.MaintenanceCategory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.WedgePoint
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.Point
import scala.collection.Seq
import scala.xml.Elem

/**
 * Analyze DICOM files for Wedge Analysis.
 */
object WedgeAnalysis extends Logging {

  def wedgeOrientationTransverse(beamName: String, plan: AttributeList): Boolean = {
    val bs = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val WedgeOrientation = DicomUtil.seqToAttr(bs, TagByName.WedgeSequence).head.get(TagByName.WedgeOrientation).getDoubleValues.head
    val horiz = Util.angleRoundedTo90(WedgeOrientation) match {
      case 0 => true
      case 90 => false
      case 180 => true
      case 270 => false
    }
    horiz
  }

  def makeWedgeBaselineName(wedgeBeamName: String, backgroundBeamName: String): String = "Wedge " + wedgeBeamName + " / " + backgroundBeamName

  def makeWedgeBaselineName(wedgePair: WedgePair): String = makeWedgeBaselineName(wedgePair.beamName, wedgePair.backgroundBeamName)

  def makeWedgeBaselineName(wedgePoint: WedgePoint): String = makeWedgeBaselineName(wedgePoint.wedgeBeamName, wedgePoint.backgroundBeamName)

  /**
   * Given the list of wedge points, determine if any of them need to have a baseline value created, and if so, create
   * them.  If any baseline values are created, then create the corresponding MaintenanceRecord record.
   */
  private def updateBaselineAndMaintenanceRecord(wedgePointList: Seq[WedgePoint], extendedData: ExtendedData, runReq: RunReq): Unit = {

    def constructOneBaseline(wedgePoint: WedgePoint): Option[Baseline] = {
      val id = makeWedgeBaselineName(wedgePoint.wedgeBeamName, wedgePoint.backgroundBeamName)
      Baseline.findLatest(extendedData.machine.machinePK.get, id, extendedData.output.dataDate.get) match {
        case Some((maintenanceRecord, baseline)) => None
        case _ => Some(Baseline.makeBaseline(-1, extendedData.output.dataDate.get, Util.sopOfAl(runReq.rtimageMap(wedgePoint.wedgeBeamName)), id, wedgePoint.percentOfBackground_pct))
      }
    }

    val newBaselineList = wedgePointList.flatMap(wp => constructOneBaseline(wp))
    if (newBaselineList.nonEmpty) {
      logger.info("Automatically creating new MaintenanceRecord and " + newBaselineList.size + "default Wedge baseline(s) for " + extendedData.institution.name + " : " + extendedData.machine.id)

      val summary = "Automatically created baseline value for Wedge."
      val preamble = "Wedge baseline values are created automatically if they have not been established for the given machine.  The following is a list of the values:\n\n"
      val valueText = newBaselineList.map(bl => "    " + bl.id + " : " + bl.value).mkString("\n")
      val analysisTime = extendedData.output.startDate
      val maintenanceRecordOrig = new MaintenanceRecord(
        None,
        MaintenanceCategory.setBaseline,
        extendedData.machine.machinePK.get,
        analysisTime,
        extendedData.user.userPK.get,
        extendedData.output.outputPK,
        summary,
        preamble + valueText)
      val maintenanceRecord = maintenanceRecordOrig.insert

      val list = newBaselineList.map(bl => bl.copy(maintenanceRecordPK = maintenanceRecord.maintenanceRecordPK.get))
      Baseline.insert(list)
      logger.info("Wedge MaintenanceRecord and baseline inserted")
    }
  }

  private case class WedgePair(beamName: String, backgroundBeamName: String)

  private def analyzeWedgePair(wedgePair: WedgePair, pointList: Seq[Point], extendedData: ExtendedData, runReq: RunReq): WedgePoint = {
    logger.info("Starting individual wedge analysis of " + wedgePair.beamName + " with background " + wedgePair.backgroundBeamName)

    def measure(beamName: String) = {
      val derived = runReq.derivedMap(beamName)
      Phase2Util.measureDose(pointList, derived.originalImage, derived.attributeList)
    }

    val baselineId = makeWedgeBaselineName(wedgePair)
    val maintenanceRecordBaseline = Baseline.findLatest(extendedData.machine.machinePK.get, baselineId, extendedData.output.dataDate.get)

    val derivedWedge = runReq.derivedMap(wedgePair.beamName)
    val derivedBackground = runReq.derivedMap(wedgePair.backgroundBeamName)

    val wedgeDose = measure(wedgePair.beamName)
    val backgroundDose = measure(wedgePair.backgroundBeamName)
    val percent = (wedgeDose * 100) / backgroundDose

    // if the baseline has been established, then use it, otherwise use the new value
    val baselineValue: Double = {
      if (maintenanceRecordBaseline.isDefined) maintenanceRecordBaseline.get._2.value.toDouble
      else percent
    }

    val wedgePoint = new WedgePoint(None, extendedData.output.outputPK.get,
      Util.sopOfAl(derivedWedge.attributeList), wedgePair.beamName, isBaseline_text = false.toString, wedgeDose,
      Util.sopOfAl(derivedBackground.attributeList), wedgePair.backgroundBeamName, backgroundDose,
      percent,
      baselineValue)

    logger.info("Finished individual wedge analysis of " + wedgePair.beamName + " with background " + wedgePair.backgroundBeamName + "\nResult:\n" + wedgePoint)
    wedgePoint
  }

  val subProcedureName = "Wedge"

  /**
   * Use the center dose and flood CenterDose points to calculate the WedgePoints.
   */
  private def analyze(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering, centerDoseList: Seq[CenterDose]): Seq[WedgePoint] = {
    val outputPK = extendedData.output.outputPK.get

    val pointList = Phase2Util.makeCenterDosePointList(runReq.flood, collimatorCentering.center)

    val beamList = runReq.derivedMap.keys.toList

    def findBeamPair(wedgeBeamName: String): Option[WedgePair] = {
      Config.WedgeBeamList.find(w => w.wedge.equalsIgnoreCase(wedgeBeamName)) match {
        case Some(wedgeBeam) => {

          val bg = wedgeBeam.backgroundList.map(bg => beamList.find(c => c.equalsIgnoreCase(bg))).flatten
          if (bg.isEmpty)
            None // no background beam
          else {
            Some(WedgePair(wedgeBeamName, bg.head)) // use the first one in the list of allowable background beams
          }
        }
        case _ => None // wedge beam name not in input list
      }

    }

    val wedgePairList = beamList.map(b => findBeamPair(b)).flatten
    wedgePairList.map(wp => analyzeWedgePair(wp, pointList, extendedData, runReq))
  }

  class WedgeResult(summary: Elem, status: ProcedureStatus.Value, wedgePointList: Seq[WedgePoint]) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the WedgeAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering, centerDoseList: Seq[CenterDose]): Either[Elem, WedgeResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName + " for machine " + extendedData.machine.id)

      val wedgePointList = analyze(extendedData, runReq, collimatorCentering, centerDoseList)
      WedgePoint.insert(wedgePointList)
      updateBaselineAndMaintenanceRecord(wedgePointList, extendedData, runReq)
      val status = ProcedureStatus.done
      logger.info("Starting HTML generation for " + subProcedureName)
      val summary = WedgeHTML.makeDisplay(extendedData, status, runReq, wedgePointList, collimatorCentering.center)
      logger.info("Finished HTML generation for " + subProcedureName)
      val result = new WedgeResult(summary, status, wedgePointList)
      logger.info("Finished analysis of " + subProcedureName + " for machine " + extendedData.machine.id)
      Right(result)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of MetadataCheck: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
