package org.aqa.webrun.phase2.wedge

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
import org.aqa.db.Baseline
import org.aqa.webrun.phase2.Phase2Util.PMIBaseline
import org.aqa.db.MaintenanceCategory
import org.aqa.db.PMI
import org.aqa.webrun.phase2.ExtendedData
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import edu.umro.ScalaUtil.DicomUtil

/**
 * Analyze DICOM files for Wedge Analysis.
 */
object WedgeAnalysis extends Logging {

  def wedgeOrientationTransverse(beamName: String, plan: AttributeList): Boolean = {
    val bs = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val WedgeOrientation = DicomUtil.seqToAttr(bs, TagFromName.WedgeSequence).head.get(TagFromName.WedgeOrientation).getDoubleValues.head
    val horiz = Util.angleRoundedTo90(WedgeOrientation) match {
      case 0 => true
      case 90 => false
      case 180 => true
      case 270 => false
    }
    horiz
  }

  def makeBaselineName(wedgeBeamName: String, backgroundBeamName: String): String = "Wedge " + wedgeBeamName + " / " + backgroundBeamName

  def makeBaselineName(wedgePair: Config.WedgeBeamPair): String = makeBaselineName(wedgePair.wedge, wedgePair.background)

  def makeBaselineName(wedgePoint: WedgePoint): String = makeBaselineName(wedgePoint.wedgeBeamName, wedgePoint.backgroundBeamName)

  /**
   * Either get the existing baseline or
   */
  private def getBaseline(machinePK: Long, wedgePair: Config.WedgeBeamPair, attributeList: AttributeList, value: Double): PMIBaseline = {
    val id = makeBaselineName(wedgePair)
    Baseline.findLatest(machinePK, id) match {
      case Some((pmi, baseline)) => new PMIBaseline(Some(pmi), baseline)
      case _ => new PMIBaseline(None, Baseline.makeBaseline(-1, attributeList, id, value))
    }
  }

  /**
   * Given the list of wedge points, determine if any of them need to have a baseline value created, and if so, create
   * them.  If any baseline values are created, then create the corresponding PMI record.
   */
  private def updateBaselineAndPMI(wedgePointList: Seq[WedgePoint], extendedData: ExtendedData, runReq: RunReq): Unit = {

    def constructOneBaseline(wedgePoint: WedgePoint): Option[Baseline] = {
      val id = makeBaselineName(wedgePoint.wedgePair)
      Baseline.findLatest(extendedData.machine.machinePK.get, id) match {
        case Some((pmi, baseline)) => None
        case _ => Some(Baseline.makeBaseline(-1, runReq.rtimageMap(wedgePoint.wedgeBeamName).attributeList.get, id, wedgePoint.percentOfBackground_pct))
      }
    }

    val newBaselineList = wedgePointList.map(wp => constructOneBaseline(wp)).flatten
    if (newBaselineList.nonEmpty) {
      logger.info("Automatically creating new PMI and " + newBaselineList.size + "default Wedge baseline(s) for " + extendedData.institution.name + " : " + extendedData.machine.id)

      val summary = "Automatically created baseline value for Wedge."
      val preamble = "Wedge baseline values are created automatically if they have not been established for the given machine.  The following is a list of the values:\n\n"
      val valueText = newBaselineList.map(bl => "    " + bl.id + " : " + bl.value).mkString("\n")
      val analysisTime = extendedData.output.startDate
      val pmiOrig = new PMI(
        None,
        MaintenanceCategory.setBaseline,
        extendedData.machine.machinePK.get,
        analysisTime,
        extendedData.user.userPK.get,
        extendedData.output.outputPK,
        summary,
        preamble + valueText)
      val pmi = pmiOrig.insert

      val list = newBaselineList.map(bl => bl.copy(pmiPK = pmi.pmiPK.get))
      Baseline.insert(list)
      logger.info("Wedge PMI and baseline inserted")
    }
  }

  private def analyzeWedgePair(wedgePair: Config.WedgeBeamPair, pointList: Seq[Point], extendedData: ExtendedData, runReq: RunReq): WedgePoint = {

    def measure(beamName: String) = {
      val derived = runReq.derivedMap(beamName)
      Phase2Util.measureDose(pointList, derived.originalImage, derived.attributeList)
    }

    val baselineId = makeBaselineName(wedgePair)
    val pmiBaseline = Baseline.findLatest(extendedData.machine.machinePK.get, baselineId)

    val derivedWedge = runReq.derivedMap(wedgePair.wedge)
    val derivedBackground = runReq.derivedMap(wedgePair.background)

    val wedgeDose = measure(wedgePair.wedge)
    val backgroundDose = measure(wedgePair.background)
    val percent = (wedgeDose * 100) / backgroundDose

    // if the baseline has been established, then use it, otherwise use the new value
    val baselineValue: Double = {
      if (pmiBaseline.isDefined) pmiBaseline.get._2.value.toDouble
      else percent
    }

    val wedgePoint = new WedgePoint(None, extendedData.output.outputPK.get,
      Util.sopOfAl(derivedWedge.attributeList), wedgePair.wedge, wedgeDose,
      Util.sopOfAl(derivedBackground.attributeList), wedgePair.background, backgroundDose,
      percent,
      baselineValue)

    wedgePoint
  }

  val subProcedureName = "Wedge"

  /**
   * Use the center dose and flood CenterDose points to calculate the WedgePoints.
   */
  private def analyze(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering, centerDoseList: Seq[CenterDose]): Seq[WedgePoint] = {
    val outputPK = extendedData.output.outputPK.get

    val pointList = Phase2Util.makeCenterDosePointList(runReq.flood.attributeList.get)

    val beamSet = runReq.rtimageMap.keySet
    val validBeamNameList = Config.WedgeBeamList.filter(b => beamSet.contains(b.wedge) && beamSet.contains(b.background))
    validBeamNameList.map(b => analyzeWedgePair(b, pointList, extendedData, runReq))
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
      updateBaselineAndPMI(wedgePointList, extendedData, runReq)
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
