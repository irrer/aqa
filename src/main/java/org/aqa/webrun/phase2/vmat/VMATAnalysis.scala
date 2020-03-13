package org.aqa.webrun.phase2.vmat

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.phase2.Phase2Util

import scala.xml.Elem
//import org.aqa.db.Output
//import org.aqa.db.Machine
//import org.aqa.db.Institution
//import org.aqa.db.Input
//import org.aqa.db.Procedure
//import org.aqa.db.User
//import org.aqa.Util
//import java.util.Date
//import org.aqa.web.WebServer
import org.aqa.db.VMAT
//import org.aqa.web.DicomAccess
//import org.aqa.web.WebUtil._
//import org.aqa.DicomFile
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName

import org.aqa.run.ProcedureStatus
//import java.io.File
//import org.aqa.Config
//import edu.umro.ImageUtil.DicomImage
//import java.awt.geom.Point2D
//import com.pixelmed.dicom.AttributeList
//import java.awt.Point
//import com.pixelmed.dicom.TagFromName
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import java.awt.geom.Rectangle2D
import com.pixelmed.dicom.SequenceAttribute

//import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.db.CollimatorCentering
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.IsoImagePlaneTranslator
import org.aqa.IsoImagePlaneTranslator
import org.aqa.Config
import edu.umro.ImageUtil.DicomImage
import org.aqa.Util

object VMATAnalysis extends Logging {

  private def getPlanAoiList(beamNameMlc: String, beamNameOpen: String, alDrGs: AttributeList, alOpen: AttributeList, plan: AttributeList): Seq[MeasureTBLREdges.TBLR] = {

    val beamSeq = Phase2Util.getBeamSequenceOfPlan(beamNameMlc, plan)
    val j = DicomUtil.findAllSingle(beamSeq, TagFromName.ControlPointSequence)
    val beamLimitList = DicomUtil.findAllSingle(beamSeq, TagFromName.BeamLimitingDevicePositionSequence).
      map(bdps => bdps.asInstanceOf[SequenceAttribute]).
      map(bdps => DicomUtil.alOfSeq(bdps)).
      flatten

    val controlPointSeq = DicomUtil.findAllSingle(beamSeq, TagFromName.ControlPointSequence).map(s => DicomUtil.alOfSeq(s.asInstanceOf[SequenceAttribute])).flatten

    /**
     * Determine if the limits are of interest, meaning that they are
     * MLC leaves oriented in the X direction and that the gap between
     * them is at least 5 mm.
     */
    def mlcOfInterest(bl: AttributeList): Option[(Double, Double)] = {
      def isMLC = {
        val t = bl.get(TagFromName.RTBeamLimitingDeviceType)
        (t != null) && t.getSingleStringValueOrEmptyString.toUpperCase.contains("MLCX")
      }

      val ljp = bl.get(TagFromName.LeafJawPositions).getDoubleValues.distinct
      val max = ljp.max
      val min = ljp.min

      def isWideEnough = (max - min) > 5 // must be greater than 5 mm

      if (isMLC && isWideEnough)
        Some((min, max))
      else None
    }

    case class MinMax(min: Double, max: Double) {
      val dist = max - min
    }

    // list of low-high pairs
    val xLimitList = beamLimitList.map(bl => mlcOfInterest(bl)).flatten.distinct.sortBy(minMax => minMax._1).map(minMax => new MinMax(minMax._1, minMax._2))

    // single low-high pair specifying Y limits
    val yLimits = {
      def isJawY(bl: AttributeList) = bl.get(TagFromName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("Y")
      val jawPos = beamLimitList.filter(bl => isJawY(bl)).head
      val pair = jawPos.get(TagFromName.LeafJawPositions).getDoubleValues.sorted
      new MinMax(pair(0), pair(1))
    }

    val aoiList = xLimitList.map(x => new MeasureTBLREdges.TBLR(yLimits.min, yLimits.max, x.min, x.max))
    aoiList
  }

  /** For testing only. */
  def testGetPlanAoiList(beamName: String, beamNameOpen: String, alDrGs: AttributeList, alOpen: AttributeList, plan: AttributeList): Seq[MeasureTBLREdges.TBLR] =
    getPlanAoiList(beamName, beamNameOpen, alDrGs, alOpen, plan)

  /**
   * Get the collimator angle of the given beam.
   */
  private def getCollimatorAngle(beamName: String, plan: AttributeList): Double = {
    val beamSeq = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val BeamLimitingDeviceAngle = DicomUtil.findAllSingle(beamSeq, TagFromName.BeamLimitingDeviceAngle).head
    val collAngle = BeamLimitingDeviceAngle.getDoubleValues.head
    collAngle
  }

  /**
   * Top level analysis for a single pair of beams.
   */
  private def analyze(beamNameMlc: String, beamNameOpen: String,
    alMlc: AttributeList, alOpen: AttributeList,
    plan: AttributeList, collimatorCentering: CollimatorCentering,
    extendedData: ExtendedData, runReq: RunReq): Seq[VMAT] = {
    val aoiSeqFromPlan = getPlanAoiList(beamNameMlc, beamNameOpen, alMlc, alOpen, plan)
    val translator = new IsoImagePlaneTranslator(alMlc)

    /**
     * Given a TBLR in rtplan coordinates, convert it to pixel coordinates.
     */
    def planToPix(tblr: MeasureTBLREdges.TBLR): MeasureTBLREdges.TBLR = {
      Seq(tblr).
        map(tblr => tblr.addOffset(collimatorCentering.center)). // compensate for central axis shift
        map(tblr => tblr.resize(-Config.VMATBorderThickness_mm)). //shrink to be safely away from the penumbra of edge effects
        map(tblr => tblr.iso2Pix(translator)). // convert to pixel coordinates
        head
    }

    val pixSeq = aoiSeqFromPlan.map(tblr => planToPix(tblr))

    val mlcAvgSeq = pixSeq.map(p => runReq.derivedMap(beamNameMlc).originalImage.averageOfRectangle(p.toRectangle))
    val openAvgSeq = pixSeq.map(p => runReq.derivedMap(beamNameOpen).originalImage.averageOfRectangle(p.toRectangle))

    val pctSeq = mlcAvgSeq.zip(openAvgSeq).map(mo => (100 * mo._1) / mo._2)
    val beamAverage_pct = pctSeq.sum / pctSeq.size
    val diffPctSeq = pctSeq.map(p => p - beamAverage_pct)

    val vmatSeq = (0 until aoiSeqFromPlan.size).map(i =>
      new VMAT(
        vmatPK = None,
        outputPK = extendedData.output.outputPK.get,
        SOPInstanceUID = Util.sopOfAl(alMlc),
        SOPInstanceUIDOpen = Util.sopOfAl(alOpen),
        beamName = beamNameMlc,
        beamNameOpen = beamNameOpen,
        averageDose_cu = mlcAvgSeq(i),
        averageDoseOpen_cu = openAvgSeq(i),
        beamAverage_pct,
        top_mm = aoiSeqFromPlan(i).top,
        bottom_mm = aoiSeqFromPlan(i).bottom,
        left_mm = aoiSeqFromPlan(i).left,
        right_mm = aoiSeqFromPlan(i).right))
    ??? // TODO : this has not been tested.
    vmatSeq
  }

  private val subProcedureName = "VMAT"

  case class VMATResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[VMAT]) extends SubProcedureResult(summry, stats, subProcedureName)

  /*
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, CenterDoseResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of CenterDose")
      val status = ProcedureStatus.done
      val resultList = analyse(extendedData, runReq, collimatorCentering)
      CenterDose.insert(resultList)
      val summary = VMATHTML.makeDisplay(extendedData, runReq, resultList, status)
      val result = Right(new CenterDoseResult(summary, status, resultList))
      logger.info("Finished analysis of CenterDose")
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
*/
}