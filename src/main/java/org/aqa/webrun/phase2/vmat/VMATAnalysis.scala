package org.aqa.webrun.phase2.vmat

import com.pixelmed.dicom.AttributeList
import org.aqa.webrun.phase2.Phase2Util
import scala.xml.Elem
import org.aqa.db.VMAT
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import org.aqa.run.ProcedureStatus
import org.aqa.Logging
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
import java.awt.geom.Rectangle2D
import com.pixelmed.dicom.SequenceAttribute
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
    outputPK: Long, mlcImage: DicomImage, openImage: DicomImage): Seq[VMAT] = {
    val aoiSeqFromPlan_mm = getPlanAoiList(beamNameMlc, beamNameOpen, alMlc, alOpen, plan)
    val translator = new IsoImagePlaneTranslator(alMlc)

    /**
     * Given a TBLR in rtplan coordinates, convert it to a rectangle in the
     * isoplane that should be used to take the measurements.
     */
    def planToMeasured(tblr: MeasureTBLREdges.TBLR): MeasureTBLREdges.TBLR = {
      tblr.addOffset(collimatorCentering.center).resize(-Config.VMATBorderThickness_mm)
    }

    val measuredSeq_mm = aoiSeqFromPlan_mm.map(tblr => planToMeasured(tblr))
    val pixSeq_pix = measuredSeq_mm.map(tblr => tblr.iso2Pix(translator))

    val mlcAvgSeq = pixSeq_pix.map(p => mlcImage.averageOfRectangle(p.toRectangle))
    val openAvgSeq = pixSeq_pix.map(p => openImage.averageOfRectangle(p.toRectangle))

    val pctSeq = (0 until mlcAvgSeq.size).map(i => (mlcAvgSeq(i) * 100) / openAvgSeq(i))
    val beamAverage_pct = pctSeq.sum / pctSeq.size
    val diffPctSeq = pctSeq.map(p => p - beamAverage_pct)

    val vmatSeq = (0 until aoiSeqFromPlan_mm.size).map(i =>
      new VMAT(
        vmatPK = None,
        outputPK,
        SOPInstanceUID = Util.sopOfAl(alMlc),
        SOPInstanceUIDOpen = Util.sopOfAl(alOpen),
        beamName = beamNameMlc,
        beamNameOpen = beamNameOpen,
        doseMLC_cu = mlcAvgSeq(i),
        doseOpen_cu = openAvgSeq(i),
        beamAverage_pct,
        topRtplan_mm = aoiSeqFromPlan_mm(i).top,
        bottomRtplan_mm = aoiSeqFromPlan_mm(i).bottom,
        leftRtplan_mm = aoiSeqFromPlan_mm(i).left,
        rightRtplan_mm = aoiSeqFromPlan_mm(i).right,
        topAOI_mm = measuredSeq_mm(i).top,
        bottomAOI_mm = measuredSeq_mm(i).bottom,
        leftAOI_mm = measuredSeq_mm(i).left,
        rightAOI_mm = measuredSeq_mm(i).right))
    //Trace.trace("vmat seq:\n    " + vmatSeq.mkString("\n    ")) // TODO : this has not been tested.
    vmatSeq
  }

  /**
   * Hook for testing only.
   */
  def testAnalyze(beamNameMlc: String, beamNameOpen: String,
    alMlc: AttributeList, alOpen: AttributeList,
    plan: AttributeList, collimatorCentering: CollimatorCentering,
    outputPK: Long, mlcImage: DicomImage, openImage: DicomImage): Seq[VMAT] = analyze(beamNameMlc, beamNameOpen, alMlc, alOpen, plan, collimatorCentering, outputPK, mlcImage, openImage)

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
