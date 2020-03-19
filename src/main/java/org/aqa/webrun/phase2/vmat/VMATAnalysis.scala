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

    // list of low-high pairs specifying X limits.  For T2 it is simple, just get all pairs.  For T3, it is necessary to
    // filter out and use only those X values that repeat, then pair them up by determining the different contiguous
    // regions they define.  This code works with the current RTPLAN that is deployed for Phase 2, but may not work for
    // other variations on VMAT.
    val xLimitList = {
      val list = beamLimitList.map(bl => mlcOfInterest(bl)).flatten.distinct.sortBy(minMax => minMax._1).map(minMax => new MinMax(minMax._1, minMax._2))
      def grp(seq: Seq[Double]) = seq.groupBy(v => v).toList.sortBy(g => g._2.size).reverse
      val lo = grp(list.map(lh => lh.min))
      val hi = grp(list.map(lh => lh.max))

      if ((lo.head._2.size > 1) && (hi.head._2.size > 1)) {
        def onlyRepeatsOf(seq: Seq[(Double, Seq[Double])]) = seq.filter(v => v._2.size > 1).map(v => v._1).distinct.sorted

        val loG = onlyRepeatsOf(lo)
        val hiG = onlyRepeatsOf(hi)

        val pairList = (0 until loG.size).map(i => new MinMax(loG(i), hiG(i)))
        pairList
      } else list
    }

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
      tblr.addOffset(collimatorCentering.center).resize(-Config.VMATPenumbraBorderThickness_mm)
    }

    val measuredSeq_mm = aoiSeqFromPlan_mm.map(tblr => planToMeasured(tblr))
    val pixSeq_pix = measuredSeq_mm.map(tblr => tblr.iso2Pix(translator))

    Trace.trace("Getting rectangle averages of mlc  mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm")
    val mlcAvgSeq = {
      val pixValSeq = pixSeq_pix.map(p => mlcImage.averageOfRectangle(p.toRectangle))
      Phase2Util.pixToDose(pixValSeq, alMlc)
    }
    Trace.trace("Getting rectangle averages of open ooooooooooooooooooooooooooooooooo")
    val openAvgSeq = {
      val pixValSeq = pixSeq_pix.map(p => openImage.averageOfRectangle(p.toRectangle))
      Phase2Util.pixToDose(pixValSeq, alOpen)
    }

    Trace.trace("mlc avg of rects: " + (mlcAvgSeq.sum / mlcAvgSeq.size) + "    open avg of rects: " + (openAvgSeq.sum / openAvgSeq.size))

    val pctSeq = (0 until mlcAvgSeq.size).map(i => (mlcAvgSeq(i) * 100) / openAvgSeq(i))
    val beamAverage_pct = pctSeq.sum / pctSeq.size
    val diffPctSeq = pctSeq.map(p => p - beamAverage_pct)
    val statusSeq = pctSeq.map(pct => if ((pct - beamAverage_pct).abs >= Config.VMATDeviationThreshold_pct) ProcedureStatus.fail else ProcedureStatus.pass)

    val vmatSeq = (0 until aoiSeqFromPlan_mm.size).map(i =>
      new VMAT(
        vmatPK = None,
        outputPK,
        status = statusSeq(i).toString(),
        SOPInstanceUIDMLC = Util.sopOfAl(alMlc),
        SOPInstanceUIDOpen = Util.sopOfAl(alOpen),
        beamNameMLC = beamNameMlc,
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

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, VMATResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of " + subProcedureName)

      val vmatListList = Config.VMATBeamPairList.map(vmatPair => {
        if (runReq.rtimageMap.contains(vmatPair.mlc) && runReq.rtimageMap.contains(vmatPair.open))
          analyze(vmatPair.mlc, vmatPair.open,
            runReq.derivedMap(vmatPair.mlc).attributeList, runReq.derivedMap(vmatPair.open).attributeList,
            runReq.rtplan.attributeList.get, collimatorCentering, extendedData.output.outputPK.get,
            runReq.derivedMap(vmatPair.mlc).originalImage, runReq.derivedMap(vmatPair.open).originalImage)
        else Seq[VMAT]()
      }).filter(l => l.nonEmpty)

      Trace.trace("vmatListList.size: " + vmatListList.size)

      val status: ProcedureStatus.Value = {
        val j = vmatListList.map(vmatList => VMAT.beamPassed(vmatList)).reduce(_ && _)   // TODO rm
        if (vmatListList.isEmpty || vmatListList.map(vmatList => VMAT.beamPassed(vmatList)).reduce(_ && _)) ProcedureStatus.pass else ProcedureStatus.fail
      }

      val summary = VMATHTML.makeDisplay(extendedData, runReq, vmatListList, status)
      val result = Right(new VMATResult(summary, status, vmatListList.flatten))
      logger.info("Putting " + vmatListList.flatten.size + " rows of " + subProcedureName + " data in database.")

      // put data in database
      vmatListList.flatten.map(vmat => vmat.insert)
      logger.info("Finished analysis of " + subProcedureName)
      result
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }

}
