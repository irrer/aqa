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

package org.aqa.webrun.phase2.vmat

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.db.VMAT
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import scala.xml.Elem

object VMATAnalysis extends Logging {

  private def getPlanAoiList(beamPair: Config.VMATBeamPair, plan: AttributeList): Seq[MeasureTBLREdges.TBLR] = {

    val beamSeq = Phase2Util.getBeamSequenceOfPlan(beamPair.MLC, plan)

    val beamLimitList = DicomUtil.findAllSingle(beamSeq, TagByName.BeamLimitingDevicePositionSequence).map(bdps => bdps.asInstanceOf[SequenceAttribute]).flatMap(bdps => DicomUtil.alOfSeq(bdps))

    /**
      * Determine if the limits are of interest, meaning that they are
      * MLC leaves oriented in the X direction and that the gap between
      * them is at least 5 mm.
      */
    def mlcOfInterest(bl: AttributeList): Option[(Double, Double)] = {
      def isMLC = {
        val t = bl.get(TagByName.RTBeamLimitingDeviceType)
        (t != null) && t.getSingleStringValueOrEmptyString.toUpperCase.contains("MLCX")
      }

      val ljp = bl.get(TagByName.LeafJawPositions).getDoubleValues.distinct
      val max = ljp.max
      val min = ljp.min

      def isWideEnough = (max - min) > 5 // must be greater than 5 mm

      if (isMLC && isWideEnough)
        Some((min, max))
      else None
    }

    case class MinMax(min: Double, max: Double) {}

    /**
      * The limits of the open field.  This limits the range of the MLC field.  Both fields should
      * have the same range, but there have been instances where they did not.
      */
    val openLimits: MinMax = {
      val beamSeqOpen = Phase2Util.getBeamSequenceOfPlan(beamPair.OPEN, plan)
      val beamLimitListOpen =
        DicomUtil.findAllSingle(beamSeqOpen, TagByName.BeamLimitingDevicePositionSequence).map(bdps => bdps.asInstanceOf[SequenceAttribute]).flatMap(bdps => DicomUtil.alOfSeq(bdps))
      val xSeq = beamLimitListOpen.filter(bl => bl.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.toUpperCase.endsWith("X")).head
      val xLimits = xSeq.get(TagByName.LeafJawPositions).getDoubleValues
      MinMax(xLimits(0), xLimits(1))
    }

    // list of low-high pairs specifying X limits.  For T2 it is simple, just get all pairs.  For T3, it is necessary to
    // filter out and use only those X values that repeat, then pair them up by determining the different contiguous
    // regions they define.  This code works with the current RTPLAN that is deployed for Phase 2, but may not work for
    // other variations on VMAT.
    val xLimitList = {
      val list = beamLimitList
        .flatMap(bl => mlcOfInterest(bl))
        .distinct
        .sortBy(minMax => minMax._1)
        .map(minMax => MinMax(minMax._1, minMax._2))
        .filter(mm => (mm.min >= openLimits.min) && (mm.max <= openLimits.max))

      def grp(seq: Seq[Double]) = seq.groupBy(v => v).toList.sortBy(g => g._2.size).reverse
      val lo = grp(list.map(lh => lh.min))
      val hi = grp(list.map(lh => lh.max))

      if ((lo.head._2.size > 1) && (hi.head._2.size > 1)) {
        def onlyRepeatsOf(seq: Seq[(Double, Seq[Double])]) = seq.filter(v => v._2.size > 1).map(v => v._1).distinct.sorted

        val loG = onlyRepeatsOf(lo)
        val hiG = onlyRepeatsOf(hi)

        val pairList = loG.indices.map(i => MinMax(loG(i), hiG(i)))
        pairList
      } else list
    }

    // single low-high pair specifying Y limits
    val yLimits = {
      def isJawY(bl: AttributeList) = bl.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrEmptyString.trim.equalsIgnoreCase("Y")
      val jawPos = beamLimitList.filter(bl => isJawY(bl)).head
      val pair = jawPos.get(TagByName.LeafJawPositions).getDoubleValues.sorted
      MinMax(pair(0), pair(1))
    }

    val aoiList = xLimitList.map(x => MeasureTBLREdges.TBLR(yLimits.min, yLimits.max, x.min, x.max))
    aoiList
  }

  /** For testing only. */
  def testGetPlanAoiList(beamPair: Config.VMATBeamPair, plan: AttributeList): Seq[MeasureTBLREdges.TBLR] =
    getPlanAoiList(beamPair, plan)

  /**
    * Top level analysis for a single pair of beams.
    */
  private def analyze(
      beamPair: Config.VMATBeamPair,
      alMlc: AttributeList,
      alOpen: AttributeList,
      plan: AttributeList,
      collimatorCentering: CollimatorCentering,
      outputPK: Long,
      mlcImage: DicomImage,
      openImage: DicomImage
  ): Seq[VMAT] = {
    val aoiSeqFromPlan_mm = getPlanAoiList(beamPair, plan)
    val translator = new IsoImagePlaneTranslator(alMlc)

    /**
      * Given a TBLR in RTPLAN coordinates, convert it to a rectangle in the
      * isoplane that should be used to take the measurements.
      */
    def planToMeasured(tblr: MeasureTBLREdges.TBLR): MeasureTBLREdges.TBLR = {
      tblr.addOffset(collimatorCentering.center).resize(-beamPair.IsolationBorder_mm.abs)
    }

    val measuredSeq_mm = aoiSeqFromPlan_mm.map(tblr => planToMeasured(tblr))
    val pixSeq_pix = measuredSeq_mm.map(tblr => tblr.iso2Pix(translator))

    val mlcAvgSeq = {
      val pixValSeq = pixSeq_pix.map(p => mlcImage.averageOfRectangle(p.toRectangle))
      Phase2Util.pixToDose(pixValSeq, alMlc)
    }
    val openAvgSeq = {
      val pixValSeq = pixSeq_pix.map(p => openImage.averageOfRectangle(p.toRectangle))
      Phase2Util.pixToDose(pixValSeq, alOpen)
    }

    logger.info("mlc avg of rectangles: " + (mlcAvgSeq.sum / mlcAvgSeq.size) + "    open avg of rectangles: " + (openAvgSeq.sum / openAvgSeq.size))

    // divide each pixel in MLC to corresponding pixel in OPEN to create a new image.
    val ratioOfImages = {
      val mlcSlope = alMlc.get(TagFromName.RescaleSlope).getDoubleValues.head
      val mlcOffset = alMlc.get(TagFromName.RescaleIntercept).getDoubleValues.head
      val openSlope = alOpen.get(TagFromName.RescaleSlope).getDoubleValues.head
      val openOffset = alOpen.get(TagFromName.RescaleIntercept).getDoubleValues.head

      /**
        * Get the value for one pixel in the final array.  Convert each to CU, then divide MLC / OPEN.
        */
      def ratio(x: Int, y: Int) = {
        val m = (mlcImage.get(x, y) * mlcSlope) + mlcOffset
        val o = (openImage.get(x, y) * openSlope) + openOffset
        if (o == 0) 0 else (m / o).toFloat
      }

      val pixArray = for (y <- 0 until mlcImage.height) yield {
        for (x <- 0 until mlcImage.width) yield {
          ratio(x, y)
        }
      }

      new DicomImage(pixArray)
    }

    val pctSeqOld = mlcAvgSeq.indices.map(i => (mlcAvgSeq(i) * 100) / openAvgSeq(i))
    val pctSeq = pixSeq_pix.map(p => ratioOfImages.averageOfRectangle(p.toRectangle) * 100)

    // log a comparison of the old way verses the new way.
    if (true) {
      // Note: old way was to divide the average of one rectangle by another.  The new way to divide each
      // pixel in one image by its counterpart in the other and then take the average.
      def toText(dSeq: Seq[Double]) = dSeq.map(p => p.formatted("%12.8f")).mkString("    ")

      println("VMAT comparison " + beamPair)
      println("old:  " + toText(pctSeqOld))
      println("new:  " + toText(pctSeq))
      val diff = pctSeq.indices.map(i => pctSeq(i) - pctSeqOld(i))
      println("diff: " + toText(diff))
    }

    val beamAverage_pct = pctSeq.sum / pctSeq.size
    val statusSeq = pctSeq.map(pct => if ((pct - beamAverage_pct).abs >= Config.VMATDeviationThreshold_pct) ProcedureStatus.fail else ProcedureStatus.pass)

    val vmatSeq = aoiSeqFromPlan_mm.indices.map(i =>
      new VMAT(
        vmatPK = None,
        outputPK,
        status = statusSeq(i).toString(),
        SOPInstanceUIDMLC = Util.sopOfAl(alMlc),
        SOPInstanceUIDOpen = Util.sopOfAl(alOpen),
        beamNameMLC = beamPair.MLC,
        beamNameOpen = beamPair.OPEN,
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
        rightAOI_mm = measuredSeq_mm(i).right
      )
    )
    vmatSeq
  }

  /**
    * Hook for testing only.
    */
  def testAnalyze(
      beamPair: Config.VMATBeamPair,
      alMlc: AttributeList,
      alOpen: AttributeList,
      plan: AttributeList,
      collimatorCentering: CollimatorCentering,
      outputPK: Long,
      mlcImage: DicomImage,
      openImage: DicomImage
  ): Seq[VMAT] = analyze(beamPair, alMlc, alOpen, plan, collimatorCentering, outputPK, mlcImage, openImage)

  private val subProcedureName = "VMAT"

  case class VMATResult(summry: Elem, stats: ProcedureStatus.Value, resultList: Seq[VMAT]) extends SubProcedureResult(summry, stats, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, VMATResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of " + subProcedureName + " for machine " + extendedData.machine.id)

      val vmatListList = Config.VMATBeamPairList
        .map(vmatPair => {
          if (runReq.rtimageMap.contains(vmatPair.MLC) && runReq.rtimageMap.contains(vmatPair.OPEN))
            analyze(
              vmatPair,
              runReq.derivedMap(vmatPair.MLC).attributeList,
              runReq.derivedMap(vmatPair.OPEN).attributeList,
              runReq.rtplan,
              collimatorCenteringResource.collimatorCenteringOfBeam(vmatPair.OPEN),
              extendedData.output.outputPK.get,
              runReq.derivedMap(vmatPair.MLC).originalImage,
              runReq.derivedMap(vmatPair.OPEN).originalImage
            )
          else Seq[VMAT]()
        })
        .filter(l => l.nonEmpty)

      val status: ProcedureStatus.Value = {
        if (vmatListList.isEmpty || vmatListList.map(vmatList => VMAT.beamPassed(vmatList)).reduce(_ && _)) ProcedureStatus.pass else ProcedureStatus.fail
      }

      val summary = VMATHTML.makeDisplay(extendedData, runReq, vmatListList, status)
      val result = Right(VMATResult(summary, status, vmatListList.flatten))
      logger.info("Putting " + vmatListList.flatten.size + " rows of " + subProcedureName + " data in database.")

      // put data in database
      vmatListList.flatten.map(vmat => vmat.insert)
      logger.info("Finished analysis of " + subProcedureName + " for machine " + extendedData.machine.id)
      result
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of CenterDose: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }

}
