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

package org.aqa.webrun.phase2.collimatorCentering

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.CollimatorCentering
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.MeasureTBLREdges
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import java.awt.Point
import java.awt.geom.Point2D
import scala.collection.parallel.ParSeq
import scala.xml.Elem

/**
  * Analyse DICOM files for ImageAnalysis.
  */
object CollimatorCenteringAnalysis extends Logging {

  val subProcedureName = "Collimator Centering"

  case class CollimatorCenteringResult(sum: Elem, sts: ProcedureStatus.Value, result: Seq[CollimatorCentering]) extends SubProcedureResult(sum, sts, subProcedureName)

  case class AnalysisResult(collimatorCentering: CollimatorCentering, measureTBLREdges090: MeasureTBLREdges.AnalysisResult, measureTBLREdges270: MeasureTBLREdges.AnalysisResult) {}

  /**
    * Perform actual analysis.
    */
  private def analyze(
      al090: AttributeList,
      al270: AttributeList,
      image090: DicomImage,
      image270: DicomImage,
      outputPK: Long,
      rtplan: AttributeList
  ): AnalysisResult = {

    val collAngle090 = Util.collimatorAngle(al090)
    val collAngle270 = Util.collimatorAngle(al270)
    val translator = new IsoImagePlaneTranslator(al090)

    val expected_mm090 = MeasureTBLREdges.imageCollimatorPositions(al090, rtplan).toTBLR(collAngle090)
    val expected_mm270 = MeasureTBLREdges.imageCollimatorPositions(al270, rtplan).toTBLR(collAngle270)

    // Calculate edges in parallel for efficiency.
    val resultPair = {
      val pointZero = new Point(0, 0)
      def m090 = MeasureTBLREdges.measure(image090, translator, Some(expected_mm090), collAngle090, image090, pointZero, 0.5)
      def m270 = MeasureTBLREdges.measure(image270, translator, Some(expected_mm270), collAngle270, image270, pointZero, 0.5)
      val rp = ParSeq(m090 _, m270 _).map(f => f()).toList
      rp
    }
    val result090 = resultPair.head
    val result270 = resultPair(1)

    val m090 = result090.measurementSet // in pixels
    val m270 = result270.measurementSet // in pixels

    val xy090 = m090.pix2iso(translator).toX1X2Y1Y2(Util.collimatorAngle(al090))
    val xy270 = m270.pix2iso(translator).toX1X2Y1Y2(Util.collimatorAngle(al270))

    // note that for collimator angles of 90 and 270, X1 and X2 are the horizontal edges, and Y1 and Y2 are the vertical edges
    val xCenter = (xy090.Y1 + xy090.Y2 + xy270.Y1 + xy270.Y2) / 4
    val yCenter = (xy090.X1 + xy090.X2 + xy270.X1 + xy270.X2) / 4

    val measuredCenterIso = new Point2D.Double(xCenter, yCenter) // place in isoplane that is measured by collimator centering
    val zero = new Point2D.Double(0, 0)

    val errDistance = zero.distance(measuredCenterIso)
    val pass: Boolean = errDistance <= Config.CollimatorCenteringTolerence_mm
    val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
    logger.info("CollimatorCentering error in mm: " + errDistance + "    Status: " + procedureStatus)

    val collimatorCentering = new CollimatorCentering(
      None,
      outputPK,
      procedureStatus.name,
      SOPInstanceUID090 = Util.sopOfAl(al090),
      SOPInstanceUID270 = Util.sopOfAl(al270),
      gantryAngleRounded_deg = Util.angleRoundedTo90(Util.gantryAngle(al090)),
      beamName090 = Phase2Util.getBeamNameOfRtimage(rtplan, al090).get,
      beamName270 = Phase2Util.getBeamNameOfRtimage(rtplan, al270).get,
      xCenter,
      yCenter, // xCollimatorCenter_mm, yCollimatorCenter_mm
      xy090.X1,
      xy090.X2,
      xy090.Y1,
      xy090.Y2,
      xy270.X1,
      xy270.X2,
      xy270.Y1,
      xy270.Y2
    )

    AnalysisResult(collimatorCentering, result090, result270)
  }

  /**
    * For testing only
    */
  def testAnalyze(
      al090: AttributeList,
      al270: AttributeList,
      image090: DicomImage,
      image270: DicomImage,
      outputPK: Long,
      rtplan: AttributeList
  ): AnalysisResult = {
    analyze(al090, al270, image090, image270, outputPK, rtplan)
  }

  private case class CenteringBeam(name: String, al: AttributeList) {
    val gantryAngle: Int = Util.angleRoundedTo90(Util.gantryAngle(al))
    val collimatorAngle: Int = Util.angleRoundedTo90(Util.collimatorAngle(al))
  }

  private class BeamPair(beamList: Seq[CenteringBeam]) {
    val beam090: CenteringBeam = beamList.filter(_.collimatorAngle == 90).head
    val beam270: CenteringBeam = beamList.filter(_.collimatorAngle == 270).head
  }

  /**
    * Get a list of collimator centering beam pairs.
    * @return A list of collimator centering beam pairs.
    */
  private def getBeamPairList(runReq: RunReq): Seq[BeamPair] = {

    /**
      * Return list of beam pairs if all of the names are present.
      * @param nameList List of required beam names.
      * @return List if they are all present, empty list if one or more are missing.
      */
    def group(nameList: Seq[String]): Seq[BeamPair] = {
      val list = runReq.rtimageMap.keys.filter(nameList.contains)
      if (list.size == nameList.size) {
        val groupList = nameList.map(beamName => CenteringBeam(beamName, runReq.rtimageMap(beamName))).groupBy(_.gantryAngle).values.map(pair => new BeamPair(pair))
        groupList.toSeq
      } else
        Seq()
    }

    val phase3List = group(Config.collimatorCenteringPhase3List)
    val phase2List = group(Config.collimatorCenteringPhase2List)

    (phase3List, phase2List) match {
      case (list, _) => list // match for Phase3
      case (_, list) => list // match for Phase2
      case _         => Seq() // could not find complete list for either Phase2 or Phase3
    }
  }

  private def processBeamPair(extendedData: ExtendedData, runReq: RunReq, beamPair: BeamPair): AnalysisResult = {

    val anlRes = analyze(
      runReq.rtimageMap(Config.CollimatorCentering090BeamName),
      runReq.rtimageMap(Config.CollimatorCentering270BeamName),
      runReq.derivedMap(beamPair.beam090.name).pixelCorrectedImage,
      runReq.derivedMap(beamPair.beam270.name).pixelCorrectedImage,
      extendedData.output.outputPK.get,
      runReq.rtplan
    )

    anlRes
  }

  /**
    * Run the CollimatorCentering sub-procedure, save results in the database, return true for pass or false for fail.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, CollimatorCenteringResult] = {
    try {
      logger.info("Starting analysis of CollimatorCentering for machine " + extendedData.machine.id)

      val beamPairList = getBeamPairList(runReq).sortBy(_.beam090.gantryAngle)
      if (beamPairList.isEmpty) {
        Left(<div>CollimatorCentering beams are missing.</div>)
      } else {

        val resultList = beamPairList.map(pair => processBeamPair(extendedData, runReq, pair))

        val procedureStatus = {
          if (resultList.map(_.collimatorCentering.status.toString).distinct.contains(ProcedureStatus.fail.toString()))
            ProcedureStatus.fail
          else
            ProcedureStatus.pass
        }

        // put data in the database
        for (r <- resultList) {
          logger.info("Inserting CollimatorCentering row: " + r.collimatorCentering)
          r.collimatorCentering.insert
        }

        val elem = CollimatorCenteringHTML.makeDisplay(extendedData, procedureStatus, resultList, runReq)
        logger.info("Finished processing for " + subProcedureName)
        val result = Right(CollimatorCenteringResult(elem, procedureStatus, resultList.map(_.collimatorCentering)))
        logger.info("Finished analysis of CollimatorCentering for machine " + extendedData.machine.id)
        result
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of CollimatorCentering: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }
}
