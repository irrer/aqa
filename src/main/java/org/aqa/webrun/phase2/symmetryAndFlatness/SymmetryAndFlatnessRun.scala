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

package org.aqa.webrun.phase2.symmetryAndFlatness

import com.pixelmed.dicom.AttributeList
import edu.umro.ImageUtil.DicomImage
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Procedure
import org.aqa.db.PSMBeam
import org.aqa.db.SymmetryAndFlatness
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.psm.PSMGrid

import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.sql.Timestamp
import scala.xml.Elem

/**
  * Analyze DICOM files for symmetry and flatness.
  */
object SymmetryAndFlatnessRun extends Logging {

  private def boolToStatus(pass: Boolean) = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

  val axialSymmetryName = "Axial Symmetry"
  val transverseSymmetryName = "Transverse Symmetry"
  val flatnessName = "Flatness"
  val profileConstancyName = "Profile Constancy"

  /**
    * Encapsulate data for generating a report.
    */
  case class SymmetryAndFlatnessBeamResult(
      symmetryAndFlatness: SymmetryAndFlatness,
      annotatedImage: BufferedImage,
      transverseProfile: Seq[Double],
      transverse_pct: IndexedSeq[Double],
      axialProfile: Seq[Double],
      axial_pct: IndexedSeq[Double],
      baseline: SymmetryAndFlatness,
      psmGrid: Option[PSMGrid],
      psmGridBaseline: Option[PSMGrid]
  ) {}

  private def getAttributeList(beamName: String, runReq: RunReq): AttributeList = {
    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)
    if (isFlood && runReq.flood.isDefined) runReq.flood.get
    else runReq.derivedMap(beamName).attributeList
  }

  def makeBaselineName(beamName: String, dataName: String): String = dataName + " " + beamName

  /**
    * Entry point for testing only.
    *
    * @param beamName         Name of beam.
    * @param machinePK        Machine being processed.
    * @param dataDate         Date that data was acquired at the machine.
    * @param attributeList    Image and metadata.
    * @param correctedImage   Image with bad pixels fixed.
    * @param collimatorCenter Collimator center offset.
    * @return
    */
  def testAnalyze(beamName: String, machinePK: Long, dataDate: Timestamp, attributeList: AttributeList, correctedImage: DicomImage, collimatorCenter: Point2D.Double): SymmetryAndFlatnessBeamResult = {

    val symmetryAndFlatnessAnalyze = SymmetryAndFlatnessAnalyze( //
      outputPK = -1,
      procedurePK = Procedure.ProcOfPhase2.get.procedurePK.get,
      machinePK = machinePK,
      dataDate = dataDate,
      attributeList,
      beamName,
      collimatorCenter,
      floodField = None,
      symmetryAndFlatnessBaselineRedoBeamList = Seq()
    )

    symmetryAndFlatnessAnalyze.analyze( //
      correctedImage = correctedImage,
      psmGrid = None
    )

  }

  /**
    * Put the results in the database.
    *
    * @param resultList List of new results.
    */
  private def storeResultsInDb(resultList: List[SymmetryAndFlatnessBeamResult]): Unit = {
    resultList.foreach(r => r.symmetryAndFlatness.insertOrUpdate())
    logger.info("Stored " + resultList.size + " SymmetryAndFlatness records")
  }

  val subProcedureName = "SymmetryAndFlatness"

  class SymmetryAndFlatnessResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
    * Run the CollimatorPosition sub-procedure, save results in the database, return right for proper execution or left for crash.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, SymmetryAndFlatnessResult] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness for machine " + extendedData.machine.id)

      // val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))
      val beamNameList = Util.makeSymFlatConstBeamNameList(runReq.rtplan).filter(beamName => runReq.derivedMap.contains(beamName))
      logger.info("Sym+Flat using beams:\n    " + beamNameList.mkString("\n    "))

      val beamSet = {
        val list = PSMBeam.historyByMachine(extendedData.machine.machinePK.get)

        def qualifies(p: PSMBeam.PSMBeamHistory): Boolean = {
          p.matchesResolution(runReq.rtimageMap.values.head) &&
          (p.output.dataDate.get.getTime < extendedData.output.dataDate.get.getTime)
        }

        list.filter(qualifies).lastOption
      }

      val psmGrid = beamSet.map(bs => PSMGrid.makePSMGrid(bs.psmBeamList))

      if (psmGrid.isEmpty)
        logger.info("No PSM (Pixel Sensitivity Matrix) available.")
      else
        logger.info("Using (Pixel Sensitivity Matrix) PSM from: " + beamSet.get.output.dataDate.get)

      def doBeam(beamName: String): Seq[SymmetryAndFlatnessBeamResult] = {

        val attributeList = getAttributeList(beamName, runReq)
        val symmetryAndFlatnessAnalyze = SymmetryAndFlatnessAnalyze( //
          extendedData.output.outputPK.get,
          extendedData.output.procedurePK,
          extendedData.machine.machinePK.get,
          extendedData.output.dataDate.get,
          attributeList,
          beamName,
          collimatorCenteringResource.centerOfBeam(beamName),
          floodField = runReq.flood,
          runReq.symmetryAndFlatnessBaselineRedoBeamList
        )

        def doAnalyze(psmGrid: Option[PSMGrid]): SymmetryAndFlatnessBeamResult = {

          val image: DicomImage = {
            if (psmGrid.isDefined)
              runReq.derivedMap(beamName).originalImage
            else
              runReq.derivedMap(beamName).pixelCorrectedImage
          }
          symmetryAndFlatnessAnalyze.analyze(image, psmGrid = psmGrid)
        }

        val noPsm = Some(doAnalyze(psmGrid = None))

        val withPsm =
          if (psmGrid.isEmpty)
            None
          else
            Some(doAnalyze(psmGrid = psmGrid))

        Seq(noPsm, withPsm).flatten
      }

      // only process beams that are both configured and have been uploaded
      val resultList: List[SymmetryAndFlatnessBeamResult] = beamNameList.par.flatMap(doBeam).toList

      def showIt(r: SymmetryAndFlatnessBeamResult): String = {
        val bs = r.baseline
        val sf = r.symmetryAndFlatness
        val text = {
          "    " + sf.beamName.format("%16s") + " : " +
            "    axial sym:" + sf.axialSymmetryPass(bs, r.psmGrid, r.psmGridBaseline).toString.format("%5s") + " : " +
            "    flatness:" + sf.flatnessPass(bs, r.psmGrid, r.psmGridBaseline).toString.format("%5s") + " : " +
            "    transverse sym:" + sf.transverseSymmetryPass(bs, r.psmGrid, r.psmGridBaseline).toString.format("%5s") + " : " +
            "    profile const:" + sf.profileConstancyPass(bs, r.psmGrid, r.psmGridBaseline).toString.format("%5s") + " : " +
            "    all: " + sf.allPass(bs, r.psmGrid, r.psmGridBaseline).toString.format("%5s")
        }
        text
      }

      logger.info("\n" + resultList.map(r => showIt(r)).mkString("\n"))

      //val pass = resultList.map(r => r.symmetryAndFlatness.allPass(r.baseline)).reduce(_ && _)
      val pass = {
        val list = resultList.map(r => r.symmetryAndFlatness.allPass(r.baseline, r.psmGrid, r.psmGridBaseline))
        list.nonEmpty && list.reduce(_ && _)
      }
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      storeResultsInDb(resultList)

      val summary = SymmetryAndFlatnessHTML.makeDisplay(extendedData, resultList, boolToStatus(pass), runReq)

      val result = new SymmetryAndFlatnessResult(summary, status)
      logger.info("Finished analysis of SymmetryAndFlatness for machine " + extendedData.machine.id)
      if (pass) Right(result) else Left(result.summary)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }
}
