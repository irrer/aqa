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

package org.aqa.webrun.phase2.wedge

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Baseline
import org.aqa.db.WedgePoint
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.phase2.SubProcedureResult

import scala.xml.Elem

/**
  * Analyze DICOM files for Wedge Analysis.
  */
object WedgeAnalysis extends Logging {

  def wedgeOrientationTransverse(beamName: String, plan: AttributeList): Boolean = {
    val bs = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val WedgeOrientation = DicomUtil.seqToAttr(bs, TagByName.WedgeSequence).head.get(TagByName.WedgeOrientation).getDoubleValues.head
    val horizontal = Util.angleRoundedTo90(WedgeOrientation) match {
      case 0   => true
      case 90  => false
      case 180 => true
      case 270 => false
    }
    horizontal
  }

  def makeWedgeBaselineName(wedgeBeamName: String, backgroundBeamName: String): String = "Wedge " + wedgeBeamName + " / " + backgroundBeamName

  def makeWedgeBaselineName(wedgePair: WedgePair): String = makeWedgeBaselineName(wedgePair.beamName, wedgePair.backgroundBeamName)

  def makeWedgeBaselineName(wedgePoint: WedgePoint): String = makeWedgeBaselineName(wedgePoint.wedgeBeamName, wedgePoint.backgroundBeamName)

  private case class WedgePair(beamName: String, backgroundBeamName: String)

  private def analyzeWedgePair(wedgePair: WedgePair, collimatorCenteringResource: CollimatorCenteringResource, extendedData: ExtendedData, runReq: RunReq): WedgePoint = {
    logger.info("Starting individual wedge analysis of " + wedgePair.beamName + " with background " + wedgePair.backgroundBeamName)

    def measure(beamName: String) = {
      val derived = runReq.derivedMap(beamName)
      Phase2Util.measureDose(collimatorCenteringResource.centerPointListOfBeam(beamName), derived.originalImage, derived.attributeList)
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

    val wedgePoint = new WedgePoint(
      None,
      extendedData.output.outputPK.get,
      Util.sopOfAl(derivedWedge.attributeList),
      wedgePair.beamName,
      // isBaseline_text = false.toString,
      isBaseline = false,
      wedgeDose,
      Util.sopOfAl(derivedBackground.attributeList),
      wedgePair.backgroundBeamName,
      backgroundDose,
      percent,
      baselineValue
    )

    logger.info("Finished individual wedge analysis of " + wedgePair.beamName + " with background " + wedgePair.backgroundBeamName + "\nResult:\n" + wedgePoint)
    wedgePoint
  }

  val subProcedureName = "Wedge"

  /**
    * Use the center dose and flood CenterDose points to calculate the WedgePoints.
    */
  private def analyze(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Seq[WedgePoint] = {

    val beamList = runReq.derivedMap.keys.toList

    def findBeamPair(wedgeBeamName: String): Option[WedgePair] = {
      Config.WedgeBeamList.find(w => w.wedge.equalsIgnoreCase(wedgeBeamName)) match {
        case Some(wedgeBeam) =>
          val bg = wedgeBeam.backgroundList.flatMap(bg => beamList.find(c => c.equalsIgnoreCase(bg)))
          if (bg.isEmpty)
            None // no background beam
          else {
            Some(WedgePair(wedgeBeamName, bg.head)) // use the first one in the list of allowable background beams
          }
        case _ => None // wedge beam name not in input list
      }

    }

    val wedgePairList = beamList.flatMap(b => findBeamPair(b))
    wedgePairList.map(wp => analyzeWedgePair(wp, collimatorCenteringResource, extendedData, runReq))
  }

  /**
    * Restore the baseline status of a wedge point as necessary.
    * @param wp Wedge point to update.
    * @param baselineList List of beam+background beam names.
    * @return List of wedge points, possibly modified.
    */
  private def restoreBaseline(wp: WedgePoint, baselineList: Seq[String]): WedgePoint = {
    val id = wp.wedgeBeamName + wp.backgroundBeamName
    if (baselineList.contains(id))
      wp.copy(isBaseline = true)
    else wp
  }

  /**
    * Determine if all the values passed.  If so, return true.  Also return true if there are no values.
    *
    * @param wedgePointList List of newly analyzed values.
    * @param machinePK Machine from which they came.
    * @return True if all passed.
    */
  private def allPass(wedgePointList: Seq[WedgePoint], machinePK: Long, procedurePK: Long): Boolean = {
    if (wedgePointList.isEmpty)
      true
    else {
      val history = WedgePoint.history(machinePK, procedurePK)
      val pkList = wedgePointList.map(_.wedgePointPK.get)

      def passes(hist: WedgePoint.WedgePointHistory): Boolean = {
        val pct = (100 * hist.wedgePoint.wedgeValue_cu) / hist.wedgePoint.backgroundValue_cu
        val baselinePct = (100 * hist.baselineWedgePoint.wedgeValue_cu) / hist.baselineWedgePoint.backgroundValue_cu
        (pct - baselinePct).abs <= Config.WedgeTolerance_pct
      }

      val allOk = history.filter(h => pkList.contains(h.wedgePoint.wedgePointPK.get)).map(passes).reduce(_ && _)

      allOk
    }
  }

  class WedgeResult(summary: Elem, status: ProcedureStatus.Value) extends SubProcedureResult(summary, status, subProcedureName)

  /**
    * Run the WedgeAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, WedgeResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName + " for machine " + extendedData.machine.id)

      val wedgePointList = {
        analyze(extendedData, runReq, collimatorCenteringResource). // analyse wedge beams
        map(wp => restoreBaseline(wp, runReq.wedgeBaselineRedoBeamList)). // restore baselines as needed
        map(wp => wp.insert) // insert into database, which also defines wedgePointPK
      }

      val pass = allPass(wedgePointList, extendedData.machine.machinePK.get, extendedData.output.procedurePK)

      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
      logger.info("Starting HTML generation for " + subProcedureName)
      val summary = WedgeHTML.makeDisplay(extendedData, status, runReq, wedgePointList, collimatorCenteringResource)
      logger.info("Finished HTML generation for " + subProcedureName)
      val result = new WedgeResult(summary, status)
      logger.info("Finished analysis of " + subProcedureName + " for machine " + extendedData.machine.id)
      Right(result)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of MetadataCheck: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }
  }
}
