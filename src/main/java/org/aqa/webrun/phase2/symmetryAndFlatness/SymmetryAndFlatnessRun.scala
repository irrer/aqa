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
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Config
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.FloodField
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
import java.util.Date
import scala.xml.Elem

/**
  * Analyze DICOM files for symmetry and flatness.
  */
object SymmetryAndFlatnessRun extends Logging {

  private def boolToStatus(pass: Boolean) = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

  val axialSymmetryNameNoPSM = "Axial Symmetry No PSM"
  val transverseSymmetryNameNoPSM = "Transverse Symmetry No PSM"
  val flatnessNameNoPSM = "Flatness No PSM"
  val profileConstancyNameNoPSM = "Profile Constancy No PSM"

  val axialSymmetryNameWithPSM = "Axial Symmetry With PSM"
  val transverseSymmetryNameWithPSM = "Transverse Symmetry With PSM"
  val flatnessNameWithPSM = "Flatness With PSM"
  val profileConstancyNameWithPSM = "Profile Constancy With PSM"

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
      symmetryAndFlatnessBaselineRedoBeamList = Seq(),
      isFlatteningFilterFree = false
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

  private case class FloodFieldCriteria(
      Rows: Int, // Number of rows in the image.  DICOM metadata 0028,0010
      Columns: Int, // Number of columns in the image.  DICOM metadata 0028,0011
      ImagePlanePixelSpacingX: Double, // Physical distance (in mm) between the center of each image pixel in the X axis.  DICOM metadata 3002,0011 first value
      ImagePlanePixelSpacingY: Double, // Physical distance (in mm) between the center of each image pixel in the Y axis.  DICOM metadata 3002,0011 second value
      KVP: Double, // energy level
      FlatteningFilterFree: Boolean // true if this is an FFF (flattening filter free)
  ) {

    def this(rtimage: AttributeList) = this(???, ???, ???, ???, ???, ???)

    def matchesFloodField(ff: FloodField): Boolean = {
      ???
    }
  }

  private def findMatchingFloodFields(psmDate: Date, rtImageList: Seq[AttributeList]): Seq[FloodField] = {
    //
    ???
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

      val psmBeamSet = {
        val list = PSMBeam.historyByMachine(extendedData.machine.machinePK.get)

        def qualifies(p: PSMBeam.PSMBeamHistory): Boolean = {
          p.matchesResolution(runReq.rtimageMap.values.head) &&
          (p.output.dataDate.get.getTime < extendedData.output.dataDate.get.getTime)
        }

        list.filter(qualifies).lastOption
      }

      val psmGrid = psmBeamSet.map(bs => PSMGrid.makePSMGrid(bs.psmBeamList))

      /**
        * @param KVP energy in KV
        * @param isFFF True if this is an FFF beam.
        */
      case class FloodFieldType(KVP: Double, isFFF: Boolean) {
        override def toString: String = s"kvp: $KVP    isFFF: $isFFF"
      }

      def makeFloodFieldType(beamName: String): FloodFieldType = {
        val kvp = DicomUtil.findAllTag(runReq.rtimageMap(beamName), TagByName.KVP).head.getDoubleValues.head
        val isFFF = {
          val planBeam = Util.getBeamOfRtimage(runReq.rtplan, runReq.rtimageMap(beamName)).head
          Util.isFFF(planBeam)
        }
        val fft = FloodFieldType(kvp, isFFF)
        fft
      }

      /**
        * Find all flood fields that the sym/flat beams need.  They must:
        * <ul>
        * <li>have the same energy (KVP) </li>
        * <li>the same energy fluence mode (FFF vs standard) </li>
        * <li>be the most recent with those KVP and fluence </li>
        * <li>be captured prior to the PSM </li>
        * </ul>
        */
      val floodFieldList: Seq[FloodField] = {
        if (psmBeamSet.isDefined) {

          /** Make a list of the types of flood fields needed for the sym/flat beams that we have. */
          val neededFloodFieldTypes = {
            val all = beamNameList.map(makeFloodFieldType)
            val distinctList = all.groupBy(_.toString).values.map(_.head)
            distinctList
          }

          val maxDate = new Timestamp(psmBeamSet.head.output.dataDate.get.getTime)
          val minDate = new Timestamp(maxDate.getTime - Config.PSMMaxFloodFieldAge_ms)

          /**
            * Given a type of flood field, try to get the actual flood field.
            * @param ffType Type of flood field.
            * @return Flood field, or, None if not available.
            */
          def getFloodField(ffType: FloodFieldType): Option[FloodField] = {
            val list = FloodField.getMatching( //
              machinePK = extendedData.machine.machinePK.get,
              kvp = ffType.KVP,
              fff = ffType.isFFF,
              minDate = minDate,
              maxDate = maxDate
            )
            list.lastOption
          }

          val ffList = neededFloodFieldTypes.flatMap(getFloodField).toSeq
          ffList
        } else {
          // there is no PSM available, so there is no point in looking for flood fields
          Seq()
        }
      }

      if (psmGrid.isEmpty)
        logger.info("No PSM (Pixel Sensitivity Matrix) available.")
      else
        logger.info("Using (Pixel Sensitivity Matrix) PSM from: " + psmBeamSet.get.output.dataDate.get)

      /**
        * Process a single beam.  If a PSM and Flood Field are available, then process it both with and without the PSM.
        * @param beamName Name of beam to process
        * @return
        */
      def doBeam(beamName: String): Seq[SymmetryAndFlatnessBeamResult] = {

        val attributeList = getAttributeList(beamName, runReq)
        val floodField = floodFieldList.find(ff => {
          val fft = makeFloodFieldType(beamName)
          (ff.FlatteningFilterFree == fft.isFFF) && (ff.KVP == fft.KVP)
        })

        def doAnalyze(psmGrid: Option[PSMGrid]): SymmetryAndFlatnessBeamResult = {

          val image: DicomImage = {
            if (psmGrid.isDefined)
              runReq.derivedMap(beamName).originalImage
            else
              runReq.derivedMap(beamName).pixelCorrectedImage
          }

          val symmetryAndFlatnessAnalyze = SymmetryAndFlatnessAnalyze( //
            extendedData.output.outputPK.get,
            extendedData.output.procedurePK,
            extendedData.machine.machinePK.get,
            extendedData.output.dataDate.get,
            attributeList,
            beamName,
            collimatorCenteringResource.centerOfBeam(beamName),
            runReq.symmetryAndFlatnessBaselineRedoBeamList,
            isFlatteningFilterFree = makeFloodFieldType(beamName).isFFF
          )

          val result = symmetryAndFlatnessAnalyze.analyze(image, psmGrid = psmGrid)
          result
        }

        val withoutPSMAndFloodField = Some(doAnalyze(psmGrid = None))

        val withPsmAndFloodField =
          if (floodField.isEmpty)
            None
          else {
            Some(doAnalyze(psmGrid = psmGrid))
          }

        Seq(withoutPSMAndFloodField, withPsmAndFloodField).flatten
      }

      // only process beams that are both configured and have been uploaded
      val resultList: List[SymmetryAndFlatnessBeamResult] = beamNameList.par.flatMap(doBeam).toList

      /*
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
       */

      //val pass = resultList.map(r => r.symmetryAndFlatness.allPass(r.baseline)).reduce(_ && _)
      val pass = {
        val list = {
          resultList.map(r => {
            if (r.symmetryAndFlatness.canDoPsm)
              r.symmetryAndFlatness.allWithPSMPass(r.baseline)
            else
              r.symmetryAndFlatness.allNoPSMPass(r.baseline)
          })
        }
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
