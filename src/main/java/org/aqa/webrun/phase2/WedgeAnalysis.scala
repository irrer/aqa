package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.PositioningCheck
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

/**
 * Analyze DICOM files for Wedge Analysis.
 */
object WedgeAnalysis extends Logging {

  private def analyzeWedge(beamName: String, extendedData: ExtendedData, runReq: RunReq): Seq[Wedge] = {
    val derived = runReq.derivedMap(beamName)
    val al = derived.dicomFile.attributeList.get
    val ips = Phase2Util.getImagePlanePixelSpacing(al)
    val measurements = MeasureTBLREdges.measure(derived.pixelCorrectedCroppedImage, ips, Util.collimatorAngle(al), derived.pixelCorrectedImage, runReq.floodOffset)

    // cut out a section of the image that is guaranteed to be pure ramp.
    val margin = Config.PenumbraThickness_mm / 2
    val meas = measurements.measurementSet

    val x = ((meas.left + margin) / ips.getX).round.toInt
    val y = ((meas.top + margin) / ips.getY).round.toInt
    val width = (meas.right - meas.left).abs.round.toInt
    val height = (meas.bottom - meas.top).abs.round.toInt

    val rampArea = derived.pixelCorrectedImage.getSubimage(new Rectangle(x, y, width, height))

    ???
  }

  private val subProcedureName = "Wedge"

  class WedgeResult(summary: Elem, status: ProcedureStatus.Value, resultList: Seq[Wedge]) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the WedgeAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, WedgeResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName)
      //      val planAttrList = runReq.rtplan.attributeList.get
      //
      //      val rtimageList = Config.PositioningCheckBeamNameList.map(BeamName => runReq.rtimageMap(BeamName))
      //      val resultList = rtimageList.map(rtimage => makePositioningCheck(extendedData.output.outputPK.get, planAttrList, rtimage.attributeList.get)).flatten
      //
      //      // make sure all were processed and that they all passed
      //      val pass = (resultList.size == Config.PositioningCheckBeamNameList.size) && resultList.map(pc => pc.pass).reduce(_ && _)
      //      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
      //
      //      PositioningCheck.insert(resultList)
      //      val elem = PositioningCheckHTML.makeDisplay(extendedData, runReq, resultList, procedureStatus)
      //      val pcr = Right(new PositioningCheckResult(elem, procedureStatus, resultList))
      logger.info("Finished analysis of " + subProcedureName)
      ???
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of PositioningCheck: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
