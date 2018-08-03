package org.aqa.webrun.phase2

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

/**
 * Analyze DICOM files for Wedge Analysis.
 */
object WedgeAnalysis extends Logging {

  private def wedgeOrientationHorizontal(beamName: String, plan: AttributeList): Boolean = {
    val bs = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val WedgeOrientation = Util.seq2Attr(bs, TagFromName.WedgeSequence).head.get(TagFromName.WedgeOrientation).getDoubleValues.head
    val horiz = Util.angleRoundedTo90(WedgeOrientation) match {
      case 0 => true
      case 90 => false
      case 180 => true
      case 270 => false
    }
    horiz
  }

  /**
   * Generate a list of readings that reflects the slope of the wedge.
   */
  private def analyzeWedge(beamName: String, extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Seq[Wedge] = {
    val derived = runReq.derivedMap(beamName)
    val al = derived.dicomFile.attributeList.get
    val ips = Phase2Util.getImagePlanePixelSpacing(al)
    val measurements = MeasureTBLREdges.measure(derived.pixelCorrectedImage, ips, Util.collimatorAngle(al), derived.pixelCorrectedImage, new Point(0, 0))
    val RescaleIntercept = derived.dicomFile.attributeList.get.get(TagFromName.RescaleIntercept).getDoubleValues.head
    val RescaleSlope = derived.dicomFile.attributeList.get.get(TagFromName.RescaleSlope).getDoubleValues.head
    val SOPInstanceUID = Util.sopOfAl(al)

    // cut out a section of the image that is guaranteed to be pure ramp.
    val xMarginPix = (Config.PenumbraThickness_mm * 3) / ips.getX
    val yMarginPix = (Config.PenumbraThickness_mm * 3) / ips.getY

    val pixMeas = {
      val meas = measurements.measurementSet
      new MeasureTBLREdges.TBLR(meas.top / ips.getY, meas.bottom / ips.getY, meas.left / ips.getX, meas.right / ips.getX)
    }

    val x = (pixMeas.left + (xMarginPix / 2)).round.toInt
    val y = (pixMeas.top + (yMarginPix / 2)).round.toInt
    val width = (pixMeas.right - pixMeas.left - xMarginPix).abs.round.toInt
    val height = (pixMeas.bottom - pixMeas.top - yMarginPix).abs.round.toInt

    val rampArea = derived.pixelCorrectedImage.getSubimage(new Rectangle(x, y, width, height))

    val orientationHorizontal = wedgeOrientationHorizontal(beamName, runReq.rtplan.attributeList.get)
    val profileUnscaled = if (orientationHorizontal) rampArea.columnSums.map(c => c / height) else rampArea.rowSums.map(r => r / width)
    val profile = profileUnscaled.map(p => (p * RescaleSlope) + RescaleIntercept)

    val imageCenter = {
      if (orientationHorizontal)
        ((derived.originalImage.width - 1) * ips.getX) / 2
      else
        ((derived.originalImage.height - 1) * ips.getY) / 2
    }

    def positionOf(i: Int): Double = {
      imageCenter -
        {
          if (orientationHorizontal)
            ((i + x) * ips.getX) + collimatorCentering.xCollimatorCenterMinusImageCenter_mm
          else
            ((i + y) * ips.getY) + collimatorCentering.yCollimatorCenterMinusImageCenter_mm
        }
    }
    val resultList = profile.indices.map(i => new Wedge(None, extendedData.output.outputPK.get, SOPInstanceUID, beamName, positionOf(i), profile(i)))
    resultList
  }

  val subProcedureName = "Wedge"

  class WedgeResult(summary: Elem, status: ProcedureStatus.Value, resultList: Seq[Seq[Wedge]]) extends SubProcedureResult(summary, status, subProcedureName)

  /**
   * Run the WedgeAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, WedgeResult] = {
    try {
      logger.info("Starting analysis of " + subProcedureName)

      val wedgeListList = Config.WedgeBeamList.filter(beamName => runReq.rtimageMap.contains(beamName)).map(beamName => analyzeWedge(beamName, extendedData, runReq, collimatorCentering))
      val status = ProcedureStatus.done
      Wedge.insertSeq(wedgeListList.flatten)
      val summary = WedgeHTML.makeDisplay(extendedData, wedgeListList, status, runReq)
      val result = new WedgeResult(summary, status, wedgeListList)
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
