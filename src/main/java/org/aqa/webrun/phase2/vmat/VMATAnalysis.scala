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

object VMATAnalysis extends Logging {

  //private
  def getPlanAoiList(beamName: String, beamNameOpen: String, al: AttributeList, alOpen: AttributeList, plan: AttributeList): Seq[Rectangle2D] = {

    val beamSeq = Phase2Util.getBeamSequenceOfPlan(beamName, plan)
    val controlPointSeq = DicomUtil.findAllSingle(beamSeq, TagFromName.ControlPointSequence).map(s => DicomUtil.alOfSeq(s.asInstanceOf[SequenceAttribute])).flatten
    val beamLimitList = DicomUtil.findAllSingle(beamSeq, TagFromName.BeamLimitingDeviceSequence).map(s => DicomUtil.alOfSeq(s.asInstanceOf[SequenceAttribute])).flatten

    def beamLimitOfInterest(bl: AttributeList): Option[(Double, Double)] = {
      def isMLC = {
        val t = bl.get(TagFromName.RTBeamLimitingDeviceType)
        (t != null) && t.getSingleStringValueOrEmptyString.toUpperCase.contains("MLC")
      }

      val ljp = bl.get(TagFromName.LeafJawPositions).getDoubleValues.distinct
      val max = ljp.max
      val min = ljp.min

      def isWideEnough = (max - min) > 5 // must be greater than 5 mm

      if (isMLC && isWideEnough)
        Some((min, max))
      else None
    }

    val xLimitList = beamLimitList.map(bl => beamLimitOfInterest(bl)).flatten.distinct.sortBy(minMax => minMax._1)

    val yLimits = 44
    ???
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
