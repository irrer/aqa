package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CollimatorPosition
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import org.aqa.Util
import scala.collection.Seq
import scala.xml.Elem
import org.aqa.db.Output
import org.aqa.run.ProcedureStatus
import org.aqa.DicomFile
import edu.umro.ImageUtil.DicomImage
import edu.umro.ImageUtil.ImageUtil
import java.awt.geom.Point2D
import org.aqa.Config
import java.awt.Rectangle
import edu.umro.ImageUtil.LocateEdge
import java.awt.image.BufferedImage
import java.awt.Color
import java.awt.Graphics2D
import java.awt.BasicStroke
import edu.umro.ScalaUtil.Trace
import scala.collection.parallel.ParSeq
import org.aqa.db.CollimatorCentering
import java.awt.Point

/**
 * Analyze DICOM files for symmetry and flatness.
 */
object SymmetryAndFlatnessAnalysis extends Logging {

  private def analyzePoints(beamName: String, extendedData: ExtendedData, runReq: RunReq): Seq[SymmetryAndFlatnessPointEvaluated] = {

    val isFlood = beamName.equalsIgnoreCase(Config.FloodFieldBeamName)

    val image = {
      if (isFlood) runReq.floodCorrectedImage
      else runReq.derivedMap(beamName).pixelCorrectedImage
    }

    val al: AttributeList = {
      if (isFlood) runReq.flood.attributeList.get
      else runReq.derivedMap(beamName).dicomFile.attributeList.get
    }

    //Config.SymmetryAndFlatnessPointList.map(p => measurePoint(p))

    ???
  }

  private def analyzeSymmetry(beamName: String, extendedData: ExtendedData, runReq: RunReq): Double = {
    ???
  }

  private def analyzeFlatness(beamName: String, extendedData: ExtendedData, runReq: RunReq): Double = {
    ???
  }

  val subProcedureName = "SymmetryAndFlatness";

  /**
   * Run the CollimatorPosition sub-procedure, save results in the database, return true for pass or false for fail.
   */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCentering: CollimatorCentering): Either[Elem, Any] = {
    try {
      logger.info("Starting analysis of SymmetryAndFlatness")

      val translator = new IsoImagePlaneTranslator(runReq.flood.attributeList.get)
      //      val pixelMap = makePixelMap(runReq, translator)
      val beamNameList = Config.SymmetryAndFlatnessBeamList.filter(beamName => runReq.derivedMap.contains(beamName))
      val pointValueList = beamNameList.par.map(beamName => analyzePoints(beamName, extendedData, runReq)).toList

      val symmetryList = beamNameList.par.map(beamName => analyzeSymmetry(beamName, extendedData, runReq)).toList
      val flatnessList = beamNameList.par.map(beamName => analyzeFlatness(beamName, extendedData, runReq)).toList

      //      val doneList = resultList.filter(r => r.isRight).map(r => r.right.get)
      //      val crashList = resultList.filter(l => l.isLeft).map(l => l.left.get)
      //
      //      // To pass (succeed), there must be no crashes and all successfully processed beams must pass.
      //      val pass = crashList.isEmpty && doneList.map(d => d._1.status.toString.equals(ProcedureStatus.pass.toString)).reduce(_ && _)
      //      val procedureStatus = if (pass) ProcedureStatus.pass else ProcedureStatus.fail
      //
      //      val doneDataList = doneList.map(r => r._1)
      //      CollimatorPosition.insert(doneDataList) // save to database
      //      logger.info("Inserted  " + doneList.size + " + SymmetryAndFlatness rows.")
      //
      //      // TODO currently buffered images are created but not used.  Not sure it is worth it to show them to the user or not.  Also they
      //      //     do not have the X1X2Y1Y2 labels, nor the difference between measured and expected.
      //      val elem = CollimatorPositionHTML.makeDisplay(extendedData, runReq, doneList, crashList, procedureStatus)
      //      val result = Right(new CollimatorPositionResult(elem, procedureStatus, doneDataList, crashList))
      //      logger.info("Starting analysis of SymmetryAndFlatness")
      //      result
      ???
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of CollimatorPosition: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}
