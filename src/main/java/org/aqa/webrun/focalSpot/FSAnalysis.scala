package org.aqa.webrun.focalSpot

import org.aqa.webrun.ExtendedData
import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.FocalSpotSet
import org.aqa.run.ProcedureStatus
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult

import scala.xml.Elem

object FSAnalysis extends Logging {

  private val maxEpidRange_mm = 1.0

  private def isValidSet(measureList: Seq[FSMeasure]): Boolean = {
    def epidRange: Double = {
      val list = measureList.map(_.RTImageSID_mm)
      list.max - list.min
    }
    // @formatter:off
    measureList.size > 3                        &&
    measureList.exists(m => m.isJaw && m.is090) &&
    measureList.exists(m => m.isJaw && m.is270) &&
    measureList.exists(m => m.isMLC && m.is090) &&
    measureList.exists(m => m.isMLC && m.is270) &&
    (epidRange < maxEpidRange_mm)   // must have similar epid positions.
    // @formatter:on
  }

  private def toSet(measureList: Seq[FSMeasure]): FSSet = {
    FSSet(
      jaw090 = measureList.find(m => m.isJaw && m.is090).get,
      jaw270 = measureList.find(m => m.isJaw && m.is270).get,
      mlc090 = measureList.find(m => m.isMLC && m.is090).get,
      mlc270 = measureList.find(m => m.isMLC && m.is270).get
    )
  }

  private def storeToDb(fsSet: FSSet, outputPK: Long): Unit = {

    val fsJaw090 = fsSet.jaw090.focalSpot.insert
    val fsJaw270 = fsSet.jaw270.focalSpot.insert
    val fsMlc090 = fsSet.mlc090.focalSpot.insert
    val fsMlc270 = fsSet.mlc270.focalSpot.insert

    val focalSpotSet = FocalSpotSet(
      focalSpotSetPK = None,
      outputPK = outputPK,
      KVP_kv = fsJaw090.KVP_kv,
      isFFF = fsJaw090.isFFF,
      focalSpotAlignmentX_mm = fsSet.focalSpotAlignmentX_mm,
      focalSpotAlignmentY_mm = fsSet.focalSpotAlignmentY_mm,
      jaw090PK = fsJaw090.focalSpotPK.get,
      jaw270PK = fsJaw270.focalSpotPK.get,
      mlc090PK = fsMlc090.focalSpotPK.get,
      mlc270PK = fsMlc270.focalSpotPK.get
    )

    focalSpotSet.insert
    logger.info(s"Inserted to DB FocalSpotSet $focalSpotSet")
  }

  val subProcedureName = "Focal Spot"

  case class FocalSpotResult(sum: Elem, sts: ProcedureStatus.Value) extends SubProcedureResult(sum, sts, subProcedureName)

  def runProcedure(extendedData: ExtendedData, fsRunReq: FSRunReq): Either[Elem, FocalSpotResult] = {

    try {
      logger.info(s"Starting processing of $subProcedureName")
      val outputPK = extendedData.output.outputPK.get

      // If running as procedure focal spot, then use all images. Otherwise only use the ones designated in the in configuration as focal spot beams.
      val rtimageList = {
        if (extendedData.procedure.isFocalSpot)
          fsRunReq.rtimageMap.values
        else {
          val nameList = fsRunReq.rtimageMap.keys.filter(beamName => Config.FocalSpotBeamNameList.contains(beamName))
          nameList.map(beamName => fsRunReq.rtimageMap(beamName))
        }
      }

      val measureList: Seq[FSMeasure] = rtimageList.par.map(rtimage => FSMeasure(fsRunReq.rtplan, rtimage, outputPK)).toList

      // make sets of four based on energy and fluence mode
      val fsSetList: Seq[FSSet] = {
        val groupList = measureList.groupBy(m => m.NominalBeamEnergy + ":" + m.fluenceName).values.toSeq
        groupList.filter(isValidSet).map(toSet).sortBy(_.jaw090.NominalBeamEnergy)
      }

      val NominalBeamEnergyList = fsSetList.map(_.jaw090.NominalBeamEnergy).mkString(", ")
      logger.info(s"Found ${fsSetList.size} sets of focal spot data with nominal beam energies of $NominalBeamEnergyList.")

      fsSetList.foreach(fsSet => storeToDb(fsSet, outputPK))

      val sum = FSHTML.makeHtml(extendedData, fsRunReq, fsSetList)
      val focalSpotResult = FocalSpotResult(sum, ProcedureStatus.done)
      logger.info(s"Finished processing of $subProcedureName")
      Right(focalSpotResult)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of " + subProcedureName + " : " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }

  }
}
