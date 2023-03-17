package org.aqa.webrun.focalSpot

import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq
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
    measureList.size == 4                       &&
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
      focalSpotAlignmentX = fsSet.focalSpotAlignmentX,
      focalSpotAlignmentY = fsSet.focalSpotAlignmentY,
      jaw090PK = fsJaw090.focalSpotPK.get,
      jaw270PK = fsJaw270.focalSpotPK.get,
      mlc090PK = fsMlc090.focalSpotPK.get,
      mlc270PK = fsMlc270.focalSpotPK.get
    )

    focalSpotSet.insert
    logger.info(s"Inserted to DB FocalSpotSet $focalSpotSet")
  }

  val subProcedureName = "FocalSpot"

  case class FocalSpotResult(sum: Elem, stats: ProcedureStatus.Value) extends SubProcedureResult(sum, stats, subProcedureName)

  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, FocalSpotResult] = {

    try {
      logger.info(s"Starting processing of $subProcedureName")
      val outputPK = extendedData.output.outputPK.get

      val rtimageList = {
        val nameList = runReq.rtimageMap.keys.filter(beamName => Config.FocalSpotBeamNameList.contains(beamName))
        nameList.map(beamName => runReq.rtimageMap(beamName))
      }

      val measureList: Seq[FSMeasure] = rtimageList.map(rtimage => FSMeasure(runReq.rtplan, rtimage, outputPK)).toSeq

      // make sets of four based on energy
      val fsSetList: Seq[FSSet] = {
        val groupList = measureList.groupBy(_.NominalBeamEnergy).values.toSeq
        groupList.filter(isValidSet).map(toSet)
      }

      val NominalBeamEnergyList = fsSetList.map(_.jaw090.NominalBeamEnergy).mkString(", ")
      logger.info(s"Found ${fsSetList.size} sets of focal spot data with nominal beam energies of $NominalBeamEnergyList.")

      fsSetList.foreach(fsSet => storeToDb(fsSet, outputPK))

      val sum = FSHTML.makeHtml(extendedData, runReq, fsSetList)
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
