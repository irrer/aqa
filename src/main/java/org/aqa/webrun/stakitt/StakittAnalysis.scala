package org.aqa.webrun.stakitt

import org.aqa.webrun.phase2.RunReq
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.CollimatorCenteringResource
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import org.aqa.run.ProcedureStatus.ProcedureStatus
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.webrun.phase2.SubProcedureResult
import org.aqa.webrun.stakitt.stakittHTML.StakittHTML
import org.aqa.Config

import scala.xml.Elem

object StakittAnalysis extends Logging {

  private val subProcedureName = "Stakitt"

  //noinspection SpellCheckingInspection
  case class StakittProcedureResult(sumry: Elem = { <div>hey</div> }, stats: ProcedureStatus.Value) extends SubProcedureResult(sumry, stats, subProcedureName) {}

  def runProcedure(extendedData: ExtendedData, runReq: RunReq, collimatorCenteringResource: CollimatorCenteringResource): Either[Elem, StakittProcedureResult] = {
    try {

      val rtimageList = runReq.rtimageMap.keys.filter(beamName => beamName.toLowerCase().matches(".*stak.*t.*")).map(beamName => runReq.rtimageMap(beamName))

      // do all the heavy lifting here, each image in parallel
      val analysisList = rtimageList.par.map(rtimage => Analysis.analyze(extendedData, rtimage, runReq.rtplan)).toList

      // put data in database
      val resultList = analysisList.filter(_.isRight).flatMap(_.right.get.stakittList).map(_.stakitt)
      resultList.foreach(_.insert)

      // generate HTML
      val html = new StakittHTML(extendedData, analysisList, runReq.rtplan)
      html.makeHTML()

      // If there was a failure, then report it. Otherwise, return 'done'.
      val status: ProcedureStatus = {
        val failList = analysisList.filter(_.isLeft)
        val s = {
          if (failList.isEmpty)
            ProcedureStatus.done
          else
            failList.head.left.get.status
        }
        s
      }

      val summary = {
        val title = s"Click to view Stakitt details.  Images: ${rtimageList.size}"

        val imageUrl = {
          val pass = status.toString().equals(ProcedureStatus.done.toString())
          if (pass) Config.passImageUrl else Config.failImageUrl
        }

        val href = "stakitt/index.html"

        <div title={title}>
          <a href={href}>
            Stakitt
            <br/>
            <img src={imageUrl} height="32"/>
          </a>
        </div>
      }
      val procedureResult = StakittProcedureResult(summary, status)
      Right(procedureResult)

    } catch {
      case t: Throwable =>
        logger.warn(s"Unexpected error in analysis of $subProcedureName: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }

  }

}
