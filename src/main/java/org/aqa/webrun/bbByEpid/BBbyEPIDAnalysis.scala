package org.aqa.webrun.bbByEpid

import scala.xml.Elem
import org.aqa.Logging
import org.aqa.run.ProcedureStatus
import javax.vecmath.Point3d
import java.awt.image.BufferedImage
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.Phase2Util
import scala.Left

object BBbyEPIDAnalysis extends Logging {

  private val subProcedureName = "BB by EPID"

  case class BBbyEPIDResult(summry: Elem, sts: ProcedureStatus.Value, position: Seq[Point3d], images: Seq[BufferedImage])

  
  
  
  def runProcedure(extendedData: ExtendedData, bbByEpidData: BBbyEPIDData): Either[Elem, BBbyEPIDResult] = {
    try {
      // This code only reports values without making judgment as to pass or fail.
      logger.info("Starting analysis of EPID Alignment")
      logger.info("Finished analysis of EPID Alignment")
      ???
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in analysis of " + subProcedureName + ": " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
      }
    }
  }
}

