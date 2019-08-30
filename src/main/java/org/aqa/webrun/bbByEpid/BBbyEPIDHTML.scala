package org.aqa.webrun.bbByEpid

import org.aqa.webrun.ExtendedData
import org.aqa.db.BBbyEPID
import org.aqa.run.ProcedureStatus
import org.aqa.db.BBbyEPIDComposite

/**
 * Generate and write HTML for EPID BB analysis.
 */
object BBbyEPIDHTML {

  def generateHtml(extendedData: ExtendedData, bbByEPIDList: Seq[Option[BBbyEPID]], composite: Option[BBbyEPIDComposite], runReq: BBbyEPIDRunReq, status: ProcedureStatus.Value) = {
    // TODO
  }

}
