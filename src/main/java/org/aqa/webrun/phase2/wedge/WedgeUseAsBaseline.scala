package org.aqa.webrun.phase2.wedge

import org.aqa.web.WebUtil.SubUrlRun
import org.restlet.Restlet
import org.restlet.Response
import org.restlet.Request
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.Logging
import org.aqa.db.MaintenanceRecord
import java.sql.Timestamp
import WedgeUseAsBaseline._
import org.aqa.db.Baseline
import org.aqa.db.BaselineSetup
import org.aqa.db.Input
import scala.xml.Elem
import org.aqa.web.MaintenanceRecordUpdate
import org.restlet.data.Status
import org.aqa.db.MaintenanceCategory
import org.aqa.Config
import org.aqa.db.WedgePoint
import org.aqa.Util

object WedgeUseAsBaseline {
  val path = new String((new WedgeUseAsBaseline).pathOf)
  val outputPKTag = "outputPK"
  val confirmTag = "confirm"
}

class WedgeUseAsBaseline extends Restlet with SubUrlRun with Logging {

  private def wedgeToDesc(wedgePoint: WedgePoint): String = {
    "    " + WedgeAnalysis.makeBaselineName(wedgePoint) + " : " + Util.fmtDbl(wedgePoint.percentOfBackground_pct)
  }

  private def wedgePointToBaselineList(wedgePoint: WedgePoint, maintenanceRecordPK: Long, acquisitionDate: Timestamp): Baseline = {
    new Baseline(None, maintenanceRecordPK, acquisitionDate, Some(wedgePoint.wedgeSOPInstanceUID), WedgeAnalysis.makeBaselineName(wedgePoint), wedgePoint.percentOfBackground_pct.toString, BaselineSetup.chosen.toString)
  }

  private def makeBaseline(request: Request, outputPK: Long): MaintenanceRecord = {
    val output = Output.get(outputPK).get
    val machine = Machine.get(output.machinePK.get).get
    val input = Input.get(output.inputPK).get

    val user = WebUtil.getUser(request)

    val userPK: Long = {
      WebUtil.getUser(request) match {
        case Some(user) => user.userPK.get
        case _ => output.userPK.get
      }
    }

    val userId = {
      user match {
        case Some(user) => user.id
        case _ => "unknown"
      }
    }

    val wedgePointList = WedgePoint.getByOutput(outputPK)

    logger.info("User " + userId + " requested creation of MaintenanceRecord baseline record for Wedge from output " + output.outputPK.get)
    val summary = "User " + userId + " created baseline values for Wedge from measured values."
    val preamble = "List of new Wedge baseline values:\n\n"
    val valueText = wedgePointList.map(saf => wedgeToDesc(saf)).mkString("\n")
    val creationTime = new Timestamp(System.currentTimeMillis)
    val acquisitionDate = if (input.dataDate.isDefined) input.dataDate.get else creationTime

    val maintenanceRecord = { new MaintenanceRecord(None, MaintenanceCategory.setBaseline, machine.machinePK.get, creationTime, userPK, Some(outputPK), summary, preamble + valueText) }.insert
    val baselineList = wedgePointList.map(saf => wedgePointToBaselineList(saf, maintenanceRecord.maintenanceRecordPK.get, acquisitionDate))
    Baseline.insert(baselineList)
    logger.info("User " + userId + " created MaintenanceRecord record for Wedge " + maintenanceRecord)
    maintenanceRecord
  }

  private def setBaseline(request: Request, outputPK: Long): Elem = {
    val maintenanceRecord = makeBaseline(request, outputPK)

    val viewEditButton = {
      <a class="btn btn-default" href={ "/admin/MaintenanceRecordUpdate?maintenanceRecordPK=" + maintenanceRecord.maintenanceRecordPK.get } role="button" title="View, edit or delete the record">View/Edit/Delete MaintenanceRecord</a>
    }

    val returnToOutputButton = {
      <a class="btn btn-default" href={ "/view/ViewOutput?outputPK=" + outputPK } role="button" title="View the report generated by the output that defined the baseline values.">Return to Output</a>
    }

    val content = {
      <div class="row">
        <div class="col-md-6 col-md-offset-2">
          <div class="row" style="margin-bottom:60px;">
            The following record has been created along with the corresponding baseline values:
            <p></p>
            <b>Summary:</b>{ maintenanceRecord.summary }
            <p></p>
            <b>Description:</b><br/>{ maintenanceRecord.summary }
            <p></p>
            <em>Note that you may undo the setting of baseline values by deleting this record.</em>
          </div>
          <div class="row">
            <div class="col-md-3">
              { viewEditButton }
            </div>
            <div class="col-md-3">
              { returnToOutputButton }
            </div>
          </div>
        </div>
      </div>

    }

    content
  }

  private def requireConfirmation(request: Request, outputPK: Long): Elem = {

    val content = {
      <div class="row">
        <div class="col-md-6 col-md-offset-2">
          <div class="row" style="margin-bottom:60px;">
            Please either confirm or cancel the setting of<br/>
            the baseline values for symmetry and flatness.
            <p></p>
            <em>Note that you may undo the setting of baseline values<br/>by deleting this record.</em>
          </div>
          <div class="row">
            <div class="col-md-3">
              <a class="btn btn-primary" href={ path + "?outputPK=" + outputPK + "&" + confirmTag + "=true" } role="button">Confirm</a>
            </div>
            <div class="col-md-3">
              <a class="btn btn-default" href={ "/view/ViewOutput?outputPK=" + outputPK } role="button">Cancel</a>
            </div>
          </div>
        </div>
      </div>

    }

    content
  }

  private def noOutput = {
    <div>
      No reference was provided.  Use your browser's Back button to return to the previous page.
    </div>
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap = getValueMap(request)

    try {
      val content: Elem =
        (valueMap.get(outputPKTag), valueMap.get(confirmTag)) match {
          case (Some(outputPKText), Some(confirm)) => setBaseline(request, outputPKText.toLong)
          case (Some(outputPKText), _) => requireConfirmation(request, outputPKText.toLong)
          case (_, _) => noOutput
        }

      val html = WebUtil.wrapBody(content, "Set Baseline for Symmetry and Flatness")
      WebUtil.setResponse(html, response, Status.SUCCESS_OK)
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, t)
      }
    }

  }

}
