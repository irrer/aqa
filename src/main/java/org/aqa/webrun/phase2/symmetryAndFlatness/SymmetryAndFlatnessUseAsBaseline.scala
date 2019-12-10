package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.web.WebUtil.SubUrlRun
import org.restlet.Restlet
import org.restlet.Response
import org.restlet.Request
import org.aqa.web.WebUtil._
import org.aqa.web.WebUtil
import org.aqa.db.Output
import org.aqa.db.Machine
import org.aqa.db.SymmetryAndFlatness
import org.aqa.Logging
import org.aqa.db.MaintenanceRecord
import java.sql.Timestamp
import SymmetryAndFlatnessUseAsBaseline._
import org.aqa.db.Baseline
import org.aqa.db.BaselineSetup
import org.aqa.db.Input
import scala.xml.Elem
import org.aqa.web.MaintenanceRecordUpdate
import org.restlet.data.Status
import org.aqa.db.MaintenanceCategory
import org.aqa.Config

object SymmetryAndFlatnessUseAsBaseline {
  val path = new String((new SymmetryAndFlatnessUseAsBaseline).pathOf)
  val outputPKTag = "outputPK"
  val confirmTag = "confirm"
}

class SymmetryAndFlatnessUseAsBaseline extends Restlet with SubUrlRun with Logging {

  private def safToDesc(saf: SymmetryAndFlatness): String = {
    "    " + saf.beamName + " : Axial Symmetry pct: " + saf.axialSymmetry_pct + "\n" +
      "    " + saf.beamName + " : Transverse Symmetry pct: " + saf.transverseSymmetry_pct + "\n" +
      "    " + saf.beamName + " : Flatness pct: " + saf.flatness_pct
  }

  private def safToBaselineList(saf: SymmetryAndFlatness, maintenanceRecordPK: Long, acquisitionDate: Timestamp): Seq[Baseline] = {
    def make(name: String, value: Double) = {
      new Baseline(None, maintenanceRecordPK, acquisitionDate, Some(saf.SOPInstanceUID), SymmetryAndFlatnessAnalysis.makeBaselineName(saf.beamName, name), value.toString, BaselineSetup.chosen.toString)
    }
    val axial = make(SymmetryAndFlatnessAnalysis.axialSymmetryName, saf.axialSymmetry_pct)
    val transverse = make(SymmetryAndFlatnessAnalysis.transverseSymmetryName, saf.transverseSymmetry_pct)
    val flatness = make(SymmetryAndFlatnessAnalysis.flatnessName, saf.flatness_pct)
    val constancy = make(SymmetryAndFlatnessAnalysis.profileConstancyName, saf.profileConstancy_pct) // TODO
    val top = make(Config.SymmetryPointTop.name, saf.top_cu)
    val bottom = make(Config.SymmetryPointBottom.name, saf.bottom_cu)
    val left = make(Config.SymmetryPointLeft.name, saf.left_cu)
    val right = make(Config.SymmetryPointRight.name, saf.right_cu)
    val center = make(Config.SymmetryPointCenter.name, saf.center_cu)

    Seq(axial, transverse, flatness, constancy, top, bottom, left, right, center)
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

    val safList = SymmetryAndFlatness.getByOutput(outputPK)

    logger.info("User " + userId + " requested creation of MaintenanceRecord record for Symmetry and Flatness from output " + output.outputPK.get)
    val summary = "User " + userId + " created baseline values for Symmetry and Flatness from measured values."
    val preamble = "List of new Symmetry and Flatness baseline values:\n\n"
    val valueText = safList.map(saf => safToDesc(saf)).mkString("\n")
    val creationTime = new Timestamp(System.currentTimeMillis)
    val acquisitionDate = if (input.dataDate.isDefined) input.dataDate.get else creationTime

    val maintenanceRecord = { new MaintenanceRecord(None, MaintenanceCategory.setBaseline, machine.machinePK.get, creationTime, userPK, Some(outputPK), summary, preamble + valueText) }.insert
    val baselineList = safList.map(saf => safToBaselineList(saf, maintenanceRecord.maintenanceRecordPK.get, acquisitionDate)).flatten
    Baseline.insert(baselineList)
    logger.info("User " + userId + " created MaintenanceRecord record for Symmetry and Flatness " + maintenanceRecord)
    maintenanceRecord
  }

  private def setBaseline(request: Request, outputPK: Long): Elem = {
    val maintenanceRecord = makeBaseline(request, outputPK)

    val buttonContent = {
      <div class="row" style="margin:30px;">
        <div class="col-sm-4 col-sm-offset-1">
          <a class="btn btn-default" href={ "/admin/MaintenanceRecordUpdate?maintenanceRecordPK=" + maintenanceRecord.maintenanceRecordPK.get } role="button" title="View, edit or delete the MaintenanceRecord record">View/Edit/Delete MaintenanceRecord</a>
        </div>
        <div class="col-sm-4">
          <a class="btn btn-default" href={ "/view/ViewOutput?outputPK=" + outputPK } role="button" title="View the report generated by the output that defined the baseline values.">Return to Output</a>
        </div>
      </div>
    }

    val content = {
      <div class="row" style="margin:60px;">
        The following MaintenanceRecord record has been created along with the corresponding baseline values:
        <p></p>
        <b>Summary:</b>{ maintenanceRecord.summary }
        <p></p>
        <b>Description:</b><br/>{ maintenanceRecord.summary }
        <p></p>
        <em>Note that you may undo the setting of baseline values by deleting this MaintenanceRecord record.</em>
        { buttonContent }
      </div>
    }
    content
  }

  private def requireConfirmation(request: Request, outputPK: Long): Elem = {

    val content = {
      <div class="row" style="margin:60px;">
        Please either confirm or cancel the setting of the baseline values for symmetry and flatness.
        <p/>
        <em>Note that you may undo the setting of baseline values by deleting this MaintenanceRecord record.</em>
        <div class="col-sm-4 col-sm-offset-1">
          <a class="btn btn-primary" href={ path + "?outputPK=" + outputPK + "&" + confirmTag + "=true" } role="button">Confirm</a>
        </div>
        <div class="col-sm-4">
          <a class="btn btn-default" href={ "/view/ViewOutput?outputPK=" + outputPK } role="button">Cancel</a>
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
