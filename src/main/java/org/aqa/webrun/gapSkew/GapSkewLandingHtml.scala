/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.gapSkew

import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.GapSkew
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet

import java.util.Date
import scala.xml.Elem

/**
  * Support for generating JS scripts on request.
  */
object GapSkewLandingHtml {
  val path = new String((new GapSkewLandingHtml).pathOf)
}

class GapSkewLandingHtml extends Restlet with SubUrlRoot with Logging {

  private case class Summary(machine: Machine, output: Option[Output] = None, gapSkew: Option[GapSkew] = None) {}

  val procedurePK: Long = Procedure.ProcOfGapSkew.get.procedurePK.get

  /**
    * Get the latest Output for GapSkew from the given machine.
    * @param machine Data was from this machine.
    * @return Latest output, or None if there is none.
    */
  private def latestOutput(machine: Machine): Option[Output] = {
    val list = Output.getByMachineAndProcedure(machinePK = machine.machinePK.get, procedurePK)
    list.sortBy(_.dataDate.get.getTime).lastOption
  }

  /**
    * Get all summary data for the given institution.
    *
    * @param institutionPK For this institution.
    *
    * @return List of latest data for each machine in the institution.
    */
  private def summaryData(institutionPK: Long): Seq[Summary] = {

    /**
      * Get the summary for the machine.  If there is no data, then indicate that.
      *
      * @param machine Treatment machine.
      * @return Summary, which may or may not have data.
      */
    def makeSummary(machine: Machine): Summary = {
      val output = latestOutput(machine)
      if (output.isDefined) {
        val gapSkew = GapSkew.getByOutput(output.get.outputPK.get).maxBy(_.largestHorzSkew_deg)
        Summary(machine, output, Some(gapSkew))
      } else {
        Summary(machine)
      }
    }

    val machineList = Machine.listMachinesFromInstitution(institutionPK).sortWith(Machine.orderMachine)

    val summaryList = machineList.map(makeSummary)
    summaryList
  }

  /**
    * Return true if the output exists but is expired.  If the output is None then false is returned.
    * @param output Check date of this output.
    * @return True if data expired.
    */
  private def dataExpired(output: Option[Output]): Boolean = {
    val cutoff = System.currentTimeMillis() - Config.GapSkewExpiration_ms
    output.isDefined && (output.get.dataDate.get.getTime < cutoff)
  }

  private def summaryToHtml(summary: Summary): Elem = {

    val machineElem = {
      val color: String = {
        0 match {
          case _ if summary.output.isEmpty => GapSkewUtil.colorNone
          case _                           => GapSkewUtil.statusColor(summary.gapSkew.get.largestHorzSkew_deg)
        }
      }

      val title = {
        val common = WebUtil.titleNewline + "Warning limit: " + Config.GapSkewAngleWarn_deg + "  Fail limit: " + Config.GapSkewAngleFail_deg

        0 match {
          case _ if summary.gapSkew.isEmpty     => "No gap skew data for this machine."
          case _ if dataExpired(summary.output) => "The data for this machine more than " + Config.GapSkewExpiration_day + " days old and is out of date." + common
          case _                                => "Largest skew (deg): " + summary.gapSkew.get.largestHorzSkew_deg
        }
      }

      <td><h3 style={"margin:8px; background-color:" + color + "; border:solid " + color + " 1px; border-radius: 10px; padding: 12px;"} title={title} aqaalias="">{summary.machine.id}</h3></td>
    }

    val dateElem = {
      val color = if (dataExpired(summary.output)) GapSkewUtil.colorNone else "white"

      val elem = {
        if (summary.output.isEmpty)
          <td></td>
        else {
          <td style={"background-color:" + color}>{WebUtil.timeAgo(new Date(summary.output.get.dataDate.get.getTime))}</td>
        }
      }
      elem
    }

    val skewElem: Elem = {
      if (summary.gapSkew.isEmpty)
        <td>No Data</td>
      else {
        val skew = summary.gapSkew.get.largestHorzSkew_deg
        <td title={"Skew (deg): " + skew}>{GapSkewUtil.fmt2(skew)}</td>
      }
    }

    val detailsElem: Elem = {
      if (summary.output.isEmpty)
        <td></td>
      else
        <td><a href={ViewOutput.viewOutputUrl(summary.output.get.outputPK.get)}>Details</a></td>
    }

    <tr>
      {machineElem}
      {dateElem}
      {skewElem}
      {detailsElem}
    </tr>
  }

  private def content(institutionPK: Long): Elem = {
    val summary = summaryData(institutionPK)
    <div class="row">
      <div class="col-md-4 col-md-offset-4">
        <center><h2>Gap Skew Offset Status</h2></center>
        <table class="table table-responsive table-bordered">
            <tr>
              <td><b> Machine </b></td>
              <td><b> Date </b></td>
              <td title="Skew show as angle in degrees"><b> Largest Skew </b></td>
              <td><b> Details </b></td>
            </tr>
            {summary.map(summaryToHtml)}
        </table>
      </div>
    </div>
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
      val text = WebUtil.wrapBody(content(institutionPK), "Gap Skew")
      WebUtil.respond(content(institutionPK), "Gap Skew", response)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
