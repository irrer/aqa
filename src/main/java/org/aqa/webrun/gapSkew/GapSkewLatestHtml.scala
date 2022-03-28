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
import org.aqa.Util
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
object GapSkewLatestHtml {
  val path = new String((new GapSkewLatestHtml).pathOf)
}

class GapSkewLatestHtml extends Restlet with SubUrlRoot with Logging {

  private case class Latest(machine: Machine, output: Option[Output] = None, gapSkew: Option[GapSkew] = None) {}

  private case class OutputGapSkew(output: Output, gapSkew: GapSkew) {}

  val procedurePK: Long = Procedure.ProcOfGapSkew.get.procedurePK.get

  private case class GapSkewData(machineList: Seq[Machine], outputList: Seq[Output], gapSkewList: Seq[GapSkew]) {

    /**
      * Get the latest Output for GapSkew from the given machine.
      * @param machine Data was from this machine.
      * @return Latest output, or None if there is none.
      */
    def getLatestForMachine(machine: Machine): Latest = {
      val output = outputList.filter(_.machinePK.get == machine.machinePK.get).sortBy(_.dataDate.get.getTime).lastOption

      if (output.isDefined) {
        val gapSkew = gapSkewList.filter(_.outputPK == output.get.outputPK.get).maxBy(_.largestHorzSkew_deg)
        Latest(machine, output, Some(gapSkew))
      } else {
        Latest(machine)
      }
    }

    private def largestGapSkew(output: Output): GapSkew = {
      gapSkewList.filter(_.outputPK == output.outputPK.get).maxBy(_.largestHorzSkew_deg)
    }

    def getPrevious(machine: Machine): Seq[OutputGapSkew] = {
      val outList = outputList.filter(_.machinePK.get == machine.machinePK.get).sortBy(_.dataDate.get.getTime).reverse.drop(1)
      outList.map(o => OutputGapSkew(o, largestGapSkew(o)))
    }
  }

  private def getGapSkewData(institutionPK: Long): GapSkewData = {
    val machineList = Machine.listMachinesFromInstitution(institutionPK).sortWith(Machine.orderMachine)
    val procedurePK = Procedure.ProcOfGapSkew.get.procedurePK.get
    val outputList = machineList.flatMap(m => Output.getByMachineAndProcedure(m.machinePK.get, procedurePK))
    val outputSet = outputList.map(_.outputPK.get).toSet
    val gapSkewList = GapSkew.listByOutputSet(outputSet)

    GapSkewData(machineList, outputList, gapSkewList)

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

  private def summaryToHtml(machine: Machine, gapSkewData: GapSkewData): Elem = {

    val latest = gapSkewData.getLatestForMachine(machine)

    val machineElem = {
      val color: String = {
        0 match {
          case _ if latest.output.isEmpty => GapSkewUtil.colorNone
          case _                          => GapSkewUtil.statusColor(latest.gapSkew.get.largestHorzSkew_deg)
        }
      }

      val title = {
        val common = WebUtil.titleNewline + "Warning limit: " + Config.GapSkewAngleWarn_deg + "  Fail limit: " + Config.GapSkewAngleFail_deg

        0 match {
          case _ if latest.gapSkew.isEmpty     => "No gap skew data for this machine."
          case _ if dataExpired(latest.output) => "The data for this machine more than " + Config.GapSkewExpiration_day + " days old and is out of date." + common
          case _                               => "Largest skew (deg): " + latest.gapSkew.get.largestHorzSkew_deg
        }
      }

      <td><h3 style={"margin:17px; background-color:" + color + "; border:solid " + color + " 1px; border-radius: 10px; padding: 12px;"} title={title} aqaalias="">{latest.machine.id}</h3></td>
    }

    val dateElem = {
      val color = if (dataExpired(latest.output)) GapSkewUtil.colorNone else "white"

      val elem = {
        if (latest.output.isEmpty)
          <td></td>
        else {
          <td style={"vertical-align:middle; text-align:center; white-space: nowrap; background-color:" + color}>{WebUtil.timeAgo(new Date(latest.output.get.dataDate.get.getTime))}</td>
        }
      }
      elem
    }

    val skewElem: Elem = {
      val style = "vertical-align:middle; text-align:center;"
      if (latest.gapSkew.isEmpty)
        <td style={style}>No Data</td>
      else {
        val skew = latest.gapSkew.get.largestHorzSkew_deg
        <td title={"Skew (deg): " + skew} style={style}>{GapSkewUtil.fmt2(skew)}</td>
      }
    }

    val detailsElem: Elem = {
      if (latest.output.isEmpty)
        <td></td>
      else
        <td style="vertical-align:middle; text-align:center;">
          <a href={ViewOutput.viewOutputUrl(latest.output.get.outputPK.get)}>Details</a>
        </td>
    }

    val previousElem: Elem = {

      val previousList = gapSkewData.getPrevious(machine)

      def toElem(outputGapSkew: OutputGapSkew): Elem = {
        val largestHorzSkew_deg = outputGapSkew.gapSkew.largestHorzSkew_deg
        val color = GapSkewUtil.statusColor(largestHorzSkew_deg)
        val outputDate = outputGapSkew.output.dataDate.get

        val title = Util.timeHumanFriendly(outputDate) + WebUtil.titleNewline + "Largest skew (deg): " + largestHorzSkew_deg
        val dateStyle = "margin:8px; background-color:" + color + "; border:solid " + color + " 1px; border-radius: 4px; padding: 4px;white-space: nowrap; padding: 12px;"

        <div title={title} style="border: 1px solid lightgrey; padding: 5px; margin-right:8px;">
          <a href={ViewOutput.viewOutputUrl(outputGapSkew.output.outputPK.get)}>
            <center>
              <div style={dateStyle}>{WebUtil.timeAgo(outputDate)}</div>
              <div>{GapSkewUtil.fmt2(largestHorzSkew_deg)}</div>
            </center>
          </a>
        </div>
      }

      val pList = previousList.map(toElem)

      <td style="width: 100%;">
        <div style="width: 1050px; overflow: auto; display: flex; flex-direction: row;">
            {pList ++ pList ++ pList ++ pList ++ pList ++ pList}
        </div>
      </td>
    }

    <tr>
      {machineElem}
      {dateElem}
      {skewElem}
      {detailsElem}
      {previousElem}
    </tr>
  }

  private def content(gapSkewData: GapSkewData): Elem = {
    val j = gapSkewData.machineList.filter(m => m.getRealId.length < 4).map(m => summaryToHtml(m, gapSkewData)) //  TODO rm

    val list = gapSkewData.machineList.map(m => summaryToHtml(m, gapSkewData)) // TODO put back
    <div class="row">
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <center><h2 style="margin-bottom:24px;">Latest Gap Skew Offset</h2></center>
        </div>
        <div class="row">
          <div class="col-md-12">
            <table class="table table-responsive table-bordered" xstyle="table-layout: fixed;">
              <tr>
                <td><b> Machine </b></td>
                <td><b> Date </b></td>
                <td title="Skew shown as angle in degrees"><b style="white-space: nowrap;"> Largest Skew (deg) </b></td>
                <td><b> Details </b></td>
                <td style="width: 100%;"><b> Previous </b></td>
              </tr>
              {j}
            </table>
          </div>
        </div>
      </div>
    </div>
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val institutionPK = WebUtil.getUser(valueMap).get.institutionPK
      val gapSkewData = getGapSkewData(institutionPK)
      WebUtil.respond(content(gapSkewData), "Latest Gap Skew", response)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
