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

import java.text.SimpleDateFormat
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

  private case class GapSkewData(machineList: Seq[Machine], outputList: Seq[Output], gapSkewList: Seq[GapSkew]) {

    /**
      * Get the latest Output for GapSkew from the given machine.
      * @param machine Data was from this machine.
      * @return Latest output, or None if there is none.
      */
    def getLatestForMachine(machine: Machine): Latest = {
      val output = outputList.filter(_.machinePK.get == machine.machinePK.get).sortBy(_.dataDate.get.getTime).lastOption

      if (output.isDefined) {
        val list = gapSkewList.filter(_.outputPK == output.get.outputPK.get)
        if (list.isEmpty)
          Latest(machine)
        else {
          val gapSkew = list.maxBy(_.largestHorzSkew_deg)
          Latest(machine, output, Some(gapSkew))
        }
      } else {
        Latest(machine)
      }
    }

    private def largestGapSkew(output: Output): Option[GapSkew] = {
      val list = gapSkewList.filter(_.outputPK == output.outputPK.get)
      if (list.isEmpty)
        None
      else
        Some(list.maxBy(_.largestHorzSkew_deg))
    }

    /**
      * Get all GapSkew results for this machine, ordered as most recent first.
      * @param machine Only for this machine.
      * @return List of GapSkew results for this machine, ordered as most recent first.
      */
    def getPrevious(machine: Machine): Seq[OutputGapSkew] = {
      val outList = outputList.filter(_.machinePK.get == machine.machinePK.get).sortBy(_.dataDate.get.getTime).reverse
      val list = outList.flatMap(o => {
        val gs = largestGapSkew(o)
        if (gs.isEmpty)
          None
        else
          Some(OutputGapSkew(o, gs.get))
      })
      list
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

  private def decorate(elem: Elem, outputGapSkew: OutputGapSkew): Elem = {

    val color = GapSkewUtil.statusColor(outputGapSkew.gapSkew.largestHorzSkew_deg)
    val borderColor = if (dataExpired(Some(outputGapSkew.output))) GapSkewUtil.colorNone else color

    val expired = dataExpired(Some(outputGapSkew.output))

    val spaces = "    "

    val sep1 = WebUtil.titleNewline + spaces

    val expirationDateText = {
      val days = Config.GapSkewExpiration_day
      if (days.round == days)
        days.round.toString
      else
        Util.fmtDbl(days)
    }

    val dateTitle = {
      val dateFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm")
      dateFormat.format(outputGapSkew.output.dataDate.get) + {
        if (expired) sep1 + sep1 + "The data for this machine more than " + expirationDateText + spaces + sep1 + "days old and is out of date. (grey border)" else ""
      }
    }

    val limits = {
      "Warning limit (deg): " + Config.GapSkewAngleWarn_deg + spaces + "Fail limit (deg): " + Config.GapSkewAngleFail_deg + spaces
    }

    val statusTitle = {
      val largestSkew = outputGapSkew.gapSkew.largestHorzSkew_deg.abs
      val text = 0 match {
        case _ if largestSkew.abs > Config.GapSkewAngleFail_deg => "The largest skew is above the fail limit."
        case _ if largestSkew.abs > Config.GapSkewAngleWarn_deg => "The largest skew is above the warning limit."
        case _                                                  => "All skew angles are below the warning limit."
      }
      text
    }

    val valueTitle = "Largest skew (deg): " + outputGapSkew.gapSkew.largestHorzSkew_deg.formatted("%10.5f").trim

    val title = Seq(valueTitle, dateTitle, statusTitle, limits).filterNot(_.isEmpty).mkString(sep1, sep1 + sep1, sep1)

    val style = "margin:8px; background-color:" + color + "; border:solid " + borderColor + " 8px; border-radius: 15px; padding: 4px;white-space: nowrap; padding: 12px;"

    <div title={title} style={style}>{elem}</div>
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
      val content = {
        val h = {
          val length = latest.machine.getRealId.trim.length
          length match {
            case _ if length < 6  => <h1 aqaalias="">{latest.machine.id}</h1>
            case _ if length < 12 => <h2 aqaalias="">{latest.machine.id}</h2>
            case _                => <h3 aqaalias="">{latest.machine.id}</h3>
          }
        }
        val elem = <center>{h}</center>
        if (latest.output.isEmpty)
          elem
        else
          decorate(elem, OutputGapSkew(latest.output.get, latest.gapSkew.get))
      }

      <td style="vertical-align:middle; text-align:center;">{content}</td>
    }

    val previousElem: Elem = {
      val previousList = gapSkewData.getPrevious(machine)

      def toElem(outputGapSkew: OutputGapSkew): Elem = {
        val largestHorzSkew_deg = outputGapSkew.gapSkew.largestHorzSkew_deg
        val outputDate = outputGapSkew.output.dataDate.get

        val title = Util.timeHumanFriendly(outputDate) + WebUtil.titleNewline + "Largest skew (deg): " + largestHorzSkew_deg

        val elem = {
          <div>{WebUtil.timeAgo(outputDate)}</div>
        }

        <div title={title} style="border: 1px solid lightgrey; padding: 5px; margin-right:8px; margin-top:3px; ">
          <a href={ViewOutput.viewOutputUrl(outputGapSkew.output.outputPK.get)}>
            {decorate(elem, outputGapSkew)}
            <center>
              <div>Skew (deg): {GapSkewUtil.fmt2(largestHorzSkew_deg)}</div>
            </center>
          </a>
        </div>
      }

      <td style="width:100%;">
        <div style="overflow: auto; display: flex; flex-direction: row; vertical-align:middle;">
            {if (previousList.isEmpty) <h3 style="color: lightgrey;"><i>No Data</i></h3> else previousList.map(toElem)}
        </div>
      </td>
    }

    <div class="row" style="border-bottom:solid lightgrey 1px;">
      <div class="col-md-2">
        {machineElem}
      </div>
      <div class="col-md-10" style="border-left:solid lightgrey 1px;">
        {previousElem}
      </div>
    </div>
  }

  private def content(gapSkewData: GapSkewData): Elem = {
    val list = gapSkewData.machineList.map(m => summaryToHtml(m, gapSkewData))
    <div class="row">
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <center><h2 style="margin-bottom:24px;">Latest Gap Skew Offset</h2></center>
        </div>
        <div class="row">
          <div class="col-md-12">
            <div class="row" style="border-bottom:solid lightgrey 1px;">
              <div class="col-md-2">
                <h3><u> Machine </u></h3>
              </div>
              <div class="col-md-10" style="border-left:solid lightgrey 1px;">
                <h3><u> Results (most recent first) </u></h3>
              </div>
            </div>
              {list}
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
