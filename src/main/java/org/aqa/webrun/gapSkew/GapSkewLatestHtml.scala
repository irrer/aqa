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
  * Show latest gap skew results for all machines.
  */
object GapSkewLatestHtml {
  val path = new String((new GapSkewLatestHtml).pathOf)
}

class GapSkewLatestHtml extends Restlet with SubUrlRoot with Logging {

  private val userDateFormat = new SimpleDateFormat("d MMM yyyy")

  private case class Latest(machine: Machine, output: Option[Output] = None, gapSkew: Option[GapSkew] = None) {}

  private case class OutputGapSkew(output: Output, gapSkew: Option[GapSkew]) {}

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
          Latest(machine, output)
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
        Some(OutputGapSkew(o, gs))
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

  private def dataTitle(outputGapSkew: OutputGapSkew): String = {
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
      if (outputGapSkew.gapSkew.isDefined) {
        val largestSkew = outputGapSkew.gapSkew.get.largestHorzSkew_deg.abs
        val text = 0 match {
          case _ if largestSkew.abs > Config.GapSkewAngleFail_deg => "The largest skew is above the fail limit."
          case _ if largestSkew.abs > Config.GapSkewAngleWarn_deg => "The largest skew is above the warning limit."
          case _                                                  => "All skew angles are below the warning limit."
        }
        text
      } else "No Data"
    }

    val valueTitle = {
      "Largest skew (deg): " + {
        if (outputGapSkew.gapSkew.isDefined)
          outputGapSkew.gapSkew.get.largestHorzSkew_deg.formatted("%10.5f").trim
        else
          "No Data"
      }
    }

    val title = Seq(valueTitle, dateTitle, statusTitle, limits).filterNot(_.isEmpty).mkString(sep1, sep1 + sep1, sep1)
    title
  }

  private def decorate(elem: Elem, outputGapSkew: OutputGapSkew): Elem = {

    val color = {
      if (outputGapSkew.gapSkew.isDefined)
        GapSkewUtil.statusColor(outputGapSkew.gapSkew.get.largestHorzSkew_deg)
      else
        GapSkewUtil.colorAbort
    }
    val borderColor = if (dataExpired(Some(outputGapSkew.output))) GapSkewUtil.colorNone else color

    val style = "margin:8px; background-color:" + color + "; border:solid " + borderColor + " 5px; border-radius: 5px; padding: 4px;white-space: nowrap; padding: 12px;"

    <div style={style}>{elem}</div>
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

    val rowHeight = 115
    val colWidth = 150

    val machineElem = {
      <h2 aqaalias="">{latest.machine.id}</h2>
    }

    val previousElem: Elem = {
      val previousList = gapSkewData.getPrevious(machine)

      def toElem(outputGapSkew: OutputGapSkew): Elem = {

        val largestHorzSkew_deg: String = {
          if (outputGapSkew.gapSkew.isDefined)
            "Skew (deg): " + GapSkewUtil.fmt2(outputGapSkew.gapSkew.get.largestHorzSkew_deg)
          else
            "No Data"
        }

        val outputDate = outputGapSkew.output.dataDate.get

        val elem = {
          <div>{userDateFormat.format(outputDate)}</div>
        }

        val href = ViewOutput.viewOutputUrl(outputGapSkew.output.outputPK.get)

        val style = s"width:${colWidth}px; padding-right:10px; padding-left:10px; border-right:1px solid lightgrey; "

        <a title={dataTitle(outputGapSkew)} style={style} href={href}>
          <center>
            {decorate(elem, outputGapSkew)}
            <div>{largestHorzSkew_deg}</div>
          </center>
        </a>
      }

      val tdStyle = s"height: ${rowHeight}px; overflow: auto; display: flex; flex-direction: row; vertical-align:middle;"

      def noData = { <h3 style="color: lightgrey;vertical-align:middle;"><i style="vertical-align:middle;">No Data</i></h3> }

      val elemList = previousList.map(toElem)

      <td style={tdStyle}>
        {if (previousList.isEmpty) noData else elemList}
      </td>

    }

    val row = {
      <tr>
        <td style="vertical-align:middle;width:1%;"> {machineElem} </td>
        {previousElem}
    </tr>
    }

    row
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
            <table class="table table-bordered">
              <tr>
                <td>
                  <center><h3> Machine </h3></center>
                </td>
                <td>
                  <h3> Results (most recent first) </h3>
                </td>
              </tr>
              {list}
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
