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
import org.aqa.db.CachedUser
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
  val path: String = new String((new GapSkewLatestHtml).pathOf)

  val ref: Elem = <a href={path} title="Latest results for all machines" style="margin-left:50px;"> Latest Gap Skew </a>
}

class GapSkewLatestHtml extends Restlet with SubUrlRoot with Logging {

  private val dateFormat = new SimpleDateFormat("EEE MMM d, yyyy HH:mm")

  /**
    * Return true if the output exists and is recent.  If the output is None then false is returned.
    * @param output Check date of this output.
    * @return True if data expired.
    */
  private def dataRecent(output: Output): Boolean = {
    val cutoff = System.currentTimeMillis() - Config.GapSkewExpiration_ms
    output.dataDate.get.getTime >= cutoff
  }

  private def makeSummary(href: String, output: Output, gos: GapOffsetSkew): Elem = {

    def fmt(description: String, v090: GosValue, v270: GosValue): Elem = {
      <tr>
        <td style="text-align:right; padding-right:8px; border:1px solid lightgrey;white-space:nowrap;">{description + " " + v090.units}</td>
        <td style="text-align:right; padding-left:8px; padding-right:8px; border:1px solid lightgrey;" title={v090.v.toString}><b>{GapSkewUtil.fmt2(v090.v)}</b></td>
        <td style="text-align:right; padding-left:8px; border:1px solid lightgrey;" title={v270.v.toString}><b>{GapSkewUtil.fmt2(v270.v)}</b></td>
      </tr>
    }

    val dateStyle = {
      if (dataRecent(output))
        "border:solid #ddddff 1px; border-radius: 8px; padding: 8px;"
      else
        "background-color:#eeeeee; border:solid #dddddd 1px; border-radius: 8px; padding: 8px;"
    }

    val dateTitle = {
      val age_day = (System.currentTimeMillis() - output.dataDate.get.getTime) / (24 * 60 * 60 * 1000.0).round.toInt
      val ageText = s"Data is $age_day days old."
      if (dataRecent(output))
        ageText
      else {
        s"$ageText ${WebUtil.titleNewline}Grey background indicates data older than ${Config.GapSkewExpiration_day.toString} days"
      }
    }

    <td style="text-align:center; vertical-align:middle; border:1px solid darkgrey; padding-right:24px; padding-left:24px;">
      <center style={dateStyle} title={dateTitle}><a href={href}>{Util.formatDate(dateFormat, output.dataDate.get)}</a></center>
      <div>
        <table class="table-borderless" style="border-collapse:collapse; border-style:hidden;">
          <tr style="border:1px solid lightgrey;">
            <td style="text-align:center;border:1px solid lightgrey;"> Value </td>
            <td style="text-align:center;border:1px solid lightgrey;"> C90 </td>
            <td style="text-align:center;border:1px solid lightgrey;"> C270 </td>
          </tr>
          {fmt("A Skew", gos.col090.aSkew_mmPer40cm, gos.col270.aSkew_mmPer40cm)}
          {fmt("B Skew", gos.col090.bSkew_mmPer40cm, gos.col270.bSkew_mmPer40cm)}
          {fmt("Gap", gos.col090.abAvgDiff, gos.col270.abAvgDiff)}
          {fmt("Offset", gos.col090.abAvgAvg, gos.col270.abAvgAvg)}
        </table>
      </div>
    </td>
  }

  private def toSummary(output: Output, gapSkewHistory: Seq[GapSkew.GapSkewHistory]): Elem = {

    val href = ViewOutput.viewOutputUrl(output.outputPK.get)

    GapOffsetSkew.makeGapOffsetSkew(gapSkewHistory.map(_.gapSkew)) match {
      case Right(gos) => makeSummary(href, output, gos)
      case Left(error) =>
        val nl = WebUtil.titleNewline
        val title = s"Unable to process.$nl${nl}Missing images that have:$nl$error$nl${nl}Click for details."
        <td title={title} style={"vertical-align:middle; text-align:center; border:1px solid darkgrey;"}>
          <a href={href}>{Util.formatDate(dateFormat, output.dataDate.get)}</a><br/>Missing Data
        </td>
    }
  }

  private def toTr(machine: Machine): Elem = {

    val machinePK = machine.machinePK.get

    val gapSkewHistoryList = GapSkew.historyByMachine(machinePK)

    val outputList = Output.getByMachineAndProcedure(machinePK, procedurePK = Procedure.ProcOfGapSkew.get.procedurePK.get).sortBy(_.dataDate.get.getTime).reverse

    <tr style="vertical-align:middle; border:1px solid darkgrey;">
      <td style="text-align:left; border:1px solid darkgrey;">
        <h2 aqaalias="">
          {machine.id}
        </h2>
      </td>
        {outputList.map(o => toSummary(o, gapSkewHistoryList.filter(_.output.outputPK.get == o.outputPK.get)))}
    </tr>
  }

  private def makeContent(institutionPK: Long): Elem = {

    val machineList = Machine.listMachinesFromInstitution(institutionPK).sortWith(Machine.orderMachine)

    val list = machineList.map(toTr)

    <div class="row">
      <div class="col-md-10 col-md-offset-1">
        <div class="row">
          <center><h2 style="margin-bottom:24px;">Latest Gap Skew Offset</h2></center>
        </div>
        <div class="row">
          <div class="col-md-12">
            <table class="table table-bordered" style="vertical-align:middle; border:1px solid darkgrey;">
              <tr style="vertical-align:middle; border:1px solid darkgrey;">
                <td style="vertical-align:middle; border:1px solid darkgrey;">
                  <center><h3> Machine </h3></center>
                </td>
              </tr>
              {list}
            </table>
          </div>
        </div>
      </div>
    </div>

  }

  private def content(request: Request): Elem = {
    CachedUser.get(request) match {
      case Some(user) =>
        makeContent(user.institutionPK)
      case _ => <div><h3>No data for this institution</h3></div>
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      WebUtil.respond(content(request), "Latest Gap Skew", response)
    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }

}
