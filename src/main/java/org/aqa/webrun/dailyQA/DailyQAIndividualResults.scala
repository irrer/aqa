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

package org.aqa.webrun.dailyQA

import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.web.OutputList
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
  * Report the individual results for all EPID and CBCT analysis on the given day.  This
  * is generally more of a diagnostic tool than for daily use.
  */
object DailyQAIndividualResults {

  private val dateFormat = new SimpleDateFormat("EEE MMM dd, YYYY")
  private val timeFormat = new SimpleDateFormat("H:mm:ss a")
  private val dateAndTimeFormat = new SimpleDateFormat("EEE MMM dd, YYYY H:mm:ss a")

  def get(instPK: Long, date: Date): Elem = {

    val procedureNameSet = {
      Procedure.list.map(_.name).filter(name => name.toLowerCase.contains("cbct") || name.toLowerCase.contains("epid")).toSet
    }

    // list of CBCT and EPID outputs for this date and institution
    val extList =
      Output
        .extendedList(Some(instPK), 1000, Some(date))
        .filter(e => procedureNameSet.contains(e.procedure_name))
        .sortBy(_.input_dataDate.get.getTime) // this determines the order that rows appear.  Maybe should be: <code>sortBy(_.output_startDate.getTime)</code> instead?

    def extToRow(ext: Output.ExtendedValues) = {

      val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + ext.output_outputPK

      <tr>
        <td>
          <a title="View details" href={url}>
            {dateAndTimeFormat.format(ext.output_startDate)}
          </a>
        </td>
        <td>
          {timeFormat.format(ext.input_dataDate.get)}
        </td>
        <td>
          {ext.procedure_name}
        </td>
        <td>
          {WebUtil.wrapAlias(ext.machine_id)}
        </td>
        <td>
          {OutputList.redoUrl(ext.output_outputPK)}
        </td>
      </tr>
    }

    val content = {
      <div style="margin-top:60px;">
        <center>
          <h4>Results for Each Image Series
            {dateFormat.format(date)}
          </h4>
        </center>
        <table class="table table-striped">
          <tbody>
            <thead>
              <tr>
                <th>Analysis</th>
                <th>Acquisition</th>
                <th>Procedure</th>
                <th>Machine</th>
                <th>Redo</th>
              </tr>
            </thead>{extList.map(o => extToRow(o))}
          </tbody>
        </table>
      </div>
    }
    content
  }
}
