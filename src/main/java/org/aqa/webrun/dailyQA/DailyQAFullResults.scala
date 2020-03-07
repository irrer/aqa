package org.aqa.webrun.dailyQA

import scala.xml.Elem
import java.util.Date
import edu.umro.ScalaUtil.Util
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.Output
import org.aqa.db.Procedure
import java.text.SimpleDateFormat
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil
import org.aqa.web.OutputList

object DailyQAFullResults {

  private val timeFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss")

  def get(instPK: Long, date: Date): Elem = {

    val procedureNameSet = {
      Procedure.list.map(_.name).filter(name => name.toLowerCase.contains("cbct") || name.toLowerCase.contains("epid")).toSet
    }

    // list of CBCT and EPID outputs for this date and institution
    val extList = Output.extendedList(Some(instPK), 1000, Some(date)).
      filter(e => procedureNameSet.contains(e.procedure_name)).
      sortBy(_.input_dataDate.get.getTime)

    def extToRow(ext: Output.ExtendedValues) = {

      val url = ViewOutput.path + "?" + ViewOutput.outputPKTag + "=" + ext.output_outputPK

      <tr>
        <td><a title="View details" href={ url }>{ timeFormat.format(ext.input_dataDate.get) }</a></td>
        <td>{ ext.procedure_name }</td>
        <td>{ WebUtil.wrapAlias(ext.machine_id) }</td>
        <td><a title="Click to re-run analysis.  Results will replace previous results." href={ OutputList.path + "?" + OutputList.redoTag + "=" + ext.output_outputPK }>Redo</a></td>
      </tr>
    }

    val content = {
      <div style="margin-top:60px;">
        <center><h4>Results for Each Image Series</h4></center>
        <table class="table table-striped">
          <tbody>
            <thead>
              <tr>
                <th>Acquisition</th>
                <th>Procedure</th>
                <th>Machine</th>
                <th>Redo</th>
              </tr>
            </thead>
            { extList.map(o => extToRow(o)) }
          </tbody>
        </table>
      </div>
    }
    content
  }
}