package org.aqa.web

import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.aqa.web.WebUtil._
import org.aqa.db.Institution
import org.aqa.AnonymizeUtil
import org.aqa.db.Machine
import org.restlet.data.Status
import org.aqa.db.Output
import org.aqa.db.Procedure
import scala.xml.Elem
import org.aqa.Util
import java.util.Date
import java.text.SimpleDateFormat
import java.sql.Timestamp

//import org.restlet.security.Verifier
//import org.restlet.security.ChallengeAuthenticator
//import org.restlet.data.ChallengeScheme
//import org.restlet.data.ChallengeRequest
//import org.restlet.data.ChallengeResponse
//import org.restlet.Context
//import org.aqa.db.User
//import org.restlet.data.Status
//import scala.xml.Elem
//import java.net.URLDecoder
//import org.restlet.engine.security.AuthenticatorHelper
//import org.restlet.engine.security.HttpBasicHelper
//import org.restlet.engine.header.ChallengeWriter
//import org.restlet.data.Header
//import org.restlet.util.Series
//import org.aqa.Util
//import org.aqa.db.UserRole
//import org.aqa.db.CachedUser
//import org.aqa.Crypto
//import org.aqa.Config

object DataCollectionSummary {
  val path = "/DataCollectionSummary"
  val messageTag = "Message"
}

class DataCollectionSummary extends Restlet with SubUrlAdmin {

  def quote(text: String) = "'" + text + "'"

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  def fmt(date: Date) = dateFormat.format(date)

  private def fmtDate(date: Date): String = quote(fmt(date))

  private def makeCountTable(instList: Seq[Institution], machList: Seq[Machine], outputList: Seq[Output]): Elem = {

    val instMap = instList.map(inst => (inst.institutionPK.get, inst)).toMap

    /**
     * Given an output, return the institutionPK.
     */
    def outputToInst(output: Output) = {
      machList.find(mach => mach.machinePK.get == output.machinePK.get).get.institutionPK
    }

    // map of institutionPKs to their data
    val instOutputMap = outputList.groupBy(output => outputToInst(output))

    def outputCount(instPK: Long) = {
      instOutputMap.get(instPK) match {
        case Some(outList) => outList.size
        case _ => 0
      }
    }

    def timeOfMostRecent(instPK: Long) = {
      instOutputMap.get(instPK) match {
        case Some(outList) => outList.sortBy(_.dataDate.get.getTime).last.dataDate.get.getTime
        case _ => 0
      }
    }
    
    /**
     * Convert an institution to a row in the table.
     */
    def instToRow(inst: Institution): Elem = {
      val nameElem = { <td>{ inst.name_real.get }</td> }
      val outList = instOutputMap.get(inst.institutionPK.get)
      val count = outputCount(inst.institutionPK.get)
      val countElem = { <td>{ count.toString }</td> }
      val latest: Option[Timestamp] = if (outList.isDefined) outList.get.sortBy(o => o.dataDate.get.getTime).last.dataDate else None
      val latestElem = { <td>{ if (latest.isDefined) WebUtil.timeAgo(latest.get) else "NA" }</td> }
      <tr>{ nameElem }{ countElem }{ latestElem }</tr>
    }

    val instSortedByMostRecent = instList.map(inst => (inst, timeOfMostRecent(inst.institutionPK.get))).sortBy(_._2).map(_._1).reverse

    <div class="col-md-4 col-md-offset-2">
      <h3>List of Institutions</h3>
      <table class="table table-bordered">
        <thead>
          <tr>
            <th title="Name of Institution">Name</th>
            <th title="Number of Phase 2 Data Sets">Phase 2 Count</th>
            <th title="The date of the most recent data acquistion.">Most Recent</th>
          </tr>
        </thead>
        { instSortedByMostRecent.map(inst => instToRow(inst)) }
      </table>
    </div>
  }

  private def makeMainChart(instList: Seq[Institution], machList: Seq[Machine], outputList: Seq[Output]): String = {

    case class Row(name: String, data: Seq[String], index: Int) {
      def toXs = quote(name) + ": " + quote(name + "Data")
      def toColumn = {
        val allX = quote(name + "Data") +: data
        val allY = quote(name) +: data.map(_ => index.toString)
        def bracket(seq: Seq[String]) = seq.mkString("[", ",", "]")
        bracket(allX) + ",\n" + bracket(allY)
      }
    }

    def rowInfo(mach: Machine, index: Int): Row = {

      val outList = outputList.filter(out => out.machinePK.get == mach.machinePK.get).sortBy(_.dataDate.get.getTime)
      val textList = outList.map(out => fmtDate(out.dataDate.get))

      val name = {
        val inst = instList.find(inst => inst.institutionPK.get == mach.institutionPK).get
        val nm = inst.name_real.get + " : " + mach.id_real.get + " : " + textList.size
        nm
      }

      new Row(name, textList, index)
    }

    val ml = machList.groupBy(_.institutionPK).map(im => im._2).flatten
    val rowList = ml.zipWithIndex.map(machIndex => rowInfo(machIndex._1, machIndex._2))

    val script =

      """
var SummaryChart = c3.generate({
    data: {
        xs: {
            """ + rowList.map(row => row.toXs).mkString(",\n") + """ 
        },
        columns: [
            """ + rowList.map(row => row.toColumn).mkString(",\n") + """ 
        ]
    },
  bindto : '#SummaryChart',
//  size: {
//    height: 800
//  },
  axis: {
    x: {
      type: 'timeseries',
      min: '2019-04-01',
      //max: '2019-12-31',
    tick: { format: '%Y-%m-%d' },
    },
    y: {
      // min: 2,
      // max: 30
    }
  }
});

"""

    script
  }

  private def generateReport(response: Response) = {
    // get institutions and machine as clear text
    val instList = Institution.list.map(inst => inst.copy(name_real = Some(AnonymizeUtil.decryptWithNonce(inst.institutionPK.get, inst.name_real.get))))
    val machList = Machine.list.map(mach => mach.copy(id_real = Some(AnonymizeUtil.decryptWithNonce(mach.institutionPK, mach.id_real.get))))

    val phase2PK = Procedure.list.find(proc => proc.name.toLowerCase.contains("phase2")).get.procedurePK.get

    val outputList = Output.list.filter(output => output.procedurePK == phase2PK && output.machinePK.isDefined)

    val mainChart = makeMainChart(instList, machList, outputList)

    val content = {
      <div>
        <div class="row">
          <div class="col-md-6 col-md-offset-2">
            <h2>Phase 2 Collection Summary</h2>
            { (new Date).toString }
            <br/>
            The graph below shows when data was collected for each of the respective institutions and machine.<br/>
            Hover over each dot to show<i>Inst Name : Machine : Number of Data Sets</i>
            .<br/>
            The vertical scale is meaningless, and is only used to separate the different institutions vertically.  Use the image snippet at the bottom of this web page as a guide.<br/>
          </div>
        </div>
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            <h3>Summary Chart</h3>
            <div id="SummaryChart" style="height: 800px;"></div>
          </div>
        </div>
        <div class="row">
          <div>
            { makeCountTable(instList, machList, outputList) }
          </div>
        </div>
        <div class="row">
          <div class="col-md-8 col-md-offset-2">
            <hr/>
            <h4>Image snippet showing what the values shown on mouse hover mean:</h4>
            <div style="margin:30px; border: 1px solid lightgrey;">
              <img style="margin:30px;" src="/static/images/DataCollectionSummaryDetail.png" height="342"/>
            </div>
          </div>
        </div>
      </div>
    }

    val refresh = None
    val c3 = true
    val htmlText = wrapBody(content, "Data Collection Summary", refresh, c3, Some("<script>\n" + mainChart + "\n</script>"))
    setResponse(htmlText, response, Status.SUCCESS_OK)
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    try {
      val valueMap = getValueMap(request)
      if (userIsWhitelisted(request)) {
        generateReport(response)
      } else
        badRequest(response, "You are not authorized to view this page", Status.CLIENT_ERROR_FORBIDDEN)
    } catch {
      case t: Throwable => {
        WebUtil.internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

}
