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

class DataCollectionSummary extends Restlet with SubUrlRoot {

  def quote(text: String) = "'" + text + "'"

  private def fmtDate(date: Date): String = quote(Util.standardDateFormat.format(date))

  private def makeZeroTable(instList: Seq[Institution], machList: Seq[Machine], outputList: Seq[Output]): Elem = {

    def instOfOutput(output: Output) = {
      val j = machList.find(mach => mach.machinePK.get == output.machinePK.get)
      j.get.institutionPK
    }

    val haveData = outputList.map(output => instOfOutput(output)).distinct
    val noData = instList.map(_.institutionPK.get).diff(haveData).map(instPK => instList.find(inst => inst.institutionPK.get == instPK)).flatten.map(inst => inst.name_real).flatten

    <div>
      <h3>List of Institutions with No Data</h3>
      <table>
        { noData.map(instName => <tr><td>{ instName }</td></tr>) }
      </table>
    </div>
  }

  private def makeMainChart(instList: Seq[Institution], machList: Seq[Machine], outputList: Seq[Output]): String = {

    case class Row(name: String, data: Seq[String], index: Int) {
      def toXs = quote(name) + ": " + quote(name + "Data") + ","
      def toColumn = {
        "[" + quote(name) + "," + data.mkString(",") + "],\n" +
        "[" + quote(name + "Data") + "," + data.map(_ => index).mkString(",") + "]"
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
var chart = c3.generate({
    data: {
        xs: {
            """ + rowList.map(row => row.toXs).mkString(",\n") +  """ 
        },
        columns: [
            """ + rowList.map(row => row.toColumn).mkString(",\n") +  """ 
        ]
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

    val zeroTable = makeZeroTable(instList, machList, outputList)
    val mainChart = makeMainChart(instList, machList, outputList)

    val content = {
      <div>
        { zeroTable }
        <h3>Main Chart</h3>
        <pre>
          { mainChart }
        </pre>
      </div>
    }

    simpleWebPage(content, Status.SUCCESS_OK, "Data Collection Summary", response)
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
