package org.aqa.web

import org.aqa.web.WebUtil.SubUrlAdmin
import org.aqa.Logging
import org.aqa.db.PatientProcedure
import org.aqa.web.WebUtil.getUser
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.setResponse
import org.aqa.web.WebUtil.wrapBody
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import scala.xml.Elem

object AQAClientInterface {
  private val path = new String((new ServiceInfo).pathOf)
}

class AQAClientInterface extends Restlet with SubUrlAdmin with Logging {

  private val pageTitle = "Client Interface"

  private def toRow(pp: PatientProcedure.ExtendedData): Elem = {
    val ref = s"/GetSeries?PatientID=${pp.dicomAnonymous.originalValue}"
    <tr>
      <td>
        {pp.procedure.fullName}
      </td> 
      <td>
        <a href={ref}>{WebUtil.wrapAlias(pp.dicomAnonymous.value)}</a>
      </td>
    </tr>
  }

  private def seriesContent(institutionPK: Long): Elem = {
    val ppList = PatientProcedure.listExtended(institutionPK).sortBy(pp => pp.procedure.fullName + " %% " + pp.dicomAnonymous.originalValue)
    <div style="margin-left: 40px;">
      <table class="table table-bordered">
        <thead>
          <tr>
            <td><b>Procedure</b></td>
            <td><b>Patient ID</b></td>
          </tr>
        </thead>
        {ppList.map(toRow)}
      </table>
    </div>
  }

  private def mainHtml(institutionPK: Long): Elem = {
    val content = {
      <div class="row">
        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            <h2>
              {pageTitle}
            </h2>
            <p style="width:500px; margin-bottom:40px;">
              The AQA Client gets information from the AQA server via HTTP calls that return XML formatted
              content.  This page allows the user to view this data.  The chief reason for this page is as
              a convience to developers and administrators.
            </p>
          </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1" style="margin-bottom:30px;">
            <h4>Download the configuration for all machines</h4>
            <a href="/admin/MachineXml" style="margin-left: 40px;">Machine Configuration</a>
          </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1" style="margin-bottom:30px;">
            <h4>Download List of patient procedures (associates patients with procedures)</h4>
            <a href="/admin/PatientProcedureXml" style="margin-left: 40px;">Patient Procedures</a>
          </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1" style="margin-bottom:30px;">
            <h4>Download a listing of machine log entries</h4>
            <a href="/admin/MachineLogXml" style="margin-left: 40px;">Machine Service Logs</a>
          </div>
        </div>

        <div class="row">
          <div class="col-md-5 col-md-offset-1">
            <h4>Download series information by Patient ID</h4>
            {seriesContent(institutionPK)}
          </div>
        </div>
        <p style="margin-bottom:300px;"> </p>
      </div>
    }

    content
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    val institutionPK: Long = {
      getUser(request) match {
        case Some(user) => user.institutionPK
        case _          => -1
      }
    }
    try {
      0 match {
        case _ => setResponse(wrapBody(mainHtml(institutionPK), pageTitle), response, Status.SUCCESS_OK)
      }
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }

}
