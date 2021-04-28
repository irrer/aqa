package org.aqa.web

import org.aqa.Logging
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType
import org.restlet.data.Status

import scala.xml.Elem
import scala.xml.PrettyPrinter

/**
  * Generate XML that lists DICOM series associated with the given patient ID.
  */
class RtplanProcedureList extends Restlet with SubUrlRoot with Logging {

  private val PatientIDTag = "PatientID"

  private def generateXml(institutionPK: Long): Elem = {
    <div>{???}</div>
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(request)

      0 match {
        case _ if user.isEmpty => badRequest(response, "User not logged in or user can not be identified", Status.CLIENT_ERROR_BAD_REQUEST)
        case _ => {
          logger.info("Getting list of RTPLAN to Procedure associations for institutionPK" + user.get.institutionPK)
          val xml = generateXml(user.get.institutionPK)
          val xmlText = new PrettyPrinter(1024, 2).format(xml)
          response.setEntity(xmlText, MediaType.TEXT_XML)
          logger.info("Got list of RTPLAN to Procedure associations for institutionPK" + user.get.institutionPK)
        }
      }

    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }

}
