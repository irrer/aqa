package org.aqa.web

import org.aqa.Config
import org.aqa.Logging
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType

import scala.xml.Elem
import scala.xml.PrettyPrinter

class PatientProcedureXml extends Restlet with SubUrlAdmin with Logging {

  private def getXmlForInstitution(institutionPK: Long): Elem = {

    val list = PatientProcedure.listExtended(institutionPK)

    def procedureToXml(procedure: Procedure): Elem = {
      <Procedure>
        <Name>{procedure.name}</Name>
        <Version>{procedure.name}</Version>
        <URL>{Config.RootUrl}/run/{procedure.webUrl}</URL>
      </Procedure>
    }

    def ppToXml(pp: PatientProcedure.ExtendedData): Elem = {
      <PatientProcedure>
        <PatientID>{pp.dicomAnonymous.originalValue}</PatientID>
        {
        if (pp.procedure.name.toLowerCase() contains "bb by") {
          Seq(procedureToXml(Procedure.ProcOfBBbyCBCT.get), procedureToXml(Procedure.ProcOfBBbyEPID.get))
        } else
          procedureToXml(pp.procedure)
      }
      </PatientProcedure>
    }

    val elem = {
      <PatientProcedureList>
        {list.map(e => ppToXml(e))}
      </PatientProcedureList>
    }

    elem
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(valueMap).get

      val xml = getXmlForInstitution(user.institutionPK)
      val xmlText = new PrettyPrinter(1024, 2).format(xml)
      response.setEntity(xmlText, MediaType.TEXT_XML)
      logger.info("Fetched PatientProcedureList:\n" + xmlText)
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
