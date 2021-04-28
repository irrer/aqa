package org.aqa.web

import org.aqa.Logging
import org.aqa.db.DicomAnonymous
import org.aqa.db.DicomSeries
import org.aqa.db.Procedure
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

  private def generateXml(institutionPK: Long): Elem = {
    // list of all RTPLAN associated with Procedure
    val rtplanProcedureList = DicomSeries.rtplanProcedureList(institutionPK)

    // map of alias value --> original value
    val deAnonMap: Map[String, String] = {
      val uidSet = rtplanProcedureList.map(_.rtplanUID).toSet
      DicomAnonymous.getAttributeByValueSet(institutionPK, uidSet).map(da => (da.value, da.originalValue)).toMap
    }

    // Restrict list to those that can be de-anonymized.  This should be all of them.
    val rpList = rtplanProcedureList.filter(pp => deAnonMap.contains(pp.rtplanUID))

    // log message on some missing
    if (rtplanProcedureList.size != rpList.size)
      logger.warn("RtplanProcedureList had problems de-anonymizing values.  list size: " + rtplanProcedureList.size + "    number de-anonymized: " + rpList.size)

    /**
      * Convert one entry to XML, de-anonymizing the SOPInstanceUID.
      * @param rtplanProcedure value to convert
      * @return XML representation useful to client.
      */
    def toXml(rtplanProcedure: DicomSeries.RtplanProcedure): Elem = {

      def procedureToXml(procedure: Procedure): Elem = {
        <Run Version={procedure.version} Name={procedure.name} URL={procedure.webUrl}/>
      }

      // List of procedures that this plan can run.  Handle the special case where if it is Daily QA, then the
      // plan is associated with both BBbyCBCT and BBbyEPID.
      val procedureList = {
        val pk = rtplanProcedure.procedure.procedurePK.get
        if (
          Procedure.ProcOfBBbyEPID.isDefined &&
          Procedure.ProcOfBBbyCBCT.isDefined &&
          ((pk == Procedure.ProcOfBBbyEPID.get.procedurePK.get) || (pk == Procedure.ProcOfBBbyEPID.get.procedurePK.get))
        ) {
          Seq(Procedure.ProcOfBBbyEPID.get, Procedure.ProcOfBBbyCBCT.get)
        } else
          Seq(rtplanProcedure.procedure)
      }

      <RtplanProcedure>
        <RtplanUID>{deAnonMap(rtplanProcedure.rtplanUID)}</RtplanUID>
        {procedureList.map(procedureToXml)}
      </RtplanProcedure>
    }

    val xml = {
      <RtplanProcedureList>
        {rpList.map(toXml)}
      </RtplanProcedureList>
    }
    xml
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    try {
      val user = getUser(request)

      0 match {
        case _ if user.isEmpty => badRequest(response, "User not logged in or user can not be identified", Status.CLIENT_ERROR_BAD_REQUEST)
        case _ =>
          logger.info("Getting list of RTPLAN to Procedure associations for institutionPK" + user.get.institutionPK)
          val xml = generateXml(user.get.institutionPK)
          val xmlText = new PrettyPrinter(1024, 2).format(xml)
          response.setEntity(xmlText, MediaType.TEXT_XML)
          logger.info("Got list of RTPLAN to Procedure associations for institutionPK" + user.get.institutionPK)
      }

    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }

}
