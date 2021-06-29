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

package org.aqa.web

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

  /**
    * Get the patient procedure list for the given institution as XML.
    * @param institutionPK For this institution.
    * @return XML version of patient procedure list.
    */
  private def getXmlForInstitution(institutionPK: Long): Elem = {

    val list = PatientProcedure.listExtended(institutionPK)

    def procedureToXml(procedure: Procedure): Elem = {
      <Procedure>
        <Name>{procedure.name}</Name>
        <Version>{procedure.version}</Version>
        <URL>/run/{procedure.webUrl}</URL>
      </Procedure>
    }

    def ppToXml(pp: PatientProcedure.ExtendedData): Elem = {
      <PatientProcedure>
        <PatientID>{pp.dicomAnonymous.originalValue}</PatientID>
        {
        0 match {
          case _ if pp.procedure.name.toLowerCase() contains "bb by" =>
            Seq(procedureToXml(Procedure.ProcOfBBbyCBCT.get), procedureToXml(Procedure.ProcOfBBbyEPID.get))
          case _ if pp.procedure.name.toLowerCase() contains "loc" =>
            Seq(procedureToXml(Procedure.ProcOfLOC.get), procedureToXml(Procedure.ProcOfLOCBaseline.get))
          case _ =>
            procedureToXml(pp.procedure)
        }
      }
      </PatientProcedure>
    }

    val elem = {
      <PatientProcedureList>
        {list.filter(_.patientProcedure.active).map(e => ppToXml(e))}
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
      logger.info("Fetched PatientProcedureList size in bytes: " + xmlText.length)
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
