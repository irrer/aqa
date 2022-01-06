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
import org.aqa.Util
import org.aqa.db.PatientProcedure
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType

import scala.xml.Elem

object PatientProcedureXml extends Logging {

  /**
    * Local cache for list of patient procedures to reduce the processing load and
    * improve response times.  These do not change often, so with caching, the server
    * can simply and quickly return the cached version instead of constructing it
    * from the database.
    *
    * Key: institutionPK
    * Value: XML text
    */
  private val cache = scala.collection.mutable.HashMap[Long, String]()

  /**
    * Get contents from cache for given institution if they are present.
    *
    * @param institutionPK Institution.
    * @return Cached XML or None.
    */
  private def cacheGet(institutionPK: Long): Option[String] =
    cache.synchronized {
      cache.get(institutionPK)
    }

  /**
    * Put the given XML text into the cache, adding it if it is not there, replacing it if it is.
    *
    * @param institutionPK Institution.
    * @param xmlText       XML in text form.
    */
  private def cachePut(institutionPK: Long, xmlText: String): Unit =
    cache.synchronized {
      cache.put(institutionPK, xmlText)
    }

  /**
    * Remove the cache entry.  If it does not exist, then do nothing.
    *
    * @param institutionPK Institution.
    */
  def cacheClear(institutionPK: Option[Long]): Unit =
    cache.synchronized {
      institutionPK match {
        case Some(pk) => cache.remove(pk)
        case _        => cache.clear()
      }
    }
}

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

  private def getXmlTextForInstitutionCached(institutionPK: Long): String = {
    PatientProcedureXml.cacheGet(institutionPK) match {
      case Some(xmlText) => xmlText
      case _ =>
        val xml = getXmlForInstitution(institutionPK)
        val xmlText = Util.prettyPrint(xml)
        PatientProcedureXml.cachePut(institutionPK, xmlText)
        xmlText
    }
  }

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(valueMap).get
      val xmlText = getXmlTextForInstitutionCached(user.institutionPK)
      response.setEntity(xmlText, MediaType.TEXT_XML)
      logger.info("Fetched PatientProcedureList size in bytes: " + xmlText.length)
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
