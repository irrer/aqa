/*
 * Copyright 2023 Regents of the University of Michigan
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
import org.aqa.db.EPID
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MultileafCollimator
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType

import scala.xml.Elem

/**
  * Provide a way for a client to get an XML version of the treatment machines.
  *
  * Users get the subset of results of machines items that apply to their
  * institution.  Results are cached for efficiency.
  *
  * @author Jim Irrer irrer@med.umich.edu
  */
object MachineXml extends Logging {

  /**
    * Local cache for list of machines to reduce the processing load and
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
  def clearCache(institutionPK: Option[Long]): Unit =
    cache.synchronized {
      institutionPK match {
        case Some(pk) => cache.remove(pk)
        case _        => cache.clear()
      }
    }
}

class MachineXml extends Restlet with SubUrlAdmin with Logging {

  /**
    * Get the machine list for the given institution as XML.
    * @param institutionPK For this institution.
    * @return XML version of machine list.
    */
  private def getXmlForInstitution(institutionPK: Long): Elem = {

    def machineToXml(machine: Machine): Elem = {

      logger.info(s"Constructing XML from database for machine ${machine.id}")

      val realDeviceSerialNumber = machine.getRealDeviceSerialNumber

      <Machine>
        <AnonymousId>{machine.id}</AnonymousId>
        <Id>{machine.getRealId}</Id>
        <Collimator>{MultileafCollimator.get(machine.multileafCollimatorPK).get.model}</Collimator>
        <EPID>{EPID.get(machine.epidPK).get.model}</EPID>
        {if (realDeviceSerialNumber.nonEmpty) <SerialNumber>{realDeviceSerialNumber.get}</SerialNumber>}  
        <ImagingBeam2_5_mv>{machine.imagingBeam2_5_mv}</ImagingBeam2_5_mv>
        <OnboardImager>{machine.onboardImager}</OnboardImager>
        <Table6DOF>{machine.table6DOF}</Table6DOF>
        <RespiratoryManagement>{machine.respiratoryManagement}</RespiratoryManagement>
        <DeveloperMode>{machine.developerMode}</DeveloperMode>
        <Active>{machine.active}</Active>
        {if (machine.getRealTpsId.nonEmpty) <TpsID>{machine.getRealTpsId.get}</TpsID>}
        <Notes>{machine.getRealNotes}</Notes>
      </Machine>
    }

    /**
      * Make a list of all machines.
      * @return List of machines as XML.
      */
    val elem: Elem = {
      <MachineList>
        {Machine.getForInstitution(institutionPK).sortBy(_.getRealId).map(machineToXml)}
      </MachineList>
    }

    elem
  }

  private def getXmlTextForInstitutionCached(institutionPK: Long): String = {
    MachineXml.cacheGet(institutionPK) match {
      case Some(xmlText) => xmlText
      case _ =>
        val xml = getXmlForInstitution(institutionPK)
        val xmlText = Util.prettyPrint(xml)
        MachineXml.cachePut(institutionPK, xmlText)
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
      logger.info(s"Fetched XML MachineList size in bytes: ${xmlText.length}  Institution: ${Institution.get(user.institutionPK).get.name}")
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
