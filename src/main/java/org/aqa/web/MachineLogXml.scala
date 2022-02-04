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

import org.aqa.AnonymizeUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.db.MachineLog
import org.aqa.web.WebUtil._
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.MediaType

import scala.xml.Elem

/**
  * Get the list of all machine log dates for the user's institution.
  *
  * This was written to support the automatic uploading of machine logs.  It allows the
  * client to see what has already been uploaded so that it can just upload new entries.
  *
  * Example of output:
  *
  *
  *   <MachineLogList>
  *     <MachineLogDateList machineId="TB5" DeviceSerialNumber="2448" NumberOfMachineLogs="6">
  *       <DateTimeSaved>2014-08-26T01:00:13</DateTimeSaved>
  *       <DateTimeSaved>2019-02-12T18:47:11</DateTimeSaved>
  *       <DateTimeSaved>2019-02-12T18:53:22</DateTimeSaved>
  *       <DateTimeSaved>2019-02-13T19:07:00</DateTimeSaved>
  *       <DateTimeSaved>2019-03-07T16:20:08</DateTimeSaved>
  *       <DateTimeSaved>2019-03-22T18:37:06</DateTimeSaved>
  *     <MachineLogDateList>
  *   <MachineLogList>
  */

class MachineLogXml extends Restlet with SubUrlAdmin with Logging {

  /**
    * Get the real device serial number for the given machine.
    * @param machine get DeviceSerialNumber for this machine.
    * @return DeviceSerialNumber de-anonymized.
    */
  private def getRealDeviceSerialNumber(machine: Machine): String = {
    machine.getRealDeviceSerialNumber match {
      case Some(serialNumber) => serialNumber
      case _                  => "unknown"
    }
  }

  /**
    * Convert a single machine to XML.
    * @param machine Machine to convert.
    * @return List of machine log dates, plus identifying information for the machine.
    */
  private def machineToXml(machine: Machine): Elem = {
    val serialNumber = getRealDeviceSerialNumber(machine)
    val machineId = machine.getRealId
    val dateList = MachineLog.getDateList(machine.machinePK.get)

    <MachineLogDateList machineId={machineId} DeviceSerialNumber={serialNumber} NumberOfMachineLogs={dateList.size.toString}>
      {
      dateList.map(d => <DateTimeSaved>{Util.standardDateFormat.format(d)}</DateTimeSaved>)
    }
    </MachineLogDateList>
  }

  /**
    * Get the list of machine logs times for each machine in the given institution as XML.
    * @param institutionPK For this institution.
    * @return XML version of list of machine log times.
    */
  private def getXmlForInstitution(institutionPK: Long): Elem = {

    val machineList = Machine.listMachinesFromInstitution(institutionPK)

    val xml = {
      <MachineLogList>
        {machineList.map(machineToXml)}
      </MachineLogList>
    }

    xml
  }

  /**
    * Handle a user request.
    * @param request HTML request.  No parameters.
    * @param response As XML.
    */
  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)
    val valueMap = getValueMap(request)
    try {
      val user = getUser(valueMap).get
      val xml = getXmlForInstitution(user.institutionPK)
      val xmlText = Util.prettyPrint(xml)
      response.setEntity(xmlText, MediaType.TEXT_XML)
      logger.info("Fetched MachineLog.   Size in bytes: " + xmlText.length)
    } catch {
      case t: Throwable =>
        WebUtil.internalFailure(response, t)
    }
  }
}
