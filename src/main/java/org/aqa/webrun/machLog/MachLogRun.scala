package org.aqa.webrun.machLog

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

import com.pixelmed.dicom.AttributeList
import org.aqa.AnonymizeUtil
import org.aqa.db.MachineLog
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.run.ProcedureStatus
import org.aqa.run.RunProcedure
import org.aqa.run.RunReqClass
import org.aqa.run.RunTrait
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil._
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.WebRunProcedure
import org.restlet.Request
import org.restlet.Response

import java.sql.Timestamp
import scala.xml.Elem

/**
  * Add maintenance records based on machine log XML files.
  */
class MachLogRun(procedure: Procedure) extends WebRunProcedure(procedure) with RunTrait[MachLogRunReq] {

  override def getProcedure: Procedure = procedure

  override def getDataDate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[Timestamp] = {
    xmlList.flatMap(xml => MachineLog.getDateTimeSaved(xml)).sortBy(_.getTime).headOption
  }

  override def getPatientID(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Option[String] = {
    None
  }

  override def getMachineDeviceSerialNumberList(alList: Seq[AttributeList], xmlList: Seq[Elem]): Seq[String] = {
    val dsnList = xmlList.flatMap(xml => AnonymizeUtil.deviceSerialNumberInXml(xml)).distinct
    dsnList
  }

  /**
    * Make the run requirements from the attribute lists.
    */
  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], output: Option[Output]): MachLogRunReq = {
    val result = MachLogRunReq(xmlList.map(MachineLog.construct).flatten)
    result
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, MachLogRunReq] = {

    val machineLogList = xmlList.map(MachineLog.construct).flatten

    logger.info("Number of XML files uploaded: " + xmlList.size + "    Number of machine log entries: " + machineLogList.size)

    val result: Either[WebUtil.StyleMapT, MachLogRunReq] = 0 match {
      case _ if machineLogList.isEmpty                            => formError("No Machine Log files uploaded")
      case _ if machineLogList.map(_.machinePK).distinct.size > 1 => formError("Machine log entries come from more than one machine.  Only upload files from one machine at a time.")
      case _ =>
        val runReq = MachLogRunReq(machineLogList)
        Right(runReq)
    }
    result
  }

  /**
    * Determine which of the given maintenance records do not already exist in the database.
    * @param maintenanceRecordList
    * @return List of records that are not in the database.
    */
  private def findNewMaintenanceRecords(maintenanceRecordList: Seq[MaintenanceRecord]): Seq[MaintenanceRecord] = {
    if (maintenanceRecordList.isEmpty)
      Seq()
    else {
      val machinePK = maintenanceRecordList.head.machinePK
      val existingRec = MaintenanceRecord.getSet(machinePK, maintenanceRecordList.map(_.creationTime).toSet)
      def eq(a: MaintenanceRecord, b: MaintenanceRecord): Boolean = {
        (a.creationTime.getTime == b.creationTime.getTime) && a.category.equals(b.category)
      }
      val newRecList = maintenanceRecordList.filterNot(mr => existingRec.exists(e => eq(mr, e)))
      newRecList
    }
  }

  /**
    * If any of the machine log records uploaded by the client are new, then add them to the
    * database and return them.  Ignore those that are already in the database.
    * @return List of new machine logs.
    */
  private def updateMachineLogs(machineId: String, logList: Seq[MachineLog]): Seq[MachineLog] = {
    // look up existing ones by machinePK and time stamp
    val existing = MachineLog.get(logList.head.machinePK, logList.map(_.DateTimeSaved).toSet).map(_.DateTimeSaved).toSet
    val newList = logList.filter(ml => ml.hasNode && (!existing.contains(ml.DateTimeSaved)))

    if (false) { // TODO put back in
      val newInDb = newList.map(_.insert)
      logger.info("Number of new MachineLog entries inserted for " + machineId + " : " + newInDb.size)
      newInDb
    } else
      newList
  }

  override def run(extendedData: ExtendedData, runReq: MachLogRunReq, response: Response): ProcedureStatus.Value = {

    val newMachLogList = updateMachineLogs(extendedData.machine.id, runReq.machineLogList)

    val maintenanceRecordList = new MachLogMakeMaintenanceRecords(extendedData, runReq.machineLogList).makeMaintenanceRecords()

    val newMaintenceRecordList = findNewMaintenanceRecords(maintenanceRecordList)

    new MachLogHTML(extendedData, newMachLogList, runReq.machineLogList, newMaintenceRecordList, maintenanceRecordList).generate()

    ProcedureStatus.done
  }

  // override def postRun(extendedData: ExtendedData, runReq: LOC2RunReq): Unit = {}

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
