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
import org.aqa.Util
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
    val dsnList = xmlList.flatMap(xml => Util.machineLogSerialNumber(xml)).distinct
    dsnList
  }

  /**
    * Make the run requirements from the attribute lists.
    */
  override def makeRunReqForRedo(alList: Seq[AttributeList], xmlList: Seq[Elem], output: Option[Output]): MachLogRunReq = {
    val runReq = MachLogRunReq(xmlList)
    runReq
  }

  /**
    * Validate inputs enough so as to avoid trivial input errors and then organize data to facilitate further processing.
    */
  override def validate(valueMap: ValueMapT, alList: Seq[AttributeList], xmlList: Seq[Elem]): Either[StyleMapT, MachLogRunReq] = {

    val machList = xmlList.flatMap(Util.machineLogSerialNumber).distinct

    logger.info("Number of XML files uploaded: " + xmlList.size + "    Number of machine log entries: " + machList.size)

    val result: Either[WebUtil.StyleMapT, MachLogRunReq] = 0 match {
      case _ if machList.isEmpty  => formError("No Machine Log files uploaded")
      case _ if machList.size > 1 => formError("Machine log entries come from more than one machine.  Only upload files from one machine at a time.")
      case _ =>
        val runReq = MachLogRunReq(xmlList)
        Right(runReq)
    }
    result
  }

  /**
    * Determine which of the given maintenance records do not already exist in the database.
    * @param maintenanceRecordList List of uploaded maintenance records.
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
    *
    * @param uploadedMachLogList List of machine logs that the user uploaded.
    * @return List of uploaded machine logs that are not in the database.
    */
  private def findNewMachineLogs(uploadedMachLogList: Seq[MachineLog]): Seq[MachineLog] = {
    // look up existing ones by machinePK and time stamp
    val existing = MachineLog.get(uploadedMachLogList.head.machinePK, uploadedMachLogList.map(_.DateTimeSaved).toSet).map(_.DateTimeSaved).toSet
    val newList = uploadedMachLogList.filterNot(ml => existing.contains(ml.DateTimeSaved))
    newList
  }

  override def run(extendedData: ExtendedData, runReq: MachLogRunReq, response: Response): ProcedureStatus.Value = {

    val machinePK = extendedData.machine.machinePK.get

    // List of all uploaded machine logs.  Some or all may already be in the database.  These each have null as their primary keys.
    val uploadedMachLogList = runReq.machineLogList.flatMap(e => MachineLog.construct(e, extendedData.output.outputPK.get))

    val uploadedTimeSet = uploadedMachLogList.map(_.DateTimeSaved).toSet

    // List of machine logs that the user uploaded but already exist in the DB.
    val oldMachLogList = MachineLog.get(machinePK, uploadedTimeSet)

    val newMachLogList = {
      val existingTimeSet = oldMachLogList.map(_.DateTimeSaved.getTime).map(t => new Timestamp(t)).toSet
      val list = uploadedMachLogList.filterNot(ml => existingTimeSet.contains(ml.DateTimeSaved))
      logger.info("Inserting " + list.size + " new machine log entries for machine " + extendedData.machine.id)
      list.map(_.insert)
    }

    val userPK = extendedData.user.userPK.get
    val outputPK = extendedData.output.outputPK.get

    val oldMaintenanceRecordList = MaintenanceRecord.getSet(machinePK, uploadedTimeSet)

    val newMaintenanceRecordList = {
      val uploadedMachLogMaintenanceRecordList = (oldMachLogList ++ newMachLogList).flatMap(ml => MachLogMakeMaintenanceRecord.makeMaintenanceRecordList(ml, userPK = userPK, outputPK = outputPK))
      def isOld(old: MaintenanceRecord, mr: MaintenanceRecord): Boolean = {
        (old.creationTime == mr.creationTime) &&
        (old.machineLogNodeIndex.isDefined) &&
        (old.machineLogNodeIndex.get == mr.machineLogNodeIndex.get)
      }
      val newList = uploadedMachLogMaintenanceRecordList.filterNot(mr => oldMaintenanceRecordList.exists(old => isOld(old, mr)))
      logger.info("Inserting " + newList.size + " new machine maintenance record entries for machine " + extendedData.machine.id)
      newList.map(_.insert)
    }

    new MachLogHTML(extendedData, newMachLogList, oldMachLogList, newMaintenanceRecordList, oldMaintenanceRecordList).generate()

    ProcedureStatus.done
  }

  // override def postRun(extendedData: ExtendedData, runReq: LOC2RunReq): Unit = {}

  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)
    RunProcedure.handleInput(valueMap, response, this.asInstanceOf[RunTrait[RunReqClass]], authenticatedUserPK = None, sync = true)
  }

}
