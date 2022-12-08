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

package org.aqa.webrun

import org.aqa.Util
import org.aqa.db.EPID
import org.aqa.db.Input
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.web.MachineUpdate
import org.aqa.web.OutputList

import java.text.SimpleDateFormat
import scala.xml.Elem

/**
  * Convenience class for passing around commonly used data, which is basically all of
  * the database objects associated with a given Output object.
  */
case class ExtendedData(
    output: Output,
    input: Input,
    machine: Machine,
    machineType: MachineType,
    multileafCollimator: MultileafCollimator,
    epid: EPID,
    institution: Institution,
    procedure: Procedure,
    user: User
) {}

object ExtendedData {

  def get(output: Output): ExtendedData = {

    val input = Input.get(output.inputPK).get

    val machine = Machine.get(output.machinePK.get).get
    val machineType = MachineType.get(machine.machineTypePK).get
    val multileafCollimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val epid = EPID.get(machine.epidPK).get

    val institution = Institution.get(machine.institutionPK).get
    val procedure = Procedure.get(output.procedurePK).get
    val user = User.get(output.userPK.get).get

    new ExtendedData(output, input, machine, machineType, multileafCollimator, epid, institution, procedure, user)
  }

  def wrapExtendedData(extendedData: ExtendedData, content: Elem, offset: Int = 1) = {
    val twoLineDate = new SimpleDateFormat("MMM dd yyyy\nHH:mm")
    def wrapElement(col: Int, name: String, value: String, asAlias: Boolean): Elem = {
      val html =
        if (asAlias) {
          <span aqaalias="">{value}</span>
        } else {
          val valueList = value.split("\n");
          { <span>{valueList.head}{valueList.tail.map(line => { <span><br/> {line} </span> })}</span> }
        }

      { <div class={"col-md-" + col}><em>{name}:</em><br/>{html}</div> }

    }

    val dataAcquisitionDate = {
      if (extendedData.output.dataDate.isDefined) twoLineDate.format(extendedData.output.dataDate.get)
      else "unknown"
    }

    val elapsed: String = {
      val fin = extendedData.output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - extendedData.output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val showMachine = {
      val href = "/admin/MachineUpdate?machinePK=22"
      <div class="col-md-1">
        <h2 title="Treatment machine.  Click for details.">{MachineUpdate.linkToMachineUpdate(extendedData.machine.machinePK.get, extendedData.machine.id)}</h2>
      </div>
    }

    def wrapWithHeader = {
      <div class="row">
        <div class="row">
          <div class="col-md-10 col-md-offset-1">
            {showMachine}
            {wrapElement(2, "Institution", extendedData.institution.name, true)}
            {wrapElement(1, "Data Acquisition", dataAcquisitionDate, false)}
            {wrapElement(1, "Analysis Started", twoLineDate.format(extendedData.output.startDate), false)}
            {wrapElement(1, "User", extendedData.user.id, true)}
            {wrapElement(1, "Elapsed", elapsed, false)}
            {wrapElement(1, "Procedure", procedureDesc, false)}
            <div class="col-md-1">{OutputList.redoUrl(extendedData.output.outputPK.get)}</div>
          </div>
        </div>
        <div class="row">
          <div class={"col-md-10 col-md-offset-" + offset}>
            {content}
          </div>
        </div>
      </div>
    }

    wrapWithHeader
  }
}
