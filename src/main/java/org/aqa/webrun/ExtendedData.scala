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

import org.aqa.db.EPID
import org.aqa.db.Input
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.MachineType
import org.aqa.db.MultileafCollimator
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.web.OutputHeading

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
) {

  /**
    * Convenience method.  Will throw an exception if output.outputPK is None.
    * @return outputPK
    */
  def outputPK: Long = output.outputPK.get
}

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

  def wrapExtendedData(extendedData: ExtendedData, content: Elem, offset: Int = 1): Elem = {

    val wrapWithHeader = {
      <div class="row">
        {OutputHeading.reference(extendedData.outputPK)}
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
