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

package org.aqa.customizeRtPlan

import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.web.WebUtil
import org.restlet.Response

class MakeRtplanPhase2 extends MakeRtplan {
  override def name: String = "Phase2"

  override def planFileProcedureName: String = "Phase2"

  override def procedure: Procedure = Procedure.ProcOfPhase2.get

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {

    val rtplan = makeRtplan(machine, userPK, planSpecification)

    val machineEnergyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)

    CustomizeRtPlanUtil.reassignPlanEnergies(rtplan, machineEnergyList)

    val elem = {
      <div>
        <span>
          <h4>
            <p/>
            Preview
            {name}
          </h4> <p/> <pre title="DICOM meta-data">
          {WebUtil.nl + DicomUtil.attributeListToString(rtplan)}
        </pre>
        </span>
      </div>
    }

    val file = makeDownloadFile(name, "dcm")
    DicomUtil.writeAttributeListToFile(rtplan, file, "AQA")

    Download(elem, file)

  }
}
