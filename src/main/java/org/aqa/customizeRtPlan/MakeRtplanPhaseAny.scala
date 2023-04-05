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

package org.aqa.customizeRtPlan

import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Machine
import org.restlet.Response

abstract class MakeRtplanPhaseAny extends MakeRtplan {

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {

    val rtplan = makeRtplan(machine, userPK, planSpecification)

    val machineEnergyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)

    CustomizeRtPlanUtil.reassignPlanEnergies(rtplan, machineEnergyList)
    saveDicomToDatabase(machine, userPK, rtplan, planSpecification, procedure)
    val elem = {
      <div>
        {dicomToElem(rtplan, name)}
      </div>
    }

    val file = makeDicomDownloadFile(name)
    DicomUtil.writeAttributeListToFile(rtplan, file, "AQA")

    Download(elem, file)

  }
}
