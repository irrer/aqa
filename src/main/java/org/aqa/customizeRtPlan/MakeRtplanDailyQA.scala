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

import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.restlet.Response

class MakeRtplanDailyQA extends MakeRtplan {
  override def name: String = "Daily QA OBI"

  override def planFileProcedureName: String = "DailyQA"

  override def procedure: Procedure = Procedure.ProcOfBBbyEPID.get

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {
    super.showPlan(machine, userPK, planSpecification, response)
  }
}
