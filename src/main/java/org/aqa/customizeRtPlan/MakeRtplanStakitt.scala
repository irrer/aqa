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

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.fixRtplanGeometry
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.orderBeamsByRenaming
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.removeVarianPrivateTagAttributes
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.replaceAllUIDs
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.setRtplanDateTimeToNow
import org.aqa.db.Machine
import org.aqa.db.Procedure

/**
  * Make RTPLAN for Stakitt.
  *
  */
class MakeRtplanStakitt extends MakeRtplan {

  override def name: String = "Stakitt"

  override def planFileProcedureName: String = "Stakitt"

  override def procedure: Procedure = Procedure.ProcOfStakitt.get

  override def makeRtplan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, procName: String): AttributeList = {

    val rtplan = DicomUtil.clone(CustomizeRtPlanUtil.getCollimatorCompatiblePlanForMachine(machine, procName).head.dicomFile.attributeList.get)

    orderBeamsByRenaming(rtplan)

    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    fixRtplanGeometry(rtplan)

    removeVarianPrivateTagAttributes(rtplan)

    rtplan
  }

}
