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
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.removeBeamFromPlan
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.removeVarianPrivateTagAttributes
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.replaceAllUIDs
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.setNumberOfBeamsInFractionGroupSequence
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.setRtplanDateTimeToNow
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.restlet.Response

abstract class MakeRtplanIsoCheck extends MakeRtplan {

  override def name: String // = "IsoCheck Full Table"

  override def planFileProcedureName: String = "IsoCheck"

  override def procedure: Procedure = Procedure.ProcOfWinstonLutz.get

  protected val beamRemovalList: Seq[String]

  override def makeRtplan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, procName: String): AttributeList = {

    val rtplan = DicomUtil.clone(CustomizeRtPlanUtil.getCollimatorCompatiblePlanForMachine(machine, procName).head.dicomFile.attributeList.get)

    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    fixRtplanGeometry(rtplan)

    removeVarianPrivateTagAttributes(rtplan)

    beamRemovalList.foreach(beamName => removeBeamFromPlan(rtplan, beamName))

    setNumberOfBeamsInFractionGroupSequence(rtplan)

    rtplan
  }

  /*
   */

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {
    super.showPlan(machine, userPK, planSpecification, response)
  }
}
