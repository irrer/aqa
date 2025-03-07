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
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.restlet.Response

abstract  class MakeRtplanIsoCheck extends MakeRtplan {

  /**
    * Remove the referenced structure set, if there is one.
    *
    * @param rtplan Modify this plan.
    */
  private def removeReferencedStructureSetSequence(rtplan: AttributeList): Unit = {
    val attr = rtplan.get(TagByName.ReferencedStructureSetSequence)
    if (attr != null) {
      rtplan.remove(TagByName.ReferencedStructureSetSequence)
    }
  }

  override def name: String // = "IsoCheck Full Table"

  override def planFileProcedureName: String = "IsoCheck"

  override def procedure: Procedure = Procedure.ProcOfWinstonLutz.get

  protected val beamRemovalList: Seq[String] // = Seq()

  /**
    * Remove the references to patient setup.
    * @param rtplan From this plan.
    */
  private def removeReferencesToPatientSetupNumber(rtplan: AttributeList): Unit = {
    val beamAlList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)
    beamAlList.map(beamAl => beamAl.remove(TagByName.ReferencedPatientSetupNumber))
  }

  override def makeRtplan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, procName: String): AttributeList = {
    val rtplan = super.makeRtplan(machine, userPK, planSpecification, procName)
    removeReferencedStructureSetSequence(rtplan)
    rtplan.remove(TagByName.DoseReferenceSequence)
    removeReferencesToPatientSetupNumber(rtplan)
    beamRemovalList.foreach(beamName => CustomizeRtPlanUtil.removeBeamFromPlan(rtplan, beamName))
    CustomizeRtPlanUtil.orderBeamsByRenaming(rtplan)
    rtplan
  }

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {
    super.showPlan(machine, userPK, planSpecification, response)
  }
}
