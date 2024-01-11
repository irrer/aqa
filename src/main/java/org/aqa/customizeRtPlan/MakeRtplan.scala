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
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.FormButton
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Config
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.removeVarianPrivateTagAttributes
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.replaceAllUIDs
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.saveAnonymizedDicom
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.setRtplanDateTimeToNow
import org.aqa.customizeRtPlan.CustomizeRtPlanUtil.setupPatientProcedure
import org.aqa.db.MultileafCollimator
import org.aqa.web.MachineUpdate
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.styleNone
import org.aqa.Util
import org.restlet.Response

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

abstract class MakeRtplan extends Logging {

  protected def name: String

  protected def planFileProcedureName: String
  protected def procedure: Procedure

  def fileDateText: String = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'_'HH-mm")
    dateFormat.format(new Date)
  }

  // protected def hdPlanFileConfig: Seq[Config.PlanFileConfig] = Config.PlanFileList.filter(pf => pf.procedure.equals(planFileProcedureName) && pf.collimatorModel.toLowerCase.contains("hd"))

  // protected def milPlanFileConfig: Seq[Config.PlanFileConfig] = Config.PlanFileList.filter(pf => pf.procedure.equals(planFileProcedureName) && pf.collimatorModel.toLowerCase.contains("mil"))

  private val buttonLabel = s"Create $name"

  def makeButton: FormButton = {
    new FormButton(buttonLabel, 2, 0, CustomizeRtPlanInterface.interface.subUrl, CustomizeRtPlanInterface.interface.pathOf, WebUtil.ButtonType.BtnPrimary)
  }

  def buttonIs(valueMap: ValueMapT): Boolean = {
    valueMap.values.exists(_ == buttonLabel)
  }

  /**
    * Get the template rtplan for the given machine matching the given pattern.  If none exists, return None, which
    * can happen if the system does not have such a plan configured.  This informs the user
    * interface so that it can tell the user.
    */
  private def getCollimatorCompatiblePlanForMachine(machine: Machine, procName: String): Seq[Config.PlanFileConfig] = {
    val collimator = MultileafCollimator.get(machine.multileafCollimatorPK).get
    val planFileList = Config.PlanFileList.filter(pf =>
      pf.procedure.equalsIgnoreCase(procName.toLowerCase()) &&
        pf.manufacturer.equalsIgnoreCase(collimator.manufacturer) &&
        pf.collimatorModel.equalsIgnoreCase(collimator.model)
    )
    planFileList
  }

  /**
    * Determine if the configuration is valid.  Master RTPLAN files should be there.
    * @param valueMap User entered parameters.
    * @return
    */
  def validate(valueMap: ValueMapT, procName: String = planFileProcedureName): StyleMapT = {
    val machine = Machine.get(valueMap(MachineUpdate.machinePKTag).toLong).get

    val colList = getCollimatorCompatiblePlanForMachine(machine, procName)

    val badList = colList.filterNot(_.file.isFile)
    val badText = badList.map(bad => s"Configuration problem.  Contact system administrator.  Master RTPLAN file ${bad.file.getName} does not exist.").mkString(WebUtil.titleNewline)

    if (badList.isEmpty) {
      styleNone
    } else {
      WebUtil.Error.make(makeButton, badText)
    }
  }

  /**
    * Utility to show RTPLAN files.
    * @param rtplan RTPLAN DICOM
    * @param dicomName Name to show to user.
    * @return
    */
  def dicomToElem(rtplan: AttributeList, dicomName: String): Elem = {
    <span>
      <h4>
        <p/>
        Preview {dicomName}
      </h4>
      <p/>
      <pre title="DICOM meta-data">
        {WebUtil.nl + DicomUtil.attributeListToString(rtplan)}
      </pre>
    </span>
  }

  /**
    * Given all the required information, create a single rtplan that is compatible with the given machine.
    *
    * Note: This covers the simple case of making a single plan with no changes to beams.  If multiple plans
    * are needed or beam changes are needed, then this * method needs to be overridden.
    *
    * @param machine For this machine.
    * @param userPK User that requested it.
    * @param planSpecification Parameters that user specified.
    * @return A single RTPLAN.
    */
  def makeRtplan(
      machine: Machine,
      userPK: Long,
      planSpecification: PlanSpecification,
      procName: String = planFileProcedureName
  ): AttributeList = {

    val rtplan = DicomUtil.clone(getCollimatorCompatiblePlanForMachine(machine, procName).head.dicomFile.attributeList.get)

    replaceAllUIDs(rtplan) // change UIDs so that this plan will be considered new and unique from all others.

    planSpecification.setOverrides(rtplan)

    setRtplanDateTimeToNow(rtplan)

    removeVarianPrivateTagAttributes(rtplan)

    rtplan
  }

  /**
    * After the RTPLAN has been created for the user, it needs to be put in the database so it is there when the user runs the procedure.
    * As a convenience, it also adds the patient ID to the PatientProcedure list.
    * @param machine For this machine.
    * @param userPK User that is creating plan.
    * @param rtplan Plan that has been created.
    * @param planSpecification Plan parameters specified by user.
    * @param procedure Associated procedure.
    */
  def saveDicomToDatabase(
      machine: Machine,
      userPK: Long,
      rtplan: AttributeList,
      planSpecification: PlanSpecification,
      procedure: Procedure
  ): Unit = {
    val anonDicom = saveAnonymizedDicom(machine, userPK, rtplan, procedure.procedurePK)
    setupPatientProcedure(machine.institutionPK, anonDicom.get(TagByName.PatientID), Some(procedure))
  }

  protected def makeDicomDownloadFile(typeName: String): File = {

    val dateText = fileDateText
    val typeText = FileUtil.replaceInvalidFileNameCharacters(typeName.replace(' ', '_'), '_')
    val fileName = s"RTPLAN_${typeText}_$dateText.dcm"
    val file = new File(Config.tmpDirFile, fileName)
    file
  }

  /**
    * Make an RTPLAN, display it to the user, and allow then to download it.
    *
    * If more than one plan is needed of if changes to beams are needed then override this method.
    *
    * @param machine           For this machine.
    * @param userPK            User that requested it.
    * @param planSpecification Parameters that user specified.
    * @param response          Put web content here.
    */
  def showPlan(
      machine: Machine,
      userPK: Long,
      planSpecification: PlanSpecification,
      response: Response
  ): Download = {
    val rtplan = makeRtplan(machine, userPK, planSpecification)

    val machineEnergyList = CustomizeRtPlanUtil.getMachineEnergyList(machine.machinePK.get)
    CustomizeRtPlanUtil.removeUnsupportedBeams(rtplan, machineEnergyList)

    // note: If the plan needed modifications (such as adding beams) then it would be done here.
    saveDicomToDatabase(machine, userPK, rtplan, planSpecification, procedure)

    val elem = dicomToElem(rtplan, name)

    val file = makeDicomDownloadFile(name)
    Util.writeAttributeListToFile(rtplan, file)

    Download(elem, file)
  }

}
