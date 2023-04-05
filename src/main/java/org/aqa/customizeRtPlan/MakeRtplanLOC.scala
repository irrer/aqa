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

import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.Machine
import org.aqa.db.Procedure
import org.aqa.Config
import org.aqa.Util
import org.restlet.Response

import java.io.File

class MakeRtplanLOC extends MakeRtplan {
  override def name: String = "LOC"
  override def planFileProcedureName: String = "LOCDelivery"

  private def planFileProcedureNameBaseline: String = "LOCBaseline"
  override def procedure: Procedure = Procedure.ProcOfLOC.get

  override def showPlan(machine: Machine, userPK: Long, planSpecification: PlanSpecification, response: Response): Download = {

    val rtplanDelivery = makeRtplan(machine, userPK, planSpecification)

    val rtplanBaseline = makeRtplan(machine, userPK, planSpecification, "LOCBaseline")

    val dateText = fileDateText

    val deliveryFileName = s"RTPLAN_${planFileProcedureName}_$dateText.dcm"
    val baselineFileName = s"RTPLAN_${planFileProcedureNameBaseline}_$dateText.dcm"
    val zipFileName = s"RTPLANS_${planFileProcedureName}_$dateText.zip"

    val zipFile = new File(Config.tmpDirFile, zipFileName)

    // make zip file of the two plans
    val zipStream = new FileUtil.ToZipOutputStream
    zipStream.writeDicom(rtplanDelivery, deliveryFileName, "AQA")
    zipStream.writeDicom(rtplanBaseline, baselineFileName, "AQA")
    val bytes = zipStream.finish()
    Util.writeBinaryFile(zipFile, bytes)

    val elem = {
      <div>
        {dicomToElem(rtplanBaseline, "LOC Baseline")}
        <p/>
        {dicomToElem(rtplanDelivery, "LOC Delivery")}
      </div>
    }

    Download(elem, zipFile)
  }
}
