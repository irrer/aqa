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

package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.CenterDose
import org.aqa.db.Output

class CenterDoseCsv extends Phase2Csv[CenterDose.CenterDoseHistory] {

  // abbreviation for the long name
  type CD = CenterDose.CenterDoseHistory

  override val dataName: String = "Center Dose"

  override protected def makeColList: Seq[CsvCol[CD]] = {
    Seq(
      CsvCol("Beam Name", "Common name of RTPLAN beam.", (cd: CD) => cd.centerDose.beamName),
      CsvCol("Dose", "Average dose of pixels in the center of the image.  Center is offset by results from collimator offset test.", (cd: CD) => cd.centerDose.dose),
      CsvCol("Units", "Units that dose is in.", (cd: CD) => cd.centerDose.units)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[CD] = {
    val cdHistory = CenterDose.history(machinePK, MetadataCache.metadataCache.phase2ProcedurePK) ++ CenterDose.history(machinePK, MetadataCache.metadataCache.phase3ProcedurePK)
    cdHistory
  }

  override def getSopUID(data: CD): Option[String] = Some(data.centerDose.SOPInstanceUID)

  override def getOutput(data: CD): Output = data.output
}
