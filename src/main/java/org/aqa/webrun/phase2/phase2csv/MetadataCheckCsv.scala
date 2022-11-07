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

import org.aqa.db.MetadataCheck
import org.aqa.db.Output

class MetadataCheckCsv extends Phase2Csv[MetadataCheck.MetadataCheckHistory] {

  // abbreviation for the long name
  type MDC = MetadataCheck.MetadataCheckHistory

  override val dataName: String = "Metadata Check"

  override protected def makeColList: Seq[CsvCol[MDC]] = {
    Seq(
      CsvCol("Beam Name", "Name of beam in plan", (mdc: MDC) => mdc.metadataCheck.beamName),
      CsvCol("Gantry Angle Plan deg", "Planned gantry angle in degrees", (mdc: MDC) => mdc.metadataCheck.gantryAnglePlan_deg),
      CsvCol("gantryAnglePlan - Image deg", "Difference from planned gantry angle in degrees", (mdc: MDC) => mdc.metadataCheck.gantryAnglePlanMinusImage_deg),
      CsvCol("collimatorAnglePlan deg", "Planned collimator angle in degrees", (mdc: MDC) => mdc.metadataCheck.collimatorAnglePlan_deg),
      CsvCol(
        "CollimatorAnglePlan - Image deg",
        "Difference from planned collimator angle in degrees",
        (mdc: MDC) => mdc.metadataCheck.collimatorAnglePlanMinusImage_deg
      ),
      CsvCol("X1 Jaw Plan mm", "Planned jaw position for X1 in mm", (mdc: MDC) => mdc.metadataCheck.x1JawPlan_mm),
      CsvCol("X1 Jaw Plan - Image mm", "Difference from planned jaw position for X1 in mm", (mdc: MDC) => mdc.metadataCheck.x1JawPlanMinusImage_mm),
      CsvCol("X2 Jaw Plan mm", "Planned jaw position for X2 in mm", (mdc: MDC) => mdc.metadataCheck.x2JawPlan_mm),
      CsvCol("X2 Jaw Plan - Image mm", "Difference from planned jaw position for X2 in mm", (mdc: MDC) => mdc.metadataCheck.x2JawPlanMinusImage_mm),
      CsvCol("Y1 Jaw Plan mm", "Planned jaw position for Y1 in mm", (mdc: MDC) => mdc.metadataCheck.y1JawPlan_mm),
      CsvCol("Y1 Jaw Plan - Image mm", "Difference from planned jaw position for Y1 in mm", (mdc: MDC) => mdc.metadataCheck.y1JawPlanMinusImage_mm),
      CsvCol("Y2 Jaw Plan mm", "Planned jaw position for Y2 in mm", (mdc: MDC) => mdc.metadataCheck.y2JawPlan_mm),
      CsvCol("Y2 Jaw Plan - Image mm", "Difference from planned jaw position for Y2 in mm", (mdc: MDC) => mdc.metadataCheck.y2JawPlanMinusImage_mm),
      CsvCol("Energy Plan KEV", "Planned energy in kilo electron volts", (mdc: MDC) => mdc.metadataCheck.energyPlan_kev),
      CsvCol("Energy Plan - Image KEV", "Difference from planned energy in kilo electron volts", (mdc: MDC) => mdc.metadataCheck.energyPlanMinusImage_kev),
      CsvCol("Flattening Filter", "True if a flattening filter was present", (mdc: MDC) => mdc.metadataCheck.flatteningFilter.toString),
      CsvCol("Pass", "True if all values were within tolerances", (mdc: MDC) => mdc.metadataCheck.pass.toString)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[MDC] = {
    val mdcHistory = MetadataCheck.history(machinePK, MetadataCache.metadataCache.phase2ProcedurePK) ++ MetadataCheck.history(machinePK, MetadataCache.metadataCache.phase3ProcedurePK)
    mdcHistory
  }

  override def getSopUidList(data: MDC): Seq[String] = Seq(data.metadataCheck.SOPInstanceUID.get)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

  override def getOutput(data: MDC): Output = data.output
}
