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

import org.aqa.db.CollimatorPosition
import org.aqa.db.Output

class CollimatorPositionCsv extends Phase2Csv[CollimatorPosition.ColPosHistory] {

  // abbreviation for the long name
  private type CP = CollimatorPosition.ColPosHistory

  override val dataName: String = "Collimator Position"

  override protected def makeColList: Seq[CsvCol[CP]] = {
    Seq(
      CsvCol("Beam Name", "Common name of RTPLAN beam.", (cp: CP) => cp.colCent.beamName),
      CsvCol("FloodCompensation", "True if flood compensation was used.", (cp: CP) => cp.colCent.FloodCompensation.toString),
      CsvCol("Status", "Pass/Fail status.", (cp: CP) => cp.colCent.status),
      CsvCol("X1", "X1 measured position of collimator (X axis) in mm.", (cp: CP) => cp.colCent.X1_mm),
      CsvCol("X2", "X2 measured position of collimator (X axis) in mm.", (cp: CP) => cp.colCent.X2_mm),
      CsvCol("Y1", "Y1 measured position of collimator (Y axis) in mm.", (cp: CP) => cp.colCent.Y1_mm),
      CsvCol("Y2", "Y2 measured position of collimator (Y axis) in mm.", (cp: CP) => cp.colCent.Y2_mm),
      CsvCol("X1 Expected - Measured", "X1 plan - east position of collimator  (X axis) in mm", (cp: CP) => cp.colCent.X1_ExpectedMinusImage_mm),
      CsvCol("X2 Expected - Measured", "X2 plan - west position of collimator  (X axis) in mm", (cp: CP) => cp.colCent.X2_ExpectedMinusImage_mm),
      CsvCol("Y1 Expected - Measured", "Y1 plan - north position of collimator (Y axis) in mm", (cp: CP) => cp.colCent.Y1_ExpectedMinusImage_mm),
      CsvCol("Y2 Expected - Measured", "Y2 plan - south position of collimator (Y axis) in mm", (cp: CP) => cp.colCent.Y2_ExpectedMinusImage_mm),
      CsvCol("X Collimator center of rotation", "X position of the collimator's center of rotation", (cp: CP) => cp.colCent.XCollimatorCenterOfRotation_mm),
      CsvCol("Y Collimator center of rotation", "Y position of the collimator's center of rotation", (cp: CP) => cp.colCent.YCollimatorCenterOfRotation_mm)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[CP] = {
    val cpHistory = CollimatorPosition.history(machinePK, MetadataCache.metadataCache.phase2ProcedurePK) ++ CollimatorPosition.history(machinePK, MetadataCache.metadataCache.phase3ProcedurePK)
    cpHistory
  }

  override def getSopUidList(data: CP): Seq[String] = Seq(data.colCent.SOPInstanceUID)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")
  override def getOutput(data: CP): Output = data.output
}
