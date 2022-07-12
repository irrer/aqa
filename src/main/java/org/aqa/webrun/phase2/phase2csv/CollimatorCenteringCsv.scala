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

import org.aqa.db.CollimatorCentering
import org.aqa.db.Output

class CollimatorCenteringCsv extends Phase2Csv[CollimatorCentering.ColCentHistory] {

  // abbreviation for the long name
  type CCH = CollimatorCentering.ColCentHistory

  override val dataName: String = "Collimator Centering"

  private def offset(colCent: CollimatorCentering): Double = {
    val x = colCent.xCollimatorCenter_mm
    val y = colCent.yCollimatorCenter_mm
    Math.sqrt(x * x + y * y)
  }

  override protected def makeColList: Seq[CsvCol[CCH]] = {
    Seq(
      CsvCol("Gantry Angle", "Gantry angle in degrees rounded to nearest 90 degrees.", (cch: CCH) => cch.colCent.gantryAngleRounded_deg),
      CsvCol("Coll 90 Beam Name", "Name of beam delivered with collimator angle 90", (cch: CCH) => cch.colCent.beamName090),
      CsvCol("Coll 270 Beam Name", "Name of beam delivered with collimator angle 270", (cch: CCH) => cch.colCent.beamName270),
      CsvCol("X Center", "X position of center of rotation in mm", (cch: CCH) => cch.colCent.xCollimatorCenter_mm),
      CsvCol("Y Center", "Y position of center of rotation in mm", (cch: CCH) => cch.colCent.yCollimatorCenter_mm),
      CsvCol("Total Offset", "Total offset from center: sqrt(X*X + Y*Y) in mm", (cch: CCH) => offset(cch.colCent)),
      CsvCol("X1 90", "X1 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.X1_090_mm),
      CsvCol("X2 90", "X2 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.X2_090_mm),
      CsvCol("Y1 90", "Y1 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.Y1_090_mm),
      CsvCol("Y2 90", "Y2 position in mm of leaf edge with collimator at 90 degrees.", (cch: CCH) => cch.colCent.Y2_090_mm),
      CsvCol("X1 270", "X1 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.X1_270_mm),
      CsvCol("X2 270", "X2 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.X2_270_mm),
      CsvCol("Y1 270", "Y1 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.Y1_270_mm),
      CsvCol("Y2 270", "Y2 position in mm of leaf edge with collimator at 270 degrees.", (cch: CCH) => cch.colCent.Y2_270_mm)
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[CCH] = {
    val cchList = Seq(0, 90, 180, 270).flatMap(gantryAngle =>
      CollimatorCentering.history(machinePK, Some(gantryAngle), MetadataCache.metadataCache.phase2ProcedurePK) ++
        CollimatorCentering.history(machinePK, Some(gantryAngle), MetadataCache.metadataCache.phase3ProcedurePK)
    )
    cchList
  }

  override def getOutput(data: CCH): Output = data.output

  /**
    * Get the SOP of the DICOM for this data set.
    *
    * @param data Data using DICOM data.
    * @return SOP instance UID.
    */
  override protected def getSopUID(data: CCH): String = data.colCent.SOPInstanceUID270

  override protected val dicomHeaderPrefix: Option[String] = Some("C 90")
  override protected val dicom2HeaderPrefix: Option[String] = Some("C 270")

  override protected def getSopUID2(data: CCH): String = data.colCent.SOPInstanceUID090
}
