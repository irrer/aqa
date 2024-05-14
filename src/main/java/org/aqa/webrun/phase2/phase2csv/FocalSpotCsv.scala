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

import org.aqa.db.FocalSpot
import org.aqa.db.FocalSpotSet
import org.aqa.db.Output

import scala.collection.immutable.Seq

class FocalSpotCsv extends Phase2Csv[FocalSpotSet.FocalSpotSetHistory] {

  // abbreviation for the long name
  private type FSH = FocalSpotSet.FocalSpotSetHistory

  override val dataName: String = "Focal Spot"

  override protected def makeColList: Seq[CsvCol[FSH]] = {

    def makeFS(name: String, fsFunc: FSH => FocalSpot): Seq[CsvCol[FSH]] = {

      val n = name + " "

      Seq(
        CsvCol(n + "Gantry Angle", "Gantry angle in degrees rounded to nearest 90 degrees", (fsh: FSH) => fsFunc(fsh).gantryAngleRounded_deg.toString),
        CsvCol(n + "Col Angle", "Collimator angle in degrees rounded to nearest 90 degrees", (fsh: FSH) => fsFunc(fsh).collimatorAngleRounded_deg.toString),
        CsvCol(n + "Beam Name", "Name of beam in plan.", (fsh: FSH) => fsFunc(fsh).beamName),
        CsvCol(n + "Type", "Limiter type: Either Jaw or MLC.", (fsh: FSH) => fsFunc(fsh).beamLimiterName),
        CsvCol(n + "Fluence", "Either FFF or Standard.", (fsh: FSH) => fsFunc(fsh).fluenceName),
        CsvCol(n + "X center", "Center of edges X axis in mm.", (fsh: FSH) => fsFunc(fsh).centerX),
        CsvCol(n + "Y center", "Center of edges Y axis in mm.", (fsh: FSH) => fsFunc(fsh).centerY),
        CsvCol(n + "Top Error", "Measured top edge - planned top edge in mm", (fsh: FSH) => fsFunc(fsh).topEdgeError_mm),
        CsvCol(n + "Bottom Error", "Measured bottom edge - planned bottom edge in mm", (fsh: FSH) => fsFunc(fsh).bottomEdgeError_mm),
        CsvCol(n + "Left Error", "Measured left edge - planned left edge in mm", (fsh: FSH) => fsFunc(fsh).leftEdgeError_mm),
        CsvCol(n + "Right Error", "Measured right edge - planned right edge in mm", (fsh: FSH) => fsFunc(fsh).rightEdgeError_mm),
        CsvCol(n + "Top", "Measured top edge mm", (fsh: FSH) => fsFunc(fsh).topEdge_mm),
        CsvCol(n + "Bottom", "Measured bottom edge in mm", (fsh: FSH) => fsFunc(fsh).bottomEdge_mm),
        CsvCol(n + "Left", "Measured left edge in mm", (fsh: FSH) => fsFunc(fsh).leftEdge_mm),
        CsvCol(n + "Right", "Measured right edge in mm", (fsh: FSH) => fsFunc(fsh).rightEdge_mm),
        CsvCol(n + "Top Planned", "Planned top edge mm", (fsh: FSH) => fsFunc(fsh).topEdgePlanned_mm),
        CsvCol(n + "Bottom Planned", "Planned bottom edge in mm", (fsh: FSH) => fsFunc(fsh).bottomEdgePlanned_mm),
        CsvCol(n + "Left Planned", "Planned left edge in mm", (fsh: FSH) => fsFunc(fsh).leftEdgePlanned_mm),
        CsvCol(n + "Right Planned", "Planned right edge in mm", (fsh: FSH) => fsFunc(fsh).rightEdgePlanned_mm)
      )
    }

    val aggregate = Seq(
      CsvCol("X Alignment", "Alignment in X axis in mm", (fsh: FSH) => fsh.focalSpotSet.focalSpotAlignmentX_mm),
      CsvCol("Y Alignment", "Alignment in Y axis in mm", (fsh: FSH) => fsh.focalSpotSet.focalSpotAlignmentY_mm),
      CsvCol("KVP", "Beam Energy in KV", (fsh: FSH) => fsh.focalSpotSet.KVP_kv),
      CsvCol("FFF", "True if this the beams are Flattening Filter Free", (fsh: FSH) => fsh.focalSpotSet.isFFF)
    )

    val all = aggregate ++
      makeFS("Jaw 90", fsh => FocalSpotSet.FocalSpotSetHistory.jaw090(fsh)) ++
      makeFS("Jaw 270", fsh => FocalSpotSet.FocalSpotSetHistory.jaw270(fsh)) ++
      makeFS("MLC 90", fsh => FocalSpotSet.FocalSpotSetHistory.mlc090(fsh)) ++
      makeFS("MLC 270", fsh => FocalSpotSet.FocalSpotSetHistory.mlc270(fsh))

    all
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(machinePK: Long): Seq[FSH] = {
    val list = FocalSpotSet.history(machinePK, MetadataCache.metadataCache.phase3ProcedurePK) ++
      FocalSpotSet.history(machinePK, MetadataCache.metadataCache.focalSpotProcedurePK)
    list
  }

  override def getOutput(data: FSH): Output = data.output

  override protected def getSopUidList(data: FSH): Seq[String] =
    Seq(
      FocalSpotSet.FocalSpotSetHistory.jaw090(data).SOPInstanceUID,
      FocalSpotSet.FocalSpotSetHistory.jaw270(data).SOPInstanceUID,
      FocalSpotSet.FocalSpotSetHistory.mlc090(data).SOPInstanceUID,
      FocalSpotSet.FocalSpotSetHistory.mlc270(data).SOPInstanceUID
    )

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("Jaw 90", "Jaw 270", "MLC 90", "MLC 270")

}
