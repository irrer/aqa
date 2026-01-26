/*
 * Copyright 2026 Regents of the University of Michigan
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

import org.aqa.db.Output
import org.aqa.Util
import org.aqa.db.WinLutz360

class WinLutz360Csv(metadataCache: MetadataCache) extends Phase2Csv[WinLutz360.WinLutz360History](metadataCache: MetadataCache) {

  // abbreviation for the long name
  private type WLH = WinLutz360.WinLutz360History

  override val dataName: String = "WinLutz360"

  private def opt(d: Option[Double]): String = if (d.isDefined) d.get.toString else "NA"

  override protected def makeColList: Seq[CsvCol[WLH]] = {
    Seq(
      CsvCol("Beam Name", "Beam name from RTPLAN. If plan is not available, WL G*** C***", (wlh: WLH) => wlh.winLutz360.beamNameOf),
      CsvCol("Gantry ~deg", "Gantry angle in degrees", (wlh: WLH) => Util.angleRoundedTo90(wlh.winLutz360.gantryAngle_deg)),
      CsvCol("Coll ~deg", "Collimator angle in degrees", (wlh: WLH) => Util.angleRoundedTo90(wlh.winLutz360.collimatorAngle_deg)),
      CsvCol("XY Offset mm", "Distance between ball and planned centers in mm. sqrt(x^2 + y^2)", (wlh: WLH) => wlh.winLutz360.errorXY_mm),
      CsvCol("X Offset mm", "X Distance between ball and planned centers in mm. X field center - X ball center", (wlh: WLH) => wlh.winLutz360.errorX_mm),
      CsvCol("Y Offset mm", "Y Distance between ball and planned centers in mm. Y field center - Y ball center", (wlh: WLH) => wlh.winLutz360.errorY_mm),
      CsvCol("Ball X mm", "Measured X position of ball in mm.", (wlh: WLH) => wlh.winLutz360.ballCenterX_mm),
      CsvCol("Ball Y mm", "Measured Y position of ball in mm.", (wlh: WLH) => wlh.winLutz360.ballCenterY_mm),
      //
      CsvCol("X1 mm", "Measured position of X1 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.X1Offset_mm),
      CsvCol("X2 mm", "Measured position of X2 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.X2Offset_mm),
      CsvCol("Y1 mm", "Measured position of Y1 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.Y1Offset_mm),
      CsvCol("Y2 mm", "Measured position of Y2 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.Y2Offset_mm),
      //
      CsvCol("X1 mm", "Planned position of X1 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.X1PlannedOffset_mm),
      CsvCol("X2 mm", "Planned position of X2 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.X2PlannedOffset_mm),
      CsvCol("Y1 mm", "Planned position of Y1 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.Y1PlannedOffset_mm),
      CsvCol("Y2 mm", "Planned position of Y2 edge to center of field in mm", (wlh: WLH) => wlh.winLutz360.Y2PlannedOffset_mm),
      //
      CsvCol("Top mm", "Measured position of top edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.TopOffset_mm)),
      CsvCol("Bottom mm", "Measured position of bottom edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.BottomOffset_mm)),
      CsvCol("Left mm", "Measured position of left edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.LeftOffset_mm)),
      CsvCol("Right mm", "Measured position of right edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.RightOffset_mm))
      //
      // CsvCol("Top Planned mm", "Planned (expected) position of top edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.topEdgePlanned_mm)), // TODO
      // CsvCol("Bottom Planned mm", "Planned (expected) position of bottom edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.bottomEdgePlanned_mm)), // TODO
      // CsvCol("Left Planned mm", "Planned (expected) position of left edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.leftEdgePlanned_mm)), // TODO
      // CsvCol("Right Planned mm", "Planned (expected) position of right edge of field in mm", (wlh: WLH) => opt(wlh.winLutz360.rightEdgePlanned_mm)) // TODO
      //
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[WLH] = WinLutz360.historyByMachine(machinePK)

  override def getOutput(data: WLH): Output = data.output

  override protected def getSopUidList(data: WLH): Seq[String] = Seq(data.winLutz360.rtimageUID)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

}
