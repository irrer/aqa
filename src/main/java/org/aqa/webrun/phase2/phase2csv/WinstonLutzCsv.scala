/*
 * Copyright 2023 Regents of the University of Michigan
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
import org.aqa.db.WinstonLutz
import org.aqa.Util

class WinstonLutzCsv(metadataCache: MetadataCache) extends Phase2Csv[WinstonLutz.WinstonLutzHistory](metadataCache: MetadataCache) {

  // abbreviation for the long name
  private type WLH = WinstonLutz.WinstonLutzHistory

  override val dataName: String = "Winston Lutz"

  private def opt(d: Option[Double]): String = if (d.isDefined) d.get.toString else "NA"

  override protected def makeColList: Seq[CsvCol[WLH]] = {
    Seq(
      CsvCol("Beam Name", "Beam name from RTPLAN. If plan is not available, WL G*** C***", (wlh: WLH) => wlh.winstonLutz.beamNameOf),
      CsvCol("Gantry ~deg", "Gantry angle rounded to the nearest 90 degrees", (wlh: WLH) => Util.angleRoundedTo90(wlh.winstonLutz.gantryAngle_deg)),
      CsvCol("Coll ~deg", "Collimator angle rounded to the nearest 90 degrees", (wlh: WLH) => Util.angleRoundedTo90(wlh.winstonLutz.collimatorAngle_deg)),
      CsvCol("XY Offset mm", "Distance between ball and field centers in mm. sqrt(x^2 + y^2)", (wlh: WLH) => wlh.winstonLutz.errorXY_mm),
      CsvCol("X Offset mm", "X Distance between ball and field centers in mm. X field center - X ball center", (wlh: WLH) => wlh.winstonLutz.errorX_mm),
      CsvCol("Y Offset mm", "Y Distance between ball and field centers in mm. Y field center - Y ball center", (wlh: WLH) => wlh.winstonLutz.errorY_mm),
      CsvCol("Ball X mm", "Measured X position of ball in mm.", (wlh: WLH) => wlh.winstonLutz.ballX_mm),
      CsvCol("Ball Y mm", "Measured Y position of ball in mm.", (wlh: WLH) => wlh.winstonLutz.ballY_mm),
      CsvCol("Top mm", "Measured position of top edge of field in mm", (wlh: WLH) => wlh.winstonLutz.topEdge_mm),
      CsvCol("Bottom mm", "Measured position of bottom edge of field in mm", (wlh: WLH) => wlh.winstonLutz.bottomEdge_mm),
      CsvCol("Left mm", "Measured position of left edge of field in mm", (wlh: WLH) => wlh.winstonLutz.leftEdge_mm),
      CsvCol("Right mm", "Measured position of right edge of field in mm", (wlh: WLH) => wlh.winstonLutz.rightEdge_mm),
      CsvCol("Top Planned mm", "Planned (expected) position of top edge of field in mm", (wlh: WLH) => opt(wlh.winstonLutz.topEdgePlanned_mm)),
      CsvCol("Bottom Planned mm", "Planned (expected) position of bottom edge of field in mm", (wlh: WLH) => opt(wlh.winstonLutz.bottomEdgePlanned_mm)),
      CsvCol("Left Planned mm", "Planned (expected) position of left edge of field in mm", (wlh: WLH) => opt(wlh.winstonLutz.leftEdgePlanned_mm)),
      CsvCol("Right Planned mm", "Planned (expected) position of right edge of field in mm", (wlh: WLH) => opt(wlh.winstonLutz.rightEdgePlanned_mm))
    )
  }

  /**
    * Get the data for a particular machine.
    *
    * @param machinePK Machine to get data for.
    * @return List of data for the particular machine.
    */
  override protected def getData(metadataCache: MetadataCache, machinePK: Long): Seq[WLH] = WinstonLutz.historyByMachine(machinePK)

  override def getOutput(data: WLH): Output = data.output

  override protected def getSopUidList(data: WLH): Seq[String] = Seq(data.winstonLutz.rtimageUID)

  override protected val dicomHeaderPrefixList: Seq[String] = Seq("")

}
