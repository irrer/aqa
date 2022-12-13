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

package org.aqa.webrun.phase2.collimatorPosition

import org.aqa.db.CollimatorPosition
import org.aqa.Util
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq

import java.io.File

object CollimatorPositionCSV {

  val csvFileName = "CollimatorPosition.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, collimatorPositionSeq: Seq[CollimatorPosition]) = {

    // format lots of meta-information for the CSV header

    val analysisDate: String = {
      val date = extendedData.output.analysisDate match {
        case Some(d) => d
        case _       => extendedData.output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    val procedureDesc: String = extendedData.procedure.name + " : " + extendedData.procedure.version

    val machineId = extendedData.machine.id
    val userId = extendedData.user.id
    val acquisitionDate = if (extendedData.output.dataDate.isDefined) Util.standardDateFormat.format(extendedData.output.dataDate.get) else "none"

    type II = CollimatorPosition

    val dblFmt = "%14.11f"
    val textFmt = "%s"
    val columns: Seq[(String, (II) => Any)] = Seq(
      ("beamName", (ii: II) => ii.beamName),
      ("SOPInstanceUID", (ii: II) => ii.SOPInstanceUID),
      ("gantryAngle_deg", (ii: II) => ii.gantryAngle_deg),
      ("collimatorAngle_deg", (ii: II) => ii.collimatorAngle_deg),
      ("FloodCompensation", (ii: II) => ii.FloodCompensation),
      ("XCollimatorCenterOfRotation_mm", (ii: II) => ii.XCollimatorCenterOfRotation_mm),
      ("YCollimatorCenterOfRotation_mm", (ii: II) => ii.YCollimatorCenterOfRotation_mm),
      ("X1_mm", (ii: II) => ii.X1_mm),
      ("X2_mm", (ii: II) => ii.X2_mm),
      ("Y1_mm", (ii: II) => ii.Y1_mm),
      ("Y2_mm", (ii: II) => ii.Y2_mm),
      ("X1_ExpectedMinusImage_mm", (ii: II) => ii.X1_ExpectedMinusImage_mm),
      ("X2_ExpectedMinusImage_mm", (ii: II) => ii.X2_ExpectedMinusImage_mm),
      ("Y1_ExpectedMinusImage_mm", (ii: II) => ii.Y1_ExpectedMinusImage_mm),
      ("Y2_ExpectedMinusImage_mm", (ii: II) => ii.Y2_ExpectedMinusImage_mm),
      ("X1_Expected_mm", (ii: II) => ii.X1_ExpectedMinusImage_mm + ii.X1_mm),
      ("X2_Expected_mm", (ii: II) => ii.X2_ExpectedMinusImage_mm + ii.X2_mm),
      ("Y1_Expected_mm", (ii: II) => ii.Y1_ExpectedMinusImage_mm + ii.Y1_mm),
      ("Y2_Expected_mm", (ii: II) => ii.Y2_ExpectedMinusImage_mm + ii.Y2_mm),
      ("pass-fail status", (ii: II) => ii.status)
    )

    def collimatorPositionToCsv(ii: CollimatorPosition): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _         => Util.textToCsv(any.toString)
        }
      }
      columns.map(c => fmt(c._2(ii))).mkString(",")
    }

    val metaData = {
      val info = Seq(
        ("Procedure", procedureDesc),
        ("Machine", machineId),
        ("Institution", extendedData.institution.name),
        ("Acquisition Date", acquisitionDate),
        ("Analysis Date", analysisDate),
        ("User", userId),
        ("XCollimatorCenterOfRotation_mm", collimatorPositionSeq.head.XCollimatorCenterOfRotation_mm.toString),
        ("YCollimatorCenterOfRotation_mm", collimatorPositionSeq.head.YCollimatorCenterOfRotation_mm.toString)
      )

      Seq(info.map(s => Util.textToCsv(s._1)).mkString(","), info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data = collimatorPositionSeq.map(positionCheck => collimatorPositionToCsv(positionCheck))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(extendedData.output.dir, csvFileName)
    Util.writeFile(file, text)
  }

}
