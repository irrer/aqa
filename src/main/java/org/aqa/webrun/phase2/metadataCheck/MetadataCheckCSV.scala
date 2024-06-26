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

package org.aqa.webrun.phase2.metadataCheck

import org.aqa.db.MetadataCheck
import org.aqa.Util
import org.aqa.webrun.ExtendedData
import org.aqa.webrun.phase2.RunReq

import java.io.File

object MetadataCheckCSV {

  val csvFileName = "MetadataCheck.csv"

  def makeCsvFile(extendedData: ExtendedData, runReq: RunReq, metadataCheckSeq: Seq[MetadataCheck]) = {

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

    type II = MetadataCheck

    val dblFmt = "%14.11f"
    val textFmt = "%s"
    val columns: Seq[(String, (II) => Any)] = Seq(
      ("beamName", (ii: II) => ii.beamName),
      ("gantryAnglePlan_deg", (ii: II) => ii.gantryAnglePlan_deg),
      ("gantryAnglePlanMinusImage_deg", (ii: II) => ii.gantryAnglePlanMinusImage_deg),
      ("collimatorAnglePlan_deg", (ii: II) => ii.collimatorAnglePlan_deg),
      ("collimatorAnglePlanMinusImage_deg", (ii: II) => ii.collimatorAnglePlanMinusImage_deg),
      ("x1JawPlan_mm", (ii: II) => ii.x1JawPlan_mm),
      ("x1JawPlanMinusImage_mm", (ii: II) => ii.x1JawPlanMinusImage_mm),
      ("x2JawPlan_mm", (ii: II) => ii.x2JawPlan_mm),
      ("x2JawPlanMinusImage_mm", (ii: II) => ii.x2JawPlanMinusImage_mm),
      ("y1JawPlan_mm", (ii: II) => ii.y1JawPlan_mm),
      ("y1JawPlanMinusImage_mm", (ii: II) => ii.y1JawPlanMinusImage_mm),
      ("y2JawPlan_mm", (ii: II) => ii.y2JawPlan_mm),
      ("y2JawPlanMinusImage_mm", (ii: II) => ii.y2JawPlanMinusImage_mm),
      ("energyPlan_kev", (ii: II) => ii.energyPlan_kev),
      ("energyPlanMinusImage_kev", (ii: II) => ii.energyPlanMinusImage_kev),
      ("flatteningFilter", (ii: II) => ii.flatteningFilter),
      ("pass", (ii: II) => ii.pass)
    )

    def metadataCheckToCsv(ii: MetadataCheck): String = {
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
        ("User", userId)
      )

      Seq(info.map(s => Util.textToCsv(s._1)).mkString(","), info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data = metadataCheckSeq.map(positionCheck => metadataCheckToCsv(positionCheck))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    val file = new File(extendedData.output.dir, csvFileName)
    Util.writeFile(file, text)
  }

}
