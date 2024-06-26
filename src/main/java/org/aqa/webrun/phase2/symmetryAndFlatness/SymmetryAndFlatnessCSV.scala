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

package org.aqa.webrun.phase2.symmetryAndFlatness

import org.aqa.Util
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.SymmetryAndFlatness
import org.aqa.db.User
import org.aqa.run.ProcedureStatus

object SymmetryAndFlatnessCSV {

  /**
    * Construct the CSV content for the given output.
    *
    * @param output Just for this one entry.
    * @return
    */
  def makeCsvFile(output: Output): String = {

    // format lots of meta-information for the CSV header

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _       => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    val procedureDesc: String = {
      val procedure = Procedure.get(output.procedurePK).get
      procedure.name + " : " + procedure.version
    }

    val machine: Option[Machine] = Machine.get(output.machinePK.get)

    val machineId: String = if (machine.isDefined) machine.get.id else "none"

    val userId = {
      try {
        User.get(output.userPK.get).get.id
      } catch {
        case _: Throwable => "unknown"
      }
    }

    val institutionName = {
      try {
        val user = User.get(output.userPK.get).get
        Institution.get(user.userPK.get).get.name
      } catch {
        case _: Throwable => "unknown"
      }
    }

    val acquisitionDate = if (output.dataDate.isDefined) Util.standardDateFormat.format(output.dataDate.get) else "none"

    type SFB = SymmetryAndFlatness.SymmetryAndFlatnessHistory

    def boolToStatus(b: Boolean) = { if (b) ProcedureStatus.pass else ProcedureStatus.fail }.toString()

    val columns: Seq[(String, SFB => Any)] = Seq(
      ("delivery Time", (sfb: SFB) => Util.standardDateFormat.format(sfb.output.dataDate.get)),
      ("beamName", (sfb: SFB) => sfb.symmetryAndFlatness.beamName),
      ("SOPInstanceUID", (sfb: SFB) => sfb.symmetryAndFlatness.SOPInstanceUID),
      ("axialSymmetry CU", (sfb: SFB) => sfb.symmetryAndFlatness.axialSymmetry),
      ("axialSymmetryBaseline CU", (sfb: SFB) => sfb.baseline.axialSymmetry),
      ("axialSymmetryStatus", (sfb: SFB) => boolToStatus(sfb.symmetryAndFlatness.axialSymmetryPass(sfb.baseline))),
      ("transverseSymmetry CU", (sfb: SFB) => sfb.symmetryAndFlatness.transverseSymmetry),
      ("transverseSymmetryBaseline CU", (sfb: SFB) => sfb.baseline.transverseSymmetry),
      ("transverseSymmetryStatus", (sfb: SFB) => boolToStatus(sfb.symmetryAndFlatness.transverseSymmetryPass(sfb.baseline))),
      ("flatness CU", (sfb: SFB) => sfb.symmetryAndFlatness.flatness),
      ("flatnessBaseline CU", (sfb: SFB) => sfb.baseline.flatness),
      ("flatnessStatus", (sfb: SFB) => boolToStatus(sfb.symmetryAndFlatness.flatnessPass(sfb.baseline))),
      ("profileConstancy CU", (sfb: SFB) => sfb.symmetryAndFlatness.profileConstancy(sfb.baseline)),
      ("profileConstancyBaseline CU", (sfb: SFB) => sfb.baseline.profileConstancy(sfb.baseline)),
      ("profileConstancyStatus", (sfb: SFB) => boolToStatus(sfb.symmetryAndFlatness.profileConstancyPass(sfb.baseline))),
      ("top CU", (sf: SFB) => sf.symmetryAndFlatness.top_cu),
      ("bottom CU", (sf: SFB) => sf.symmetryAndFlatness.bottom_cu),
      ("left CU", (sf: SFB) => sf.symmetryAndFlatness.left_cu),
      ("right CU", (sf: SFB) => sf.symmetryAndFlatness.right_cu),
      ("center CU", (sf: SFB) => sf.symmetryAndFlatness.center_cu)
    )

    def symmetryAndFlatnessToCsv(sfb: SFB): String = {
      def fmt(any: Any): String = {
        any match {
          case d: Double => d.formatted("%14.11e")
          case _         => Util.textToCsv(any.toString)
        }
      }

      columns.map(c => fmt(c._2(sfb))).mkString(",")
    }

    val metaData = {
      val info = Seq(
        ("Procedure", procedureDesc),
        ("Machine", machineId),
        ("Institution", institutionName),
        ("Acquisition Date", acquisitionDate),
        ("Analysis Date", analysisDate),
        ("User", userId)
      )

      Seq(info.map(s => Util.textToCsv(s._1)).mkString(","), info.map(s => Util.textToCsv(s._2)).mkString(","))
    }

    val header = Seq(columns.map(c => c._1).mkString(","))

    val data: Iterable[String] = {
      if (machine.isDefined) {
        val list = {
          SymmetryAndFlatness.history(machine.get.machinePK.get, Procedure.ProcOfPhase2.get.procedurePK.get) ++
            SymmetryAndFlatness.history(machine.get.machinePK.get, Procedure.ProcOfPhase3.get.procedurePK.get)
        }

        val textList = list.map(sfb => symmetryAndFlatnessToCsv(sfb))
        textList
      } else
        Seq("Machine for outputPK " + output.outputPK.get + " could not be found")
    }

    // symmetryAndFlatnessSeq.map(positionCheck => symmetryAndFlatnessToCsv(positionCheck))

    val text = (metaData ++ header ++ data).mkString("", "\r\n", "\r\n")
    text
  }

}
