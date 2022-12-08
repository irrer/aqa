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

package org.aqa.webrun.dailyQA

import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import org.aqa.AnonymizeUtil
import org.aqa.db.DicomAnonymous
import org.aqa.db.Machine


/**
 * De-anonymize fields in a CSV.
 *
 * @param institutionPK Indicates which institution.
 */
class DailyQADeAnonymize(institutionPK: Long, machineColIndex: Int = -1, patientIdColIndex: Int = -1, operatorsNameColIndex: Int = -1) {

  // Map id --> de-anonymized real id
  private val machineMap = {
    val machineList = Machine.listMachinesFromInstitution(institutionPK)
    val mm = machineList.map(machine => (machine.id, AnonymizeUtil.decryptWithNonce(machine.institutionPK, machine.id_real.get)))
    mm.toMap
  }

  private def tagMap(tag: AttributeTag): Map[String, String] = {
    DicomAnonymous.getAttributesByTag(institutionPK, Seq(tag)).
      map(da => (da.value, AnonymizeUtil.decryptWithNonce(institutionPK, da.value_real))).toMap
  }

  private val patIdMap = tagMap(TagFromName.PatientID)

  private val operatorsNameMap = tagMap(TagFromName.OperatorsName)

  /**
   * Replace anonymized values with real values.
   *
   * @param line Entire text of one line.
   * @return Same line with fields de-anonymized.
   */
  def deAnonymize(line: String): String = {
    val columnList = line.split(",")

    def patch(colList: Seq[String], colIndex: Int, map: Map[String, String]): Seq[String] = {
      if (colIndex >= 0) {
        val anon = colList(colIndex)
        if (map.contains(anon))
          colList.patch(colIndex, Seq(map(anon)), 1)
        else
          colList
      }
      else
        colList
    }

    val c1 = patch(columnList, machineColIndex, machineMap)
    val c2 = patch(c1, patientIdColIndex, patIdMap)
    val c3 = patch(c2, operatorsNameColIndex, operatorsNameMap)

    c3.mkString(",")
  }


}
