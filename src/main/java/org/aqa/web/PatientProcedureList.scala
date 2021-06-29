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

package org.aqa.web

import org.aqa.db.PatientProcedure
import org.aqa.db.PatientProcedure.ExtendedData
import org.aqa.db.Procedure
import org.aqa.web.WebUtil._
import org.restlet.Response

object PatientProcedureList {
  private val path = new String((new PatientProcedureList).pathOf)

  def redirect(response: Response): Unit = response.redirectSeeOther(path)
}

class PatientProcedureList extends GenericList[ExtendedData] with WebUtil.SubUrlAdmin {

  type PIP = ExtendedData

  override val listName = "Patient Procedure"

  def compareForSorting(a: PIP, b: PIP): Boolean = {
    val aa = a.dicomAnonymous.originalValue
    val bb = b.dicomAnonymous.originalValue

    def onlyDigits(text: String) = text.replaceAll("[^0-9]", " ")

    val aD = onlyDigits(aa)
    val bD = onlyDigits(bb)

    if (aD.compare(bD) == 0)
      aa.compare(bb) < 0
    else {
      aD.compare(bD) < 0

      // a.dicomAnonymous.originalValue.compare(b.dicomAnonymous.originalValue) < 0
    }
  }

  //noinspection ConvertibleToMethodValue
  private val idCol = new Column[PIP](
    columnName = "Patient ID",
    compare = compareForSorting _,
    pip => makePrimaryKeyHtmlWithAQAAlias(pip.dicomAnonymous.value, pip.patientProcedure.patientProcedurePK)
  )

  private val dailyQaActive = Procedure.ProcOfBBbyCBCT.isDefined && Procedure.ProcOfBBbyEPID.isDefined

  private val cbctPK: Option[Long] = if (dailyQaActive) Procedure.ProcOfBBbyCBCT.get.procedurePK else None
  private val epidPK: Option[Long] = if (dailyQaActive) Procedure.ProcOfBBbyEPID.get.procedurePK else None

  private val locActive =  Procedure.ProcOfLOC.isDefined && Procedure.ProcOfLOCBaseline.isDefined

  private val locPK: Option[Long] = if (locActive) Procedure.ProcOfLOC.get.procedurePK else None
  private val locBaselinePK: Option[Long] = if (locActive) Procedure.ProcOfLOCBaseline.get.procedurePK else None

  /**
    * Get the procedure name to show to the user in the list.
    * @param pip PatientProcedure and associated data.
    * @return A simple name.
    */
  private def procedureName(pip: PIP): String = {

    0 match {
      case _ if dailyQaActive && (pip.patientProcedure.procedurePK == cbctPK.get || pip.patientProcedure.procedurePK == epidPK.get) => "Daily QA"
      case _ if locActive && (pip.patientProcedure.procedurePK == locPK.get || pip.patientProcedure.procedurePK == locBaselinePK.get) => "LOC"
      case _ => pip.procedure.name
    }
  }

  private def showActive(pip: PIP): String = if (pip.patientProcedure.active) "Active" else "-"

  private val procedureCol = new Column[PIP]("Procedure", pip => procedureName(pip))

  private val activeCol = new Column[PIP]("Active", showActive)

  override val columnList = Seq(idCol, procedureCol, activeCol)

  override def getData(valueMap: ValueMapT, response: Response): Seq[PIP] = {
    val user = getUser(valueMap)
    PatientProcedure.listExtended(user.get.institutionPK)
  }

  override def getPK(pip: PIP): Long = pip.patientProcedure.patientProcedurePK.get

  val checkbox = new WebInputCheckbox("All Institutions", true, Some("Check to show users from all institutions, then click 'Refresh'"), 2, 0)

}
