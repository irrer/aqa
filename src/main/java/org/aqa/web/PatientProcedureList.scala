package org.aqa.web

import edu.umro.ScalaUtil.Trace
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

  private val idCol = new Column[PIP]("Patient ID", _.patientProcedure.patientId, pip => makePrimaryKeyHtmlWithAQAAlias(pip.patientProcedure.patientId, pip.patientProcedure.patientProcedurePK))

  private val dailyQaActive = Procedure.ProcOfBBbyCBCT.isDefined && Procedure.ProcOfBBbyEPID.isDefined

  private val cbctPK: Option[Long] = if (dailyQaActive) Procedure.ProcOfBBbyCBCT.get.procedurePK else None
  private val epidPK: Option[Long] = if (dailyQaActive) Procedure.ProcOfBBbyEPID.get.procedurePK else None

  /**
    * Get the procedure name to show to the user in the list.
    * @param pip PatientProcedure and associated data.
    * @return A simple name.
    */
  private def procedureName(pip: PIP): String = {
    if (
      dailyQaActive &&
      (pip.patientProcedure.procedurePK == cbctPK.get || pip.patientProcedure.procedurePK == epidPK.get)
    )
      "Daily QA"
    else
      pip.procedure.name
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
