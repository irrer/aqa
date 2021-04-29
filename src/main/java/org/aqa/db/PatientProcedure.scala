package org.aqa.db

import org.aqa.Logging
import org.aqa.db.Db.driver.api._
import org.aqa.procedures.ProcedureOutput

import scala.xml.Elem

/**
  * Store the analysis results for one patient --> procedure association.
  */
case class PatientProcedure(
    patientProcedurePK: Option[Long], // primary key
    patientId: String, // anonymized version of patient ID
    institutionPK: Long, // institution to which this belongs
    procedurePK: Long, // procedure associated with the given patient ID.
    active: Boolean // True if patient is active and should be checked for new data.
) {

  def insert: PatientProcedure = {
    val insertQuery =
      PatientProcedure.query returning PatientProcedure.query.map(_.patientProcedurePK) into ((patientProcedure, patientProcedurePK) => patientProcedure.copy(patientProcedurePK = Some(patientProcedurePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(PatientProcedure.query.insertOrUpdate(this))

  override def toString: String =
    "patientProcedurePK : " + patientProcedurePK +
      "    patientId : " + patientId +
      "    institutionPK : " + institutionPK +
      "    procedurePK : " + procedurePK +
      "    active : " + active
}

object PatientProcedure extends ProcedureOutput with Logging {

  class PatientProcedureTable(tag: Tag) extends Table[PatientProcedure](tag, "patientProcedure") {

    def patientProcedurePK = column[Long]("patientProcedurePK", O.PrimaryKey, O.AutoInc)
    def patientId = column[String]("patientId")
    def institutionPK = column[Long]("institutionPK")
    def procedurePK = column[Long]("procedurePK")
    def active = column[Boolean]("active")

    def * =
      (
        patientProcedurePK.?,
        patientId,
        institutionPK,
        procedurePK,
        active
      ) <> (PatientProcedure.apply _ tupled, PatientProcedure.unapply)

    def institutionFK =
      foreignKey("PatientProcedure_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def procedureFK = foreignKey("PatientProcedure_procedurePKConstraint", procedurePK, Procedure.query)(_.procedurePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[PatientProcedureTable]

  override val topXmlLabel = "PatientProcedure"

  /**
   * Given the public key, retrieve the given row.
   * @param patientProcedurePK Public key.
   * @return Row of data, if it exists.
   */
  def get(patientProcedurePK: Long): Option[PatientProcedure] = {
    val action = for {
      inst <- PatientProcedure.query if inst.patientProcedurePK === patientProcedurePK
    } yield inst
    val list = Db.run(action.result)
    list.headOption
  }

  def delete(patientProcedurePK: Long): Int = {
    val q = query.filter(_.patientProcedurePK === patientProcedurePK)
    val action = q.delete
    Db.run(action)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    throw new RuntimeException("PatientProcedure: insert using Elem not supported")
  }

}
