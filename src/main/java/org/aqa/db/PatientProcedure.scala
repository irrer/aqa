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
    dicomAnonymousPK: Long, // references DICOM tag
    institutionPK: Long, // institution to which this belongs
    procedurePK: Long, // procedure associated with the given patient ID.
    active: Boolean // True if patient is active and should be checked for new data.
) {

  def insert: PatientProcedure = {
    val insertQuery =
      PatientProcedure.query returning PatientProcedure.query.map(_.patientProcedurePK) into ((patientProcedure, patientProcedurePK) =>
        patientProcedure.copy(patientProcedurePK = Some(patientProcedurePK))
      )
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate(): Int = Db.run(PatientProcedure.query.insertOrUpdate(this))

  override def toString: String =
    "patientProcedurePK : " + patientProcedurePK +
      "    dicomAnonymousPK : " + dicomAnonymousPK +
      "    institutionPK : " + institutionPK +
      "    procedurePK : " + procedurePK +
      "    active : " + active
}

object PatientProcedure extends ProcedureOutput with Logging {

  class PatientProcedureTable(tag: Tag) extends Table[PatientProcedure](tag, "patientProcedure") {

    def patientProcedurePK = column[Long]("patientProcedurePK", O.PrimaryKey, O.AutoInc)
    def dicomAnonymousPK = column[Long]("dicomAnonymousPK")
    def institutionPK = column[Long]("institutionPK")
    def procedurePK = column[Long]("procedurePK")
    def active = column[Boolean]("active")

    def * =
      (
        patientProcedurePK.?,
        dicomAnonymousPK,
        institutionPK,
        procedurePK,
        active
      ) <> (PatientProcedure.apply _ tupled, PatientProcedure.unapply)

    def institutionFK =
      foreignKey("PatientProcedure_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def procedureFK =
      foreignKey("PatientProcedure_procedurePKConstraint", procedurePK, Procedure.query)(_.procedurePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
    def dicomAnonymousFK =
      foreignKey("PatientProcedure_dicomAnonymousPKConstraint", dicomAnonymousPK, DicomAnonymous.query)(_.dicomAnonymousPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
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

  /**
    * Container for the listing of PatientProcedure data in web interface.
    * @param patientProcedure Association between patient --> procedure
    * @param institution Institution this belongs to.
    * @param procedure Procedure to be used.
    */
  case class ExtendedData(patientProcedure: PatientProcedure, institution: Institution, procedure: Procedure, dicomAnonymous: DicomAnonymous) {}

  /**
    * Get the listing of PatientProcedure data in web interface.
    * @param institutionPK For this institution.
    * @return List of rows and their associated data.
    */
  def listExtended(institutionPK: Long): Seq[ExtendedData] = {
    val action = for {
      patientPosition <- query.filter(_.institutionPK === institutionPK)
      dicomAnon <- DicomAnonymous.query.filter(_.dicomAnonymousPK === patientPosition.dicomAnonymousPK)
      institution <- Institution.query.filter(_.institutionPK === institutionPK)
      procedure <- Procedure.query.filter(_.procedurePK === patientPosition.procedurePK)
    } yield (patientPosition, institution, procedure, dicomAnon)

    val list = Db.run(action.result).map(pip => ExtendedData(pip._1, pip._2, pip._3, pip._4))
    list
  }

}
