package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import edu.umro.util.Utility

/**
 * Support the anonymization of DICOM by providing a way to store previously anonymized values in the database.
 */
case class DicomAnonymize(
  dicomAnonymizePK: Option[Long], // primary key
  institutionPK: Long, // associated institution
  attributeTag: String, // DICOM attribute tag
  value: String, // public value for tag
  attributeHash: String, // secure hash of tag and value to allow searches on this row.
  value_real: String // actual non-anonymized value encrypted
) {

  def insert = {
    val insertQuery = DicomAnonymize.query returning DicomAnonymize.query.map(_.dicomAnonymizePK) into ((dicomAnonymize, dicomAnonymizePK) => dicomAnonymize.copy(dicomAnonymizePK = Some(dicomAnonymizePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(DicomAnonymize.query.insertOrUpdate(this))
}

object DicomAnonymize extends Logging {

  class DicomAnonymizeTable(tag: Tag) extends Table[DicomAnonymize](tag, "dicomAnonymize") {

    def dicomAnonymizePK = column[Long]("dicomAnonymizePK", O.PrimaryKey, O.AutoInc)
    def institutionPK = column[Long]("institutionPK")
    def attributeTag = column[String]("attributeTag")
    def value = column[String]("value")
    def attributeHash = column[String]("attributeHash")
    def value_real = column[String]("value_real")

    def * = (
      dicomAnonymizePK.?,
      institutionPK,
      attributeTag,
      value,
      attributeHash,
      value_real) <> ((DicomAnonymize.apply _)tupled, DicomAnonymize.unapply _)

    def institutionFK = foreignKey("institutionPK", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomAnonymizeTable]

  def get(dicomAnonymizePK: Long): Option[DicomAnonymize] = {
    val action = query.filter(m => m.dicomAnonymizePK === dicomAnonymizePK)
    val list = Db.run(action.result)
    list.headOption
  }

  def listDicomAnonymizesFromInstitution(institutionPK: Long): Seq[DicomAnonymize] = {
    val action = query.filter(m => m.institutionPK === institutionPK)
    val seq = Db.run(action.result)
    seq
  }

  /**
   * Delete the dicomAnonymize from the database.  If that is successful, then also delete its configuration directory.
   */
  def delete(dicomAnonymizePK: Long): Int = {
    val dicomAnonymize = get(dicomAnonymizePK)
    if (dicomAnonymize.isDefined) {
      val q = query.filter(_.dicomAnonymizePK === dicomAnonymizePK)
      val action = q.delete
      val count = Db.run(action)
      count
    } else 0
  }

}
