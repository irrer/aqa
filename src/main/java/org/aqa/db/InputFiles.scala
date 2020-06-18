package org.aqa.db

import Db.driver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.security.InvalidParameterException

/**
 * Store the zipped file contents of an Input.
 *
 * The convention for the primary key, which is enforced by the code, is to use the same pk for this
 * table as the corresponding input entry.  This is to help ensure that there is at most one InputFiles
 * for each Input.
 *
 * Note that this tables does NOT use an auto-incrementing counter for the primary key,
 */
case class InputFiles(
  inputFilesPK: Long, // primary key
  inputPK: Long, // referenced input
  zippedContent: Array[Byte]) // The files in zip form created by the process
  {

  def insert: InputFiles = {
    val insertQuery = InputFiles.query returning InputFiles.query.map(_.inputFilesPK) into
      ((epidCenterCorrection, inputFilesPK) => epidCenterCorrection.copy(inputFilesPK = inputFilesPK))
    Db.run(insertQuery += this)
  }
}

object InputFiles extends Logging {
  class InputFilesTable(tag: Tag) extends Table[InputFiles](tag, "inputFiles") {

    def inputFilesPK = column[Long]("inputFilesPK", O.PrimaryKey)
    def inputPK = column[Long]("inputPK")
    def zippedContent = column[Array[Byte]]("zippedContent")

    def * = (inputFilesPK, inputPK, zippedContent) <> ((InputFiles.apply _)tupled, InputFiles.unapply _)

    def inputFK = foreignKey("inputPKConstraint", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[InputFilesTable]

  def get(inputFilesPK: Long): Option[InputFiles] = {
    val action = for {
      inputFiles <- query if inputFiles.inputFilesPK === inputFilesPK
    } yield (inputFiles)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByInputPK(inputPK: Long): Seq[InputFiles] = {
    val action = for {
      inputFiles <- query if inputFiles.inputPK === inputPK
    } yield (inputFiles)
    val list = Db.run(action.result)
    list
  }

  def delete(inputFilesPK: Long): Int = {
    val q = query.filter(_.inputFilesPK === inputFilesPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByInputPK(inputPK: Long): Int = {
    val q = query.filter(_.inputPK === inputPK)
    val action = q.delete
    Db.run(action)
  }

}
