package org.aqa.db

import Db.driver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.security.InvalidParameterException

/**
 * Store the zipped file contents of an Output.
 *
 * The convention for the primary key, which is enforced by the code, is to use the same pk for this
 * table as the corresponding output entry.  This is to help ensure that there is at most one OutputFiles
 * for each Output.
 *
 * Note that this tables does NOT use an auto-incrementing counter for the primary key,
 */
case class OutputFiles(
  outputFilesPK: Long, // primary key
  outputPK: Long, // referenced output
  zippedContent: Array[Byte]) // The files in zip form created by the process
  {
  def insert: OutputFiles = {
    val insertQuery = OutputFiles.query returning OutputFiles.query.map(_.outputFilesPK) into
      ((epidCenterCorrection, outputFilesPK) => epidCenterCorrection.copy(outputFilesPK = outputFilesPK))
    Db.run(insertQuery += this)
  }
}

object OutputFiles extends Logging {
  class OutputFilesTable(tag: Tag) extends Table[OutputFiles](tag, "outputFiles") {

    def outputFilesPK = column[Long]("outputFilesPK", O.PrimaryKey)
    def outputPK = column[Long]("outputPK")
    def zippedContent = column[Array[Byte]]("zippedContent")

    def * = (outputFilesPK, outputPK, zippedContent) <> ((OutputFiles.apply _)tupled, OutputFiles.unapply _)

    def outputFK = foreignKey("outputPKConstraint", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputFilesTable]

  def get(outputFilesPK: Long): Option[OutputFiles] = {
    val action = for {
      outputFiles <- query if outputFiles.outputFilesPK === outputFilesPK
    } yield (outputFiles)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByOutput(outputPK: Long): Option[OutputFiles] = {
    val action = for {
      outputFiles <- query if outputFiles.outputPK === outputPK
    } yield (outputFiles)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(outputFilesPK: Long): Int = {
    val q = query.filter(_.outputFilesPK === outputFilesPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

}
