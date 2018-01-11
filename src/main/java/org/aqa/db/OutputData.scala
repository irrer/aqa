package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util

/**
 * Store the zipped file contents of an Output.
 */
case class OutputData(
  outputDataPK: Option[Long], // primary key
  outputPK: Long, // referenced output
  data: Array[Byte]) // The files in zip form created by the process
  {

  def insert: OutputData = {
    val insertQuery = OutputData.query returning OutputData.query.map(_.outputDataPK) into ((outputData, outputDataPK) => outputData.copy(outputDataPK = Some(outputDataPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(OutputData.query.insertOrUpdate(this))
}

object OutputData extends Logging {
  class OutputDataTable(tag: Tag) extends Table[OutputData](tag, "outputData") {

    def outputDataPK = column[Long]("outputDataPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def data = column[Array[Byte]]("data")

    def * = (outputDataPK.?, outputPK, data) <> ((OutputData.apply _)tupled, OutputData.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputDataTable]

  def fileName(name: String): String = FileUtil.replaceInvalidFileNameCharacters(name, '_')

  def get(outputDataPK: Long): Option[OutputData] = {
    val action = for {
      outputData <- query if outputData.outputDataPK === outputDataPK
    } yield (outputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(outputDataPK: Long): Int = {
    val q = query.filter(_.outputDataPK === outputDataPK)
    val action = q.delete
    Db.run(action)
  }

}
