package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.security.InvalidParameterException

/**
 * Store the zipped file contents of an Output.
 *
 * The convention for the primary key, which is enforced by the code, is to use the same pk for this
 * table as the corresponding output entry.  This is to help ensure that there is at most one OutputData
 * for each Output.
 *
 * Note that this tables does NOT use an auto-incrementing counter for the primary key,
 */
case class OutputData(
  outputDataPK: Long, // primary key
  outputPK: Long, // referenced output
  data: Array[Byte]) // The files in zip form created by the process
  {

  def insert: OutputData = {
    val insertQuery = OutputData.query returning OutputData.query.map(_.outputDataPK) into
      ((epidCenterCorrection, outputDataPK) => epidCenterCorrection.copy(outputDataPK = outputDataPK))
    Db.run(insertQuery += this)
  }

  //  private def XinsertOrUpdate = {
  //    if (outputDataPK != outputPK) throw new InvalidParameterException("outputDataPK != outputPK : " + outputDataPK + " != " + outputPK)
  //    Db.run(OutputData.query.insertOrUpdate(this))
  //  }
}

object OutputData extends Logging {
  class OutputDataTable(tag: Tag) extends Table[OutputData](tag, "outputData") {

    def outputDataPK = column[Long]("outputDataPK", O.PrimaryKey)
    def outputPK = column[Long]("outputPK")
    def data = column[Array[Byte]]("data")

    def * = (outputDataPK, outputPK, data) <> ((OutputData.apply _)tupled, OutputData.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[OutputDataTable]

  def get(outputDataPK: Long): Option[OutputData] = {
    val action = for {
      outputData <- query if outputData.outputDataPK === outputDataPK
    } yield (outputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByOutput(outputPK: Long): Option[OutputData] = {
    val action = for {
      outputData <- query if outputData.outputPK === outputPK
    } yield (outputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(outputDataPK: Long): Int = {
    val q = query.filter(_.outputDataPK === outputDataPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

}
