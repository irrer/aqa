package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util
import java.security.InvalidParameterException

/**
 * Store the zipped file contents of an Input.
 *
 * The convention for the primary key, which is enforced by the code, is to use the same pk for this
 * table as the corresponding input entry.  This is to help ensure that there is at most one InputData
 * for each Input.
 *
 * Note that this tables does NOT use an auto-incrementing counter for the primary key,
 */
case class InputData(
  inputDataPK: Long, // primary key
  inputPK: Long, // referenced input
  data: Array[Byte]) // The files in zip form created by the process
  {

  def insert: InputData = {
    val insertQuery = InputData.query returning InputData.query.map(_.inputDataPK) into
      ((epidCenterCorrection, inputDataPK) => epidCenterCorrection.copy(inputDataPK = inputDataPK))
    Db.run(insertQuery += this)
  }

  //  private def XinsertOrUpdate = {
  //    if (inputDataPK != inputPK) throw new InvalidParameterException("inputDataPK != inputPK : " + inputDataPK + " != " + inputPK)
  //    Db.run(InputData.query.insertOrUpdate(this))
  //  }
}

object InputData extends Logging {
  class InputDataTable(tag: Tag) extends Table[InputData](tag, "inputData") {

    def inputDataPK = column[Long]("inputDataPK")
    def inputPK = column[Long]("inputPK")
    def data = column[Array[Byte]]("data")

    def * = (inputDataPK, inputPK, data) <> ((InputData.apply _)tupled, InputData.unapply _)

    def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[InputDataTable]

  def get(inputDataPK: Long): Option[InputData] = {
    val action = for {
      inputData <- query if inputData.inputDataPK === inputDataPK
    } yield (inputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def getByInput(inputPK: Long): Option[InputData] = {
    val action = for {
      inputData <- query if inputData.inputPK === inputPK
    } yield (inputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(inputDataPK: Long): Int = {
    val q = query.filter(_.inputDataPK === inputDataPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByInputPK(inputPK: Long): Int = {
    val q = query.filter(_.inputPK === inputPK)
    val action = q.delete
    Db.run(action)
  }

}
