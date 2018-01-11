package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import edu.umro.ScalaUtil.FileUtil
import org.aqa.Util

/**
 * Store the zipped file contents of an Input.
 */
case class InputData(
  inputDataPK: Option[Long], // primary key
  inputPK: Long, // referenced input
  data: Array[Byte]) // The files in zip form created by the process
  {

  def insert: InputData = {
    val insertQuery = InputData.query returning InputData.query.map(_.inputDataPK) into ((inputData, inputDataPK) => inputData.copy(inputDataPK = Some(inputDataPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(InputData.query.insertOrUpdate(this))
}

object InputData extends Logging {
  class InputDataTable(tag: Tag) extends Table[InputData](tag, "inputData") {

    def inputDataPK = column[Long]("inputDataPK", O.PrimaryKey, O.AutoInc)
    def inputPK = column[Long]("inputPK")
    def data = column[Array[Byte]]("data")

    def * = (inputDataPK.?, inputPK, data) <> ((InputData.apply _)tupled, InputData.unapply _)

    def inputFK = foreignKey("inputPK", inputPK, Input.query)(_.inputPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[InputDataTable]

  def fileName(name: String): String = FileUtil.replaceInvalidFileNameCharacters(name, '_')

  def get(inputDataPK: Long): Option[InputData] = {
    val action = for {
      inputData <- query if inputData.inputDataPK === inputDataPK
    } yield (inputData)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(inputDataPK: Long): Int = {
    val q = query.filter(_.inputDataPK === inputDataPK)
    val action = q.delete
    Db.run(action)
  }

}
