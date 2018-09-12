package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import java.sql.Timestamp

/**
 * Define a type of baseline value.
 */
case class BaselineType(
  baselineTypePK: Option[Long], // primary key
  id: String, // unique identifier for data.  Can contain the concatenation of values such as beam name, energy level, jaw position, energy level, etc.  Should be human readable / user friendly
  description: String // a description that helps the user understand what it is.  This is not used for computation.
) {

  def insert: BaselineType = {
    val insertQuery = BaselineType.query returning BaselineType.query.map(_.baselineTypePK) into ((baselineType, baselineTypePK) => baselineType.copy(baselineTypePK = Some(baselineTypePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BaselineType.query.insertOrUpdate(this))
}

object BaselineType extends Logging {

  class BaselineTypeTable(tag: Tag) extends Table[BaselineType](tag, "baselineType") {

    def baselineTypePK = column[Long]("baselineTypePK", O.PrimaryKey, O.AutoInc)
    def id = column[String]("id")
    def description = column[String]("description")

    def * = (
      baselineTypePK.?,
      id,
      description) <> ((BaselineType.apply _)tupled, BaselineType.unapply _)
  }

  val query = TableQuery[BaselineTypeTable]

  def get(baselineTypePK: Long): Option[BaselineType] = {
    val action = for {
      baselineType <- query if baselineType.baselineTypePK === baselineTypePK
    } yield (baselineType)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(baselineTypePK: Long): Int = {
    val q = query.filter(_.baselineTypePK === baselineTypePK)
    logger.info("deleting baselineType " + baselineTypePK)
    val action = q.delete
    Db.run(action)
  }
}
