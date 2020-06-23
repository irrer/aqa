package org.aqa.db

import slick.sql.FixedSqlAction
import Db.driver.api._

trait DbTable {
  val query: Any;

  def tableQuery = query.asInstanceOf[TableQuery[Table[_]]];

  def getByPk(pk: Long, db: Database): Option[Any];

  def insOrUpdate(row: Any): FixedSqlAction[Int, slick.dbio.NoStream, slick.dbio.Effect.Write];

  def tableName: String = tableQuery.shaped.shaped.value.tableName;
}