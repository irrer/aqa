package org.aqa.db

import edu.umro.ScalaUtil.Trace
import slick.collection.heterogeneous.HNil
import slick.collection.heterogeneous.syntax._
import slick.jdbc.H2Profile.api._

import scala.concurrent.Future

// 1. Define the case class for the data model
final case class ManyStringsRow(
    id: Option[Long] = None, // Primary key, auto-incremented
    col1: String,
    col2: String,
    col3: String,
    col4: String,
    col5: String,
    col6: String,
    col7: String,
    col8: String,
    col9: String,
    col10: String,
    col11: String,
    col12: String,
    col13: String,
    col14: String,
    col15: String,
    col16: String,
    col17: String,
    col18: String,
    col19: String,
    col20: String,
    col21: String,
    col22: String,
    col23: String,
    col24: String,
    col25: String
) {
  //

  /**
    * Inserts a new row into the table.
    *
    * @return The ID of the inserted row.
    */
  def insert(): Option[Long] = {
    // Return the auto-incremented ID
    val insertQuery = (ManyStringsRow.query returning ManyStringsRow.query.map(_.id)) += this
    val result = Db.run(insertQuery)
    result
  }

}

// 2. Define the Slick Table class using HLists for the projection
class ManyStringsTable(tag: Tag) extends Table[ManyStringsRow](tag, "MANY_STRINGS") {
  def id = column[Option[Long]]("ID", O.PrimaryKey, O.AutoInc) // Use Option[Long] for auto-increment PK

  def col1 = column[String]("COL1")
  // ... define the other 23 columns similarly
  def col2 = column[String]("COL2")
  def col3 = column[String]("COL3")
  def col4 = column[String]("COL4")
  def col5 = column[String]("COL5")
  def col6 = column[String]("COL6")
  def col7 = column[String]("COL7")
  def col8 = column[String]("COL8")
  def col9 = column[String]("COL9")
  def col10 = column[String]("COL10")
  def col11 = column[String]("COL11")
  def col12 = column[String]("COL12")
  def col13 = column[String]("COL13")
  def col14 = column[String]("COL14")
  def col15 = column[String]("COL15")
  def col16 = column[String]("COL16")
  def col17 = column[String]("COL17")
  def col18 = column[String]("COL18")
  def col19 = column[String]("COL19")
  def col20 = column[String]("COL20")
  def col21 = column[String]("COL21")
  def col22 = column[String]("COL22")
  def col23 = column[String]("COL23")
  def col24 = column[String]("COL24")
  def col25 = column[String]("COL25")

  // The * projection uses HLists and mapTo to map to the case class
  def * =
    (
      id :: col1 :: col2 :: col3 :: col4 :: col5 ::
        col6 :: col7 :: col8 :: col9 :: col10 ::
        col11 :: col12 :: col13 :: col14 :: col15 ::
        col16 :: col17 :: col18 :: col19 :: col20 ::
        col21 :: col22 :: col23 :: col24 :: col25 ::
        HNil
    ).mapTo[ManyStringsRow]
}

object ManyStringsRow {
  // TableQuery object for accessing the table
  val query = TableQuery[ManyStringsTable]

  /**
    * Inserts a new row into the table.
    *
    * @param row The row data (id should be None for auto-increment).
    * @return The ID of the inserted row.
    */
  /*
  def insert(row: ManyStringsRow): Option[Long] = {
    // Return the auto-incremented ID
    val insertQuery = (ManyStringsRow.query returning ManyStringsRow.query.map(_.id)) += row
    Db.run(insertQuery)
  }
  */

  def main(args: Array[String]): Unit = {
    DbSetup.init
    Trace.trace("==========================================")

    val msr = ManyStringsRow(
      None,
      "Hey1",
      "Hey2",
      "Hey3",
      "Hey4",
      "Hey5",
      "Hey6",
      "Hey7",
      "Hey8",
      "Hey9",
      "Hey10",
      "Hey11",
      "Hey12",
      "Hey13",
      "Hey14",
      "Hey15",
      "Hey16",
      "Hey17",
      "Hey18",
      "Hey19",
      "Hey20",
      "Hey21",
      "Hey22",
      "Hey23",
      "Hey24",
      "Hey25"
    )

    // val count = insert(msr)
    val id = msr.insert()
    Trace.trace("id: " + id)

  }
}

// 3. Repository class with CRUD methods
class ManyStringsRepository(db: Database) {

  // Create the schema
  def createSchema(): Future[Unit] = db.run(ManyStringsRow.query.schema.create)

  /**
    * Inserts a new row into the table.
    *
    * @param row The row data (id should be None for auto-increment).
    * @return The ID of the inserted row.
    */
  def insert(row: ManyStringsRow): Future[Option[Long]] = {
    // Return the auto-incremented ID
    val insertQuery = (ManyStringsRow.query returning ManyStringsRow.query.map(_.id)) += row
    db.run(insertQuery)
  }

  /**
    * Updates an existing row in the table.
    *
    * @param row The row data, including the ID of the row to update.
    * @return A Future with the number of affected rows (1 if successful).
    */
  def update(row: ManyStringsRow): Future[Int] = {
    row.id match {
      case Some(id) =>
        val updateQuery = ManyStringsRow.query.filter(_.id === id).update(row)
        db.run(updateQuery)
      case None => Future.failed(new IllegalArgumentException("Cannot update row without an ID"))
    }
  }

  /**
    * Deletes a row by its ID.
    *
    * @param id The ID of the row to delete.
    * @return A Future with the number of affected rows (1 if successful).
    */
  def delete(id: Long): Future[Int] = {
    val deleteQuery = ManyStringsRow.query.filter(_.id === id).delete
    db.run(deleteQuery)
  }

  /**
    * Retrieves all rows from the table.
    */
  def getAll: Future[Seq[ManyStringsRow]] = {
    db.run(ManyStringsRow.query.result)
  }
}
