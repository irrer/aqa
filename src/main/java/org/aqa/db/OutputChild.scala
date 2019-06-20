package org.aqa.db

/**
 * Support general handling of database tables that point to the Output table.
 */
trait OutputChild {
  /**
   * Make a copy of this object with the output PK updated to the given value.
   */
  def updateOutputPK(outputPK: Long): OutputChild;

  /**
   * Insert this object into the database
   */
  def insert: Unit;
}