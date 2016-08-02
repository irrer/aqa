package org.aqac.db

import slick.driver.PostgresDriver.api._

/** Establish connection to the database and ensure that tables are created. */

object DbSetup {

    /**
     * Initialize database by creating tables in dependency order.
     */
    def init = {
        Db.createTableIfNonexistent(Institution.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(User.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(Procedure.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(MachineType.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(Machine.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(Input.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(Output.query.asInstanceOf[TableQuery[Table[_]]])
        Db.createTableIfNonexistent(MaxLeafGap.query.asInstanceOf[TableQuery[Table[_]]])
    }
}