package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Util
import org.aqa.web.AuthenticationVerifier
import java.sql.Date
import org.aqa.Logging
import java.sql.Timestamp
import org.aqa.run.ProcedureStatus
import scala.collection.mutable.ArrayBuffer
import org.aqa.Config
import edu.umro.util.OpSys
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import java.io.File

/** Establish connection to the database and ensure that tables are created. */

object DbSetup extends Logging {

  private def makeDummyInstitution: Institution = new Institution(None, "AQA", Util.aqaUrl, "Automated Quality Assurance")

  private def ensureAtLeastOneInstitution: Institution = {
    Institution.list.headOption match {
      case Some(inst) => inst
      case None => {
        makeDummyInstitution.insert
      }
    }
  }

  private def makeAdminUser: User = {
    val defaultUser = "admin"
    val defaultPassword = "admin"

    val email = defaultUser + "@" + Util.aqaDomain
    val passwordSalt = Util.randomSecureHash
    val hashedPassword = AuthenticationVerifier.hashPassword(defaultPassword, passwordSalt)

    val institutionPK = ensureAtLeastOneInstitution.institutionPK.get
    val adminUser = new User(None, "admin", "An Administrator", email, institutionPK, hashedPassword, passwordSalt, UserRole.admin.toString, None)
    adminUser.insert
  }

  /** Ensure that the system has at least one admin user.  If there is not one, then create one. */
  private def ensureAdminUser = {
    User.getUserListByRole(UserRole.admin).headOption match {
      case Some(user) => user
      case None => makeAdminUser
    }
  }

  private val tableQueryList = List(
    Institution.query,
    User.query,
    Procedure.query,
    MachineType.query,
    MultileafCollimator.query,
    EPID.query,
    Machine.query,
    MachineBeamEnergy.query,
    Input.query,
    InputData.query,
    MaintenanceRecord.query,
    Output.query,
    OutputData.query,
    CentralAxis.query,
    LeafOffsetCorrection.query,
    EPIDCenterCorrection.query,
    LeafTransmission.query,
    LOCRSquared.query,
    DiffBaselineOpen.query,
    DiffBaselineTrans.query)

  /**
   * Initialize database by creating tables in dependency order.
   */
  lazy val init: Boolean = {
    val valid = Config.validate // force configuration to be read

    def setIfDefined(key: String, value: Option[String]): Unit = {
      value match {
        case (Some(v)) => System.setProperty(key, v)
        case _ => ;
      }
    }

    // Slick expects these properties to be set
    System.setProperty("slick.dbs.default.db.url", Config.SlickDbsDefaultDbUrl)
    System.setProperty("slick.dbs.default.driver", Config.SlickDbsDefaultDriver)
    System.setProperty("slick.dbs.default.db.driver", Config.SlickDbsDefaultDbDriver)
    setIfDefined("slick.dbs.default.db.user", Config.SlickDbsDefaultDbUser)
    setIfDefined("slick.dbs.default.db.password", Config.SlickDbsDefaultDbPassword)

    tableQueryList.map(q => Db.createTableIfNonexistent(q.asInstanceOf[TableQuery[Table[_]]]))
    ensureAdminUser
    true
  }

  /**
   * This does a quick verification that the table definitions in the database match the definition in the code.  It does not
   * check constraints.  In order to be checked, a table must contain at least one row, and if not, it is ignored.  If a table
   * contains an extra column that is not defined in the code then the table will still pass as verified.
   *
   *  This test might leave garbage in the database.  Do not use it.  Need to compare schema instead.
   */
  def smokeTest: Boolean = {

    try {
      init

      val timestamp = new Timestamp(System.currentTimeMillis)

      def readOne(query: TableQuery[Table[_]]): Unit = {
        val tableName = query.shaped.shaped.value.value.tableName
        logger.info("Verifying table " + tableName)
        val row = Db.run(query.take(1).result)
        if (row.size > 0) logger.info("    verified: " + row.head.getClass.getName + "    with row:  " + row.head)
        else logger.info("Table " + tableName + " is empty")
      }

      tableQueryList.map(q => readOne(q.asInstanceOf[TableQuery[Table[_]]]))

      true

    } catch {
      case t: Throwable => {
        logger.error("Failed database smoke test: " + fmtEx(t))
        false
      }
    }
  }

  def copyFilesToDatabase = {
    def copyOutput = {
      val action = Output.query.filter(m => m.data.isEmpty)
      val list = Db.run(action.result)
      println("copyFilesToDatabase Output list size: " + list.size)

      def copyFiles(output: Output) = {
        Db.run(Output.query.filter(_.outputPK === output.outputPK.get).map(o => (o.data)).update((Some(output.makeZipOfData))))
      }
      list.map(output => copyFiles(output))
    }

    def copyInput = {
      val action = Input.query.filter(m => m.data.isEmpty && m.directory.isDefined)
      val list = Db.run(action.result)
      println("copyFilesToDatabase Input list size: " + list.size)
      list.map(input => input.updateDirectoryAndData(input.dir))
    }

    copyOutput
    copyInput
  }

  def main(args: Array[String]): Unit = {
    println("smokeTest passes: " + smokeTest)
  }
}