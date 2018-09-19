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
import edu.umro.util.Utility

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
    InputFiles.query,
    PMI.query,
    Output.query,
    OutputFiles.query,
    Baseline.query,
    BaselineContent.query,
    CentralAxis.query,
    LeafOffsetCorrection.query,
    EPIDCenterCorrection.query,
    LeafTransmission.query,
    LOCRSquared.query,
    DiffBaselineOpen.query,
    DiffBaselineTrans.query,
    MetadataCheck.query,
    CollimatorCentering.query,
    BadPixel.query,
    CenterDose.query,
    CollimatorPosition.query,
    Wedge.query,
    SymmetryAndFlatness.query)

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
    System.setProperty("slick.dbs.default.db.url", Config.SlickDbsDefaultDbUrl.trim) // trim because sometimes XML formatter adds newline to URL
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

  /**
   * If there are files that should be stored in the database, but are not, then put them in the database.  This includes
   * input and output files.  Also, put any missing LOC baseline files in their respective output directories.
   *
   * This function was written to support the migration towards storing all input and output files in the database.  It may
   * become obsolete in the future.
   */
  def storeFilesInDatabase = {

    /**
     * At one point the LOC baseline procedure did not rename and copy the files into the output directory.  This
     * corrects that by copying them from the machine configuration directory to the proper output directory.
     */
    def retroactivelyFixLOCBaseline = {

      // PK of LOCUploadBaseFiles_1 procedure
      val baselineProcPK: Long = Db.run(Procedure.query.result).filter(p => p.webInterface.toLowerCase.contains("base")).head.procedurePK.get

      val outList = {
        val outQuery = for {
          oo <- Output.query.filter(o => o.procedurePK === baselineProcPK)
        } yield (oo)
        Db.run(outQuery.result)
      }

      case class ConfigFile(file: File) {
        val hash = Util.secureHash(Utility.readBinFile(file)).toList
      }

      val knownMachConfig: Map[List[Byte], ConfigFile] = {
        val empty = Seq[ConfigFile]()
        def doDir(dir: File): Seq[ConfigFile] = {
          def doFile(file: File): Seq[ConfigFile] = {
            try {
              Seq(new ConfigFile(file))
            } catch {
              case t: Throwable => empty
            }
          }
          if (dir.isDirectory()) {
            dir.listFiles.filter(f => f.getName.endsWith("_Baseline.dcm")).map(f => doFile(f)).flatten.toSeq
          } else empty
        }
        val allConfig = Config.machineConfigurationDirFile.listFiles.map(d => doDir(d)).flatten.toSeq
        allConfig.map(cf => (cf.hash, cf)).toMap
      }

      def doOut(output: Output) = {
        try {
          val outDir = output.dir
          val inDir = outDir.getParentFile
          val inList = outDir.getParentFile.listFiles.filter(f => f.isFile && f.canRead).map(f => new ConfigFile(f)).toSeq
          val baselineList = inList.filter(cf => knownMachConfig.contains(cf.hash))
          logger.info("Number of outputs that have files that need to be put in the database as a zip: " + baselineList.size)
          baselineList.map(b => {
            val c = knownMachConfig(b.hash)
            val data = Utility.readBinFile(c.file)
            val outFile = new File(outDir, c.file.getName)
            if (!outFile.exists) {
              logger.info("Copying baseline file " + c.file.getAbsolutePath + "  -->  " + outFile.getAbsolutePath)
              Utility.writeFile(outFile, data)
            }
          })
        } catch {
          case t: Throwable => logger.error("failure to copy baseline file: " + t)
        }
      }

      outList.map(o => doOut(o))

    }

    def copyOutput = {
      val si = for {
        oo <- Output.query.map(o => o)
        if (!({ for { dpk <- OutputFiles.query.map(d => d.outputPK).filter(d => d === oo.outputPK) } yield dpk }.exists))
      } yield (oo)

      val notStored = Db.run(si.result)
      logger.info("Number of outputs that need data updated: " + notStored.size)

      notStored.map(oo => {
        try {
          logger.info("Updating OutputFiles for " + oo.outputPK.get + " : " + oo.dir)
          oo.updateData(oo.makeZipOfFiles)
        } catch {
          case t: Throwable => {
            logger.warn("Could not update data for output " + oo + "\n    : " + t)
          }
        }
      })
    }

    def copyInput = {
      val si = for {
        ii <- Input.query.map(i => i)
        if (!({ for { dpk <- InputFiles.query.map(d => d.inputPK).filter(d => d === ii.inputPK) } yield dpk }.exists))
      } yield (ii)

      val notStored = Db.run(si.result)
      logger.info("Number of inputs that have files that need to be put in the database as a zip: " + notStored.size)

      notStored.map(ii => {
        try {
          logger.info("Updating InputFiles for " + ii.inputPK.get + " : " + ii.dir)
          ii.putFilesInDatabase(ii.dir)
        } catch {
          case t: Throwable => {
            logger.warn("Could not update data for input " + ii + "\n    : " + t)
          }
        }
      })
    }

    retroactivelyFixLOCBaseline
    copyInput
    copyOutput
  }

  def main(args: Array[String]): Unit = {
    println("smokeTest passes: " + smokeTest)
  }
}
