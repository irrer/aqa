/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.db

//import Db.driver.api._
import edu.umro.util.Utility
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Crypto
import org.aqa.Logging
import org.aqa.Util

import java.io.File

/** Establish connection to the database and ensure that tables are created. */

object DbSetup extends Logging {

  import org.aqa.db.Db.driver.api._

  private def makeDefaultInstitution: Institution = {
    val inst = new Institution(None, "AQA", None, Util.aqaUrl, "Automated Quality Assurance").insert
    def encrypt(text: String) = AnonymizeUtil.encryptWithNonce(inst.institutionPK.get, text)

    inst.copy(
      name = AnonymizeUtil.aliasify(AnonymizeUtil.institutionAliasPrefixId, inst.institutionPK.get),
      name_real = Some(encrypt("AQA")),
      url_real = encrypt("https://medicine.umich.edu/dept/radiation-oncology"),
      description_real = encrypt("to be determined")
    )
    inst.insertOrUpdate()
    inst
  }

  private def ensureAtLeastOneInstitution: Institution = {
    Institution.list.headOption match {
      case Some(inst) => inst
      case None =>
        makeDefaultInstitution
    }
  }

  private def makeAdminUser: User = {
    val defaultUser = {
      if (Config.UserWhiteList.nonEmpty)
        Config.UserWhiteList.head
      else
        "admin"
    }
    val defaultPassword = "admin"

    logger.info("Creating default user " + defaultUser + " with password: " + defaultPassword)

    val email = defaultUser + "@" + Util.aqaDomain
    val passwordSalt = Crypto.randomSecureHash
    val hashedPassword = CachedUser.hashPassword(defaultPassword, passwordSalt)

    val institutionPK = ensureAtLeastOneInstitution.institutionPK.get
    def encrypt(text: String) = AnonymizeUtil.encryptWithNonce(institutionPK, text)

    val adminUser = new User(None, "admin", Some(encrypt("admin")), encrypt("An Administrator"), encrypt(email), institutionPK, hashedPassword, passwordSalt, UserRole.admin.toString, None)
    val user = adminUser.insert.copy(id = AnonymizeUtil.aliasify(AnonymizeUtil.userAliasPrefixId, institutionPK))
    user.insertOrUpdate()
    User.get(user.userPK.get).get
  }

  /** Ensure that the system has at least one admin user.  If there is not one, then create one. */
  private def ensureAdminUser = {
    User.getUserListByRole(UserRole.admin).headOption match {
      case Some(user) => user
      case None       => makeAdminUser
    }
  }

  private val tableQueryList = List(
    Institution.query,
    User.query,
    Procedure.query,
    SystemModification.query,
    MachineType.query,
    MultileafCollimator.query,
    EPID.query,
    Machine.query,
    MachineBeamEnergy.query,
    MachineDailyQA.query,
    MachineWL.query,
    Input.query,
    InputFiles.query,
    MachineLog.query,
    MaintenanceRecord.query,
    Output.query,
    OutputFiles.query,
    DicomSeries.query,
    DicomAnonymous.query,
    PatientProcedure.query,
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
    WedgePoint.query,
    BBbyCBCT.query,
    BBbyEPID.query,
    FocalSpot.query,
    BBbyEPID.query,
    FocalSpotSet.query,
    Rect.query,
    SymmetryAndFlatness.query,
    LeafPosition.query,
    VMAT.query,
    GapSkew.query,
    WinstonLutz.query
  )

  /**
    * Initialize database by creating tables in dependency order.
    */
  def init: Boolean = {
    logger.info("Initializing connection to database")
    Config.validate // force configuration to be read

    logger.info("List of required tables in reverse order of creation:\n    " + tableQueryList.reverse.map(table => table.shaped.value.tableName).mkString("\n    "))

    tableQueryList.foreach(q => Db.createTableIfNonexistent(q.asInstanceOf[TableQuery[Table[_]]]))
    ensureAdminUser
    logger.info("Done initializing connection to database")
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
      logger.info("Starting database smoke test")
      init

      def readOne(query: TableQuery[Table[_]]): Unit = {
        val tableName = query.shaped.shaped.value.value.tableName
        logger.info("Verifying table " + tableName)
        val row = Db.run(query.take(1).result)
        if (row.nonEmpty) logger.info("    verified: " + row.head.getClass.getName + "    with row:  " + row.head)
        else logger.info("Table " + tableName + " is empty")
      }

      tableQueryList.foreach(q => readOne(q.asInstanceOf[TableQuery[Table[_]]]))
      logger.info("Database smoke test completed without error")

      logger.info("Finished database smoke test")
      true

    } catch {
      case t: Throwable =>
        logger.error("Failed database smoke test: " + fmtEx(t))
        false
    }
  }

  /**
    * If there are files that should be stored in the database, but are not, then put them in the database.  This includes
    * input and output files.  Also, put any missing LOC baseline files in their respective output directories.
    *
    * This function was written to support the migration towards storing all input and output files in the database.  It may
    * become obsolete in the future.
    */
  def storeFilesInDatabase: Seq[Any] = {

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
        } yield oo
        Db.run(outQuery.result)
      }

      case class ConfigFile(file: File) {
        val hash: List[Byte] = Crypto.secureHash(Utility.readBinFile(file)).toList
      }

      val knownMachConfig: Map[List[Byte], ConfigFile] = {
        val empty = Seq[ConfigFile]()
        def doDir(dir: File): Seq[ConfigFile] = {
          def doFile(file: File): Seq[ConfigFile] = {
            try {
              Seq(ConfigFile(file))
            } catch {
              case _: Throwable => empty
            }
          }
          if (dir.isDirectory) {
            dir.listFiles.filter(f => f.getName.endsWith("_Baseline.dcm")).flatMap(f => doFile(f)).toSeq
          } else empty
        }
        val allConfig = Config.machineConfigurationDirFile.listFiles.flatMap(d => doDir(d)).toSeq
        allConfig.map(cf => (cf.hash, cf)).toMap
      }

      def doOut(output: Output) = {
        try {
          val outDir = output.dir
          val inList = outDir.getParentFile.listFiles.filter(f => f.isFile && f.canRead).map(f => ConfigFile(f)).toSeq
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
        if ! { for { dpk <- OutputFiles.query.map(d => d.outputPK).filter(d => d === oo.outputPK) } yield dpk }.exists
      } yield oo

      val notStored = Db.run(si.result)
      logger.info("Number of outputs that need data updated: " + notStored.size)

      notStored.map(oo => {
        try {
          logger.info("Updating OutputFiles for " + oo.outputPK.get + " : " + oo.dir)
          oo.updateData(oo.makeZipOfFiles)
        } catch {
          case t: Throwable =>
            logger.warn("Could not update data for output " + oo + "\n    : " + t)
        }
      })
    }

    def copyInput = {
      val si = for {
        ii <- Input.query.map(i => i)
        if ! { for { dpk <- InputFiles.query.map(d => d.inputPK).filter(d => d === ii.inputPK) } yield dpk }.exists
      } yield ii

      val notStored = Db.run(si.result)
      logger.info("Number of inputs that have files that need to be put in the database as a zip: " + notStored.size)

      notStored.map(ii => {
        try {
          logger.info("Updating InputFiles for " + ii.inputPK.get + " : " + ii.dir)
          ii.putFilesInDatabase(ii.dir)
        } catch {
          case t: Throwable =>
            logger.warn("Could not update data for input " + ii + "\n    : " + t)
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
