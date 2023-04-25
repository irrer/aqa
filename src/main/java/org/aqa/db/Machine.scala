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

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.Utility
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.DicomFile
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Db.driver.api._
import org.aqa.web.AnonymousTranslate

import java.io.File

case class Machine(
    machinePK: Option[Long], // primary key
    id: String, // alias identifying name
    id_real: Option[String], // real name uniquely identifying name within hosting institution
    machineTypePK: Long, // type of machine foreign key
    configurationDirectory: Option[String], // directory containing configuration files unique to this machine
    multileafCollimatorPK: Long, // collimator
    epidPK: Long, // EPID
    institutionPK: Long, // institution that this machine belongs to
    serialNumber: Option[String], // encrypted version of the machine's serial number.  Becomes defined when a user associates data with it via the web interface.
    imagingBeam2_5_mv: Boolean, // True if this is supported.  Defaults to false.
    onboardImager: Boolean, // True if this is supported.  Defaults to false.
    table6DOF: Boolean, // True if (six degrees of freedom table) is supported.  Defaults to false.
    respiratoryManagement: Boolean, // True if this is supported.  Defaults to false.
    developerMode: Boolean, // True if this is supported.  Defaults to false.
    active: Boolean, // True if the machine is actively being used.  Defaults to true.  Setting to false may exclude this machine's data from some reports.
    tpsID_real: Option[String], // Equivalent DICOM: 3002,0020 RadiationMachineName.  ID of the machine in its associated treatment planning system.  Stored as encrypted.
    notes: String // optional further information
) extends Logging {

  def insert: Machine = {
    val insertQuery = Machine.query returning Machine.query.map(_.machinePK) into ((machine, machinePK) => machine.copy(machinePK = Some(machinePK)))
    val action = insertQuery += this
    val result = Db.run(action)
    AnonymousTranslate.clearCache(institutionPK)
    result
  }

  def insertOrUpdate(): Int = {
    val count = Db.run(Machine.query.insertOrUpdate(this))
    AnonymousTranslate.clearCache(institutionPK)
    count
  }

  def fileName: String = Machine.fileName(id)

  def configDir: Option[File] = if (configurationDirectory.isDefined) Some(Machine.getConfigDir(configurationDirectory.get)) else None

  override def toString: String = {
    def fmt(text: String) = text.take(8) + "..."
    def fmtB(boo: Boolean) = boo.toString.subSequence(0, 1)
    "machinePK: " + (if (machinePK.isDefined) machinePK.get else "None") +
      "  id: " + id +
      "  id_real: " + (if (id_real.isDefined) fmt(id_real.get) else "None") +
      "  machineTypePK: " + machineTypePK +
      "  config dir : " + (if (configurationDirectory.isDefined) fmt(configurationDirectory.get) else "None") +
      "  MLC PK: " + multileafCollimatorPK +
      "  epidPK: " + epidPK +
      "  institutionPK: " + institutionPK +
      "  serialNumber: " + (if (serialNumber.isDefined) serialNumber.get else "None") +
      "  image 2_5_mv: " + fmtB(imagingBeam2_5_mv) +
      "  onboardImager: " + fmtB(onboardImager) +
      "  6DOF Table: " + fmtB(table6DOF) +
      "  resp mgmt: " + fmtB(respiratoryManagement) +
      "  dev mode: " + fmtB(developerMode) +
      "  active: " + fmtB(active) +
      "  notes: " + fmt(notes)
  }

  /**
    * Get the real device serial number for the given machine.
    * @return DeviceSerialNumber de-anonymized.
    */
  def getRealDeviceSerialNumber: Option[String] = {
    if (serialNumber.isEmpty)
      None
    else {
      val anon = DicomAnonymous.getAttributesByTag(institutionPK, Seq(TagFromName.DeviceSerialNumber)).find(da => da.value.equals(serialNumber.get))
      if (anon.isEmpty)
        None
      else
        Some(anon.get.originalValue)
    }
  }

  /**
    * Get the real (de-anonymized) ID of this machine.
    * @return
    */
  def getRealId: String = {
    AnonymizeUtil.decryptWithNonce(institutionPK, id_real.get)
  }

  /**
    * Get the real (de-anonymized) tpsID of this machine.  In earlier versions of
    * the code this column did not exist, and so might not always be set up.
    *
    * @return The name of the machine used as the unique ID in the treatment planning system.
    */
  def getRealTpsId: Option[String] = {
    tpsID_real match {
      case Some(t) => Some(AnonymizeUtil.decryptWithNonce(institutionPK, t))
      case _       => None
    }
  }

  def distinctBy[T, C](list: Iterable[T], func: T => C): Iterable[T] = {
    list.groupBy(func).map(_._2.head)
  }

  /**
    * Set the tpsId_real value if it is not set.
    *
    * A complication is that for some time the RadiationMachineName was not anonymized,
    * and then later it was.  This method covers both cases.
    *
    * @param alListAny Get the machine name from here.  Only RTIMAGE files are searched.
    *
    * @return None if it was updated.  False if no change,
    */
  def setTpsIdIfNeeded(alListAny: Seq[AttributeList], write: Boolean = true): Option[String] = {

    val alList = alListAny.filter(Util.isRtimage)

    def deAnon(attr: Attribute): Attribute = {
      AnonymizeUtil.deAnonymizeAttribute(institutionPK, attr) match {
        case Some(a) => a
        case _       => attr // use original
      }
    }

    def properModality(al: AttributeList): Boolean = {
      Set("CT", "REG", "RTIMAGE").contains(Util.modalityOfAl(al))
    }

    // only update if it is not defined or is blank, and the machine is already in the database
    if ((tpsID_real.isEmpty || getRealTpsId.get.trim.isEmpty) && machinePK.isDefined) {
      val attrList = {
        val all = alList.filter(properModality).flatMap(al => DicomUtil.findAll(al, Set(TagByName.StationName, TagByName.RadiationMachineName))).toList
        val allReduced = all.groupBy(_.getSingleStringValueOrEmptyString()).map(_._2.head).map(deAnon)
        allReduced.groupBy(_.getSingleStringValueOrEmptyString()).map(_._2.head)
      }

      val machineNameList = attrList.map(_.getSingleStringValueOrEmptyString).filterNot(_.isEmpty).toSeq.distinct
      if (machineNameList.size == 1) {
        val plainText = machineNameList.head
        val tpsID_real = AnonymizeUtil.encryptWithNonce(institutionPK, plainText)
        val updatedTps = this.copy(tpsID_real = Some(tpsID_real))
        val count = {
          if (write) {
            logger.info("Setting tpsId_real name for " + this.id + " / " + this.getRealId + " to " + plainText)
            updatedTps.insertOrUpdate() // should always return 1
          } else {
            logger.info("(write=false) Would have set tpsId_real name for " + this.id + " / " + this.getRealId + " to " + plainText)
            1 // fake a successful write for testing
          }
        }
        if (count == 1)
          None
        else
          Some("The insertOrUpdate method returned " + count + " instead of 1.")
      } else
        Some("Multiple machine names defined in attribute lists: " + machineNameList.mkString("  ")) // more than one machine name in attribute lists
    } else
      Some("Machine name is already defined or machinePK is not defined.")
  }
}

object Machine extends Logging {

  class MachineTable(tag: Tag) extends Table[Machine](tag, "machine") {

    def machinePK = column[Long]("machinePK", O.PrimaryKey, O.AutoInc)
    def id = column[String]("id") // TODO add O.Unique constraint
    def id_real = column[Option[String]]("id_real")
    def machineTypePK = column[Long]("machineTypePK")
    def configurationDirectory = column[Option[String]]("configurationDirectory")
    def multileafCollimatorPK = column[Long]("multileafCollimatorPK")
    def epidPK = column[Long]("epidPK")
    def institutionPK = column[Long]("institutionPK")
    def notes = column[String]("notes")
    def serialNumber = column[Option[String]]("serialNumber")
    def imagingBeam2_5_mv = column[Boolean]("imagingBeam2_5_mv")
    def onboardImager = column[Boolean]("onboardImager")
    def table6DOF = column[Boolean]("table6DOF")
    def respiratoryManagement = column[Boolean]("respiratoryManagement")
    def developerMode = column[Boolean]("developerMode")
    def tpsID_real = column[Option[String]]("tpsID_real")
    def active = column[Boolean]("active")

    def * =
      (
        machinePK.?,
        id,
        id_real,
        machineTypePK,
        configurationDirectory,
        multileafCollimatorPK,
        epidPK,
        institutionPK,
        serialNumber,
        imagingBeam2_5_mv,
        onboardImager,
        table6DOF,
        respiratoryManagement,
        developerMode,
        active,
        tpsID_real,
        notes
      ) <> (Machine.apply _ tupled, Machine.unapply)

    def machineTypeFK = foreignKey("Machine_machineTypePKConstraint", machineTypePK, MachineType.query)(_.machineTypePK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def multileafCollimatorFK =
      foreignKey("Machine_multileafCollimatorPKConstraint", multileafCollimatorPK, MultileafCollimator.query)(
        _.multileafCollimatorPK,
        onDelete = ForeignKeyAction.Restrict,
        onUpdate = ForeignKeyAction.Cascade
      )
    def epidFK = foreignKey("Machine_epidPKConstraint", epidPK, EPID.query)(_.epidPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
    def institutionFK = foreignKey("Machine_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  def fileName(id: String): String = FileUtil.replaceInvalidFileNameCharacters(id, '_')

  private lazy val machConfigBaseDir: File = {
    val dir = new File(Config.DataDir, Config.machineConfigurationDirName)
    Util.mkdirs(dir)
    dir
  }

  private def getConfigDir(configurationDirectory: String) = new File(machConfigBaseDir, configurationDirectory)

  /**
    * Build a name and use it to create a new configuration directory for the given machine.  The name
    * is made of institution + machine id + serial number
    */
  private def initConfigDir(machine: Machine, serialNumber: String): String = {
    val instName = Institution.get(machine.institutionPK).get.name.trim
    val rawName = (instName + "_" + machine.id + "_" + serialNumber).replace(' ', '_')
    val name = FileUtil.replaceInvalidFileNameCharacters(rawName, '_')
    val dir = getConfigDir(name)
    Util.mkdirs(dir)
    name
  }

  private def deleteConfigDir(machine: Machine): Unit = {
    machine.configDir match {
      case Some(dir) => Utility.deleteFileTree(dir)
      case _         =>
    }
  }

  /**
    * Set the serial number for the machine and create the corresponding configuration directory.  Also put the
    *  configuration directory name in the machine's database entry.  Return true on success.
    */
  def setSerialNumber(machPK: Long, sr: String): Boolean = {
    try {
      val machine = Machine.get(machPK).get
      val cfgDirName = initConfigDir(machine, sr)
      logger.info("Setting serial number to " + sr + " for machine " + machine.id)
      Db.run(Machine.query.filter(_.machinePK === machPK).map(m => (m.configurationDirectory, m.serialNumber)).update(Some(cfgDirName), Some(sr))) match {
        case 1 => true
        case _ => false
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unable to make machine configuration dir for machinePK " + machPK + " : " + fmtEx(t))
        false
    }
  }

  val query = TableQuery[MachineTable]

  def get(machinePK: Long): Option[Machine] = {
    val action = query.filter(m => m.machinePK === machinePK)
    val list = Db.run(action.result)
    list.headOption
  }

  /**
    * Get a list of all machines.
    */
  def list: Seq[Machine] = Db.run(query.result)

  /** Dependent types */
  case class MMI(machine: Machine, machineType: MachineType, institution: Institution, mlc: MultileafCollimator, epid: EPID)

  /**
    * Get machine related info.
    *
    * @param instPK: If defined, only get machines from this institution, otherwise get from all institutions.
    */
  def listWithDependencies(instPK: Option[Long]): Seq[MMI] = {

    val action = for {
      machine <- query
      machineType <- MachineType.query if machineType.machineTypePK === machine.machineTypePK
      institution <- Institution.query if institution.institutionPK === machine.institutionPK
      mlc <- MultileafCollimator.query if mlc.multileafCollimatorPK === machine.multileafCollimatorPK
      epid <- EPID.query if epid.epidPK === machine.epidPK
    } yield (machine, machineType, institution, mlc, epid)

    val filtered = {
      if (instPK.isDefined) action.filter(c => c._3.institutionPK === instPK.get)
      else action
    }

    Db.run(filtered.result).map(mmi => MMI(mmi._1, mmi._2, mmi._3, mmi._4, mmi._5))
  }

  def listMachinesFromInstitution(institutionPK: Long): Seq[Machine] = {
    val action = query.filter(m => m.institutionPK === institutionPK)
    val seq = Db.run(action.result)
    seq
  }

  def listMachinesWithCollimator(multileafCollimatorPK: Long): Seq[Machine] = {
    val action = query.filter(m => m.multileafCollimatorPK === multileafCollimatorPK)
    val seq = Db.run(action.result)
    seq
  }

  /**
    * Find the machines that have serial numbers that match the given serial number.
    * @param serialNumber Serial number to look for.
    * @return List of machines with that serial number.  Usually 0 or 1.
    */
  def findMachinesBySerialNumber(serialNumber: String): Seq[Machine] = {
    val sn = serialNumber.trim
    val action = query.filter(m => m.serialNumber === sn)
    val seq = Db.run(action.result)
    seq
  }

  /**
    * Delete the machine from the database.  If that is successful, then also delete its configuration directory.
    */
  def delete(machinePK: Long): Int = {
    val machine = get(machinePK)
    if (machine.isDefined) {
      val q = query.filter(_.machinePK === machinePK)
      val action = q.delete
      val count = Db.run(action)
      deleteConfigDir(machine.get)
      AnonymousTranslate.clearCache(machine.get.institutionPK)
      count
    } else 0
  }

  /**
    * Given an attribute list, determine which machines' DICOM files match according to DeviceSerialNumber.
    */
  def attributeListToMachine(attributeList: AttributeList): Option[Machine] = {
    try {
      Util.attributeListToDeviceSerialNumber(attributeList) match {
        case Some(serNo) => Machine.findMachinesBySerialNumber(serNo).headOption
        case _           => None
      }
    } catch {
      case _: Throwable =>
        None
    }
  }

  /**
    * Compare two machines to determine which should be ordered lexicographically first.
    * It first tries to use numbers in machine names, and failing that, sorts alphabetically.
    * For example, this makes 'TX2' come after 'TB1', and 'BR1' come before 'TB1'.
    *
    * This is usually used with 'sortWith'.
    *
    * @param a One machine.
    * @param b The other machine.
    * @return True if a < b.
    */
  def orderMachine(a: Machine, b: Machine): Boolean = {
    if (a.machinePK.get == b.machinePK.get)
      false
    else {
      val aMach = a.getRealId
      val bMach = b.getRealId

      def toNum(text: String): Option[Int] = {
        if (text.matches(".*[0-9].*"))
          Some(text.replaceAll("[^0-9]", "").toInt)
        else
          None
      }

      val compare =
        if (toNum(aMach).isDefined && toNum(bMach).isDefined && (toNum(aMach).get != toNum(bMach).get)) {
          toNum(aMach).get < toNum(bMach).get
        } else {
          aMach.compareTo(bMach) <= 0
        }
      compare
    }
  }

  private def setTpsIdReal(machine: Machine, writeToDatabase: Boolean): Unit = {

    def getSeries(machinePK: Long): Option[DicomSeries] = {
      val action = for {
        ds <- DicomSeries.query if (ds.machinePK === machinePK) && (ds.modality === "RTIMAGE")
      } yield ds

      val dicomSeries = Db.run(action.take(1).result)
      dicomSeries.headOption
    }

    try {
      val machName = "[ " + machine.id + " / " + machine.getRealId + " ]"
      Trace.trace("Processing machine " + machName)
      val start = System.currentTimeMillis()

      val series = getSeries(machine.machinePK.get)
      if (series.isEmpty)
        Trace.trace("No RTIMAGE data")
      else {
        val alList = series.get.attributeListList
        if (alList.isEmpty) {
          val elapsed = System.currentTimeMillis() - start
          Trace.trace("no attribute lists.  elapsed ms: " + elapsed)
        } else {
          val al = alList.head
          val machineName = DicomUtil.findAllSingle(al, TagByName.RadiationMachineName).head.getSingleStringValueOrEmptyString()
          val attr = AttributeFactory.newAttribute(TagByName.RadiationMachineName)
          attr.addValue(machineName)
          val daList = DicomAnonymous.getByAttrAndValue(machine.institutionPK, Seq(attr))
          if (daList.isEmpty) {
            Trace.trace("Could not find tps name for " + machName)
          } else {
            val da = daList.head
            val realName = da.originalValue

            0 match {
              case _ if machine.tpsID_real.isEmpty                => Trace.trace("Name for " + machName + " is empty but found " + realName)
              case _ if machine.getRealTpsId.get.equals(realName) => Trace.trace("Name for " + machName + " matches found name " + realName)
              case _                                              => Trace.trace("Name for " + machName + " DOES NOT MATCH found name " + realName)
            }

            if (machine.tpsID_real.isEmpty) {
              Trace.trace("Expecting to set tpsId_real for machine " + machine.id + " / " + machine.getRealId + " to " + realName)
              machine.setTpsIdIfNeeded(alList, write = writeToDatabase)
            }
          }
        }
      }
    } catch {
      case t: Throwable => println("Exception with machine " + machine.id + " : " + machine.getRealId + " :: " + t + "\n" + fmtEx(t))
    }
  }

  def main(args: Array[String]): Unit = {
    val writeToDatabase = args.nonEmpty && args.head.equals("writeToDatabase")
    Config.validate
    DbSetup.init
    val start = System.currentTimeMillis()
    println("\n\n\n\n--------------------------------------\n\n\n\nStarting...")
    Trace.trace("writeToDatabase: " + writeToDatabase)
    if (false) {

      val alList = {
        val dir = new File("""D:\tmp\aqa\tmp\testAutoMachId\2022-04-06T06-38-32_RTIMAGE_2_1.2.246.352.62.2.5572065452063896773.12034796785239757188\output""")
        val fileList = dir.listFiles()
        val list = fileList.map(f => DicomFile(f).attributeList.get).toSeq
        list
      }

      val machine = Machine.get(27).head
      machine.setTpsIdIfNeeded(alList, writeToDatabase)
      System.exit(0)
    }

    val list = Machine.list
    println("Number of machine: " + list.size)
    list.foreach(m => setTpsIdReal(m, writeToDatabase))
    val elapsed = System.currentTimeMillis() - start
    println("Done.  Elapsed ms: " + elapsed)
    System.exit(0)
    //println("======== machine delete: " + delete(5))
  }
}
